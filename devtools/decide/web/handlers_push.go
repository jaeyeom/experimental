package web

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/netip"
	"net/url"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// pushHTTPTimeout bounds one push-service request, dial to body.
const pushHTTPTimeout = 15 * time.Second

// maxEndpointLen bounds a subscription endpoint.
const maxEndpointLen = 2048

func (s *Server) pushRoutes(mux *http.ServeMux) {
	mux.HandleFunc("GET /api/v1/push/vapid-key", s.pushVAPIDKey)
	mux.HandleFunc("POST /api/v1/push/subscribe", s.pushSubscribe)
	mux.HandleFunc("DELETE /api/v1/push/subscribe", s.pushUnsubscribe)
	mux.HandleFunc("PUT /api/v1/push/preferences", s.pushPreferences)
	mux.HandleFunc("POST /api/v1/push/test", s.pushTest)
}

// pushStore opens the push state on first use, so a server with push
// disabled never writes a key.
func (s *Server) pushStore() (*PushStore, error) {
	s.pushOnce.Do(func() {
		path := s.PushPath
		if path == "" {
			if s.pushErr = func() (err error) { path, err = DefaultPushPath(); return }(); s.pushErr != nil {
				return
			}
		}
		s.push, s.pushErr = OpenPushStore(path)
	})
	return s.push, s.pushErr
}

// pushClient is the push-service HTTP client. By default it refuses to
// dial non-public addresses, follows no redirects, and times out.
func (s *Server) pushClient() *http.Client {
	if s.PushClient != nil {
		return s.PushClient
	}
	s.pushClientOnce.Do(func() {
		dialer := &net.Dialer{Timeout: 10 * time.Second}
		dial := dialer.DialContext
		if !s.AllowInsecurePush {
			dial = publicDialer(dialer)
		}
		s.pushHTTP = &http.Client{
			Timeout:       pushHTTPTimeout,
			Transport:     &http.Transport{DialContext: dial, TLSHandshakeTimeout: 10 * time.Second},
			CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse },
		}
	})
	return s.pushHTTP
}

// publicDialer resolves the host itself and dials only public addresses,
// so a hostname cannot point the push at the local network.
func publicDialer(d *net.Dialer) func(context.Context, string, string) (net.Conn, error) {
	return func(ctx context.Context, network, address string) (net.Conn, error) {
		host, port, err := net.SplitHostPort(address)
		if err != nil {
			return nil, fmt.Errorf("push endpoint address: %w", err)
		}
		ips, err := net.DefaultResolver.LookupNetIP(ctx, "ip", host)
		if err != nil {
			return nil, fmt.Errorf("resolve push endpoint: %w", err)
		}
		lastErr := errors.New("push endpoint has no address")
		for _, ip := range ips {
			if !isPublicIP(ip) {
				return nil, errors.New("push endpoint resolves to a non-public address")
			}
			conn, err := d.DialContext(ctx, network, net.JoinHostPort(ip.String(), port))
			if err == nil {
				return conn, nil
			}
			lastErr = err
		}
		return nil, fmt.Errorf("dial push endpoint: %w", lastErr)
	}
}

// isPublicIP reports whether ip is a routable unicast address.
func isPublicIP(ip netip.Addr) bool {
	ip = ip.Unmap()
	if !ip.IsValid() || ip.IsLoopback() || ip.IsPrivate() || ip.IsLinkLocalUnicast() || ip.IsLinkLocalMulticast() ||
		ip.IsMulticast() || ip.IsUnspecified() || ip.IsInterfaceLocalMulticast() {
		return false
	}
	// Carrier-grade NAT, 100.64.0.0/10.
	return !netip.MustParsePrefix("100.64.0.0/10").Contains(ip)
}

// checkEndpoint validates a subscription endpoint: https, no
// credentials, and no loopback, private, or link-local target.
func (s *Server) checkEndpoint(raw string) error {
	if raw == "" || len(raw) > maxEndpointLen {
		return badRequest("endpoint is required (at most %d bytes)", maxEndpointLen)
	}
	u, err := url.Parse(raw)
	if err != nil || u.Host == "" || u.User != nil || u.Fragment != "" {
		return badRequest("endpoint is not a valid URL")
	}
	if s.AllowInsecurePush {
		if u.Scheme != "https" && u.Scheme != "http" {
			return badRequest("endpoint must be https")
		}
		return nil
	}
	if u.Scheme != "https" {
		return badRequest("endpoint must be https")
	}
	host := strings.ToLower(u.Hostname())
	if host == "localhost" || strings.HasSuffix(host, ".localhost") {
		return badRequest("endpoint host is not allowed")
	}
	if ip, err := netip.ParseAddr(host); err == nil && !isPublicIP(ip) {
		return badRequest("endpoint host is not allowed")
	}
	return nil
}

// pushEnabled writes 503 and reports false when web push is off.
func (s *Server) pushEnabled(w http.ResponseWriter) (*PushStore, bool) {
	if !s.Config.Web.Push.Enabled {
		writeError(w, http.StatusServiceUnavailable, "web push is disabled (set web.push.enabled)")
		return nil, false
	}
	st, err := s.pushStore()
	if err != nil {
		s.report(err)
		writeError(w, http.StatusInternalServerError, "web push state is unavailable")
		return nil, false
	}
	return st, true
}

func (s *Server) report(err error) {
	if s.Report != nil {
		s.Report(err)
	}
}

func (s *Server) pushVAPIDKey(w http.ResponseWriter, _ *http.Request) {
	if !s.Config.Web.Push.Enabled {
		writeJSON(w, http.StatusOK, map[string]any{"publicKey": "", "enabled": false})
		return
	}
	st, ok := s.pushEnabled(w)
	if !ok {
		return
	}
	writeJSON(w, http.StatusOK, map[string]any{"publicKey": st.Key().PublicKeyB64(), "enabled": true})
}

// subscriptionView is what the API returns: preferences, never the
// endpoint or keys.
func subscriptionView(sub Subscription) map[string]any {
	return map[string]any{
		"id": sub.ID, "enabled": sub.Enabled, "classes": sub.Classes,
		"quietStart": sub.QuietStart, "quietEnd": sub.QuietEnd, "tz": sub.TZ,
		"createdAt": sub.CreatedAt,
	}
}

func (s *Server) pushSubscribe(w http.ResponseWriter, r *http.Request) {
	st, ok := s.pushEnabled(w)
	if !ok {
		return
	}
	var body struct {
		Endpoint string           `json:"endpoint"`
		Keys     SubscriptionKeys `json:"keys"`
	}
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, http.StatusBadRequest)
		return
	}
	if err := s.checkEndpoint(body.Endpoint); err != nil {
		fail(w, err, 0)
		return
	}
	if err := checkKeys(body.Keys); err != nil {
		fail(w, err, 0)
		return
	}
	sub, err := st.Upsert(body.Endpoint, body.Keys, s.now())
	if err != nil {
		s.report(err)
		writeError(w, http.StatusInternalServerError, "could not save the subscription")
		return
	}
	writeJSON(w, http.StatusOK, subscriptionView(sub))
}

func checkKeys(k SubscriptionKeys) error {
	pub, err := decodeB64(k.P256dh)
	if err != nil || len(pub) != 65 || pub[0] != 4 {
		return badRequest("keys.p256dh must be an uncompressed P-256 point")
	}
	if _, err := ecdhP256().NewPublicKey(pub); err != nil {
		return badRequest("keys.p256dh is not a valid P-256 point")
	}
	if auth, err := decodeB64(k.Auth); err != nil || len(auth) != 16 {
		return badRequest("keys.auth must be 16 bytes")
	}
	return nil
}

type endpointBody struct {
	Endpoint string `json:"endpoint"`
}

func (s *Server) pushUnsubscribe(w http.ResponseWriter, r *http.Request) {
	st, ok := s.pushEnabled(w)
	if !ok {
		return
	}
	var body endpointBody
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, http.StatusBadRequest)
		return
	}
	removed, err := st.Remove(body.Endpoint)
	if err != nil {
		s.report(err)
		writeError(w, http.StatusInternalServerError, "could not save the subscription")
		return
	}
	writeJSON(w, http.StatusOK, map[string]any{"ok": true, "removed": removed})
}

type prefsBody struct {
	Endpoint   string    `json:"endpoint"`
	Enabled    *bool     `json:"enabled"`
	Classes    *[]string `json:"classes"`
	QuietStart *string   `json:"quietStart"`
	QuietEnd   *string   `json:"quietEnd"`
	TZ         *string   `json:"tz"`
}

// parseClock parses "HH:MM" into minutes since midnight.
func parseClock(s string) (int, error) {
	t, err := time.Parse("15:04", s)
	if err != nil {
		return 0, fmt.Errorf("invalid time %q, want HH:MM", s)
	}
	return t.Hour()*60 + t.Minute(), nil
}

// cleanClasses canonicalizes classes, dropping note and duplicates.
func cleanClasses(in []string) ([]string, error) {
	out := []string{}
	for _, c := range in {
		name, err := decide.ParseClass(c)
		if err != nil {
			return nil, badRequest("unknown class %q", c)
		}
		if name != decide.ClassNote && !slices.Contains(out, name) {
			out = append(out, name)
		}
	}
	return out, nil
}

func checkQuiet(start, end string) error {
	if (start == "") != (end == "") {
		return badRequest("quietStart and quietEnd must be set together")
	}
	for _, c := range []string{start, end} {
		if c == "" {
			continue
		}
		if _, err := parseClock(c); err != nil {
			return badRequest("%v", err)
		}
	}
	return nil
}

// validate checks b and returns the update to apply.
func (b prefsBody) validate(cur Subscription) (func(*Subscription), error) {
	var classes []string
	if b.Classes != nil {
		var err error
		if classes, err = cleanClasses(*b.Classes); err != nil {
			return nil, err
		}
	}
	start, end := cur.QuietStart, cur.QuietEnd
	if b.QuietStart != nil {
		start = *b.QuietStart
	}
	if b.QuietEnd != nil {
		end = *b.QuietEnd
	}
	if err := checkQuiet(start, end); err != nil {
		return nil, err
	}
	if b.TZ != nil && *b.TZ != "" {
		if *b.TZ == "Local" {
			return nil, badRequest("tz must be an IANA name")
		}
		if _, err := time.LoadLocation(*b.TZ); err != nil {
			return nil, badRequest("unknown tz %q", *b.TZ)
		}
	}
	return func(sub *Subscription) {
		if b.Enabled != nil {
			sub.Enabled = *b.Enabled
		}
		if b.Classes != nil {
			sub.Classes = classes
		}
		sub.QuietStart, sub.QuietEnd = start, end
		if b.TZ != nil {
			sub.TZ = *b.TZ
		}
	}, nil
}

func (s *Server) pushPreferences(w http.ResponseWriter, r *http.Request) {
	st, ok := s.pushEnabled(w)
	if !ok {
		return
	}
	var body prefsBody
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, http.StatusBadRequest)
		return
	}
	cur, found := st.Find(body.Endpoint)
	if !found {
		writeError(w, http.StatusNotFound, "no such subscription")
		return
	}
	apply, err := body.validate(cur)
	if err != nil {
		fail(w, err, 0)
		return
	}
	sub, found, err := st.Update(body.Endpoint, apply)
	switch {
	case err != nil:
		s.report(err)
		writeError(w, http.StatusInternalServerError, "could not save the subscription")
	case !found:
		writeError(w, http.StatusNotFound, "no such subscription")
	default:
		writeJSON(w, http.StatusOK, subscriptionView(sub))
	}
}

func (s *Server) pushTest(w http.ResponseWriter, r *http.Request) {
	st, ok := s.pushEnabled(w)
	if !ok {
		return
	}
	var body endpointBody
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, http.StatusBadRequest)
		return
	}
	sub, found := st.Find(body.Endpoint)
	if !found {
		writeError(w, http.StatusNotFound, "no such subscription")
		return
	}
	payload := []byte(`{"title":"decide","body":"Test notification","tag":"decide-test","url":"/","data":{"url":"/"}}`)
	switch err := s.sendPush(r.Context(), st, sub, payload); {
	case errors.Is(err, ErrSubscriptionGone):
		writeError(w, http.StatusGone, "the push service dropped this subscription")
	case err != nil:
		s.report(err)
		writeError(w, http.StatusBadGateway, "the push service refused the notification")
	default:
		writeJSON(w, http.StatusOK, map[string]any{"ok": true})
	}
}

// sendPush delivers payload to sub, removing the subscription when the
// push service reports it gone.
func (s *Server) sendPush(ctx context.Context, st *PushStore, sub Subscription, payload []byte) error {
	err := SendPush(ctx, s.pushClient(), sub, payload, st.Key(), SendOptions{
		TTL: 3600, Urgency: "normal", Subject: s.Config.Web.Push.Subject, Now: s.now(),
	})
	if errors.Is(err, ErrSubscriptionGone) {
		if _, rerr := st.Remove(sub.Endpoint); rerr != nil {
			s.report(rerr)
		}
	}
	return err
}
