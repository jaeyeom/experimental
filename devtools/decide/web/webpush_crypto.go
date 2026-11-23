package web

import (
	"bytes"
	"context"
	"crypto/aes"
	"crypto/cipher"
	"crypto/ecdh"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/hkdf"
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"encoding/binary"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"
)

// Web Push per RFC 8291 (message encryption, aes128gcm of RFC 8188) and
// RFC 8292 (VAPID), on the Go standard library only.

const (
	// recordSize is the aes128gcm record size: one 4096-byte record.
	recordSize = 4096
	// maxPayload is the largest plaintext one record holds: the record
	// less the 16-byte GCM tag and the 1-byte padding delimiter.
	maxPayload = recordSize - 16 - 1
	// uncompressedLen is the length of an uncompressed P-256 point, the
	// header's key id.
	uncompressedLen = 65
	// vapidTTL is how long a VAPID JWT lives (RFC 8292 allows up to 24h).
	vapidTTL = 12 * time.Hour
	// DefaultVAPIDSubject is the JWT sub when none is configured.
	DefaultVAPIDSubject = "mailto:decide@localhost"
)

// ErrSubscriptionGone means the push service says the subscription no
// longer exists (HTTP 404 or 410).
var ErrSubscriptionGone = errors.New("push subscription is gone")

func encodeB64(b []byte) string { return base64.RawURLEncoding.EncodeToString(b) }

// decodeB64 decodes base64url, with or without padding.
func decodeB64(s string) ([]byte, error) {
	b, err := base64.RawURLEncoding.DecodeString(strings.TrimRight(s, "="))
	if err != nil {
		return nil, fmt.Errorf("invalid base64url: %w", err)
	}
	return b, nil
}

// VAPIDKey is the server's P-256 application server key.
type VAPIDKey struct {
	priv *ecdsa.PrivateKey
}

// NewVAPIDKey generates a key.
func NewVAPIDKey() (*VAPIDKey, error) {
	priv, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return nil, fmt.Errorf("generate VAPID key: %w", err)
	}
	return &VAPIDKey{priv: priv}, nil
}

// ParseVAPIDKey parses the base64url 32-byte private scalar.
func ParseVAPIDKey(s string) (*VAPIDKey, error) {
	raw, err := decodeB64(s)
	if err != nil {
		return nil, err
	}
	priv, err := ecdsa.ParseRawPrivateKey(elliptic.P256(), raw)
	if err != nil {
		return nil, fmt.Errorf("invalid VAPID private key: %w", err)
	}
	return &VAPIDKey{priv: priv}, nil
}

// PrivateKeyB64 is the base64url private scalar. Never log it.
func (k *VAPIDKey) PrivateKeyB64() string {
	b, err := k.priv.Bytes()
	if err != nil {
		return ""
	}
	return encodeB64(b)
}

// PublicKeyB64 is the base64url uncompressed public key, as the browser's
// applicationServerKey.
func (k *VAPIDKey) PublicKeyB64() string {
	b, err := k.priv.PublicKey.Bytes()
	if err != nil {
		return ""
	}
	return encodeB64(b)
}

// AuthHeader builds the RFC 8292 Authorization value for endpoint.
func (k *VAPIDKey) AuthHeader(endpoint, subject string, now time.Time) (string, error) {
	u, err := url.Parse(endpoint)
	if err != nil || u.Scheme == "" || u.Host == "" {
		return "", fmt.Errorf("invalid push endpoint")
	}
	if subject == "" {
		subject = DefaultVAPIDSubject
	}
	hdr, _ := json.Marshal(map[string]string{"typ": "JWT", "alg": "ES256"})
	claims, err := json.Marshal(map[string]any{
		"aud": u.Scheme + "://" + u.Host,
		"exp": now.Add(vapidTTL).Unix(),
		"sub": subject,
	})
	if err != nil {
		return "", fmt.Errorf("encode VAPID claims: %w", err)
	}
	signing := encodeB64(hdr) + "." + encodeB64(claims)
	digest := sha256.Sum256([]byte(signing))
	r, s, err := ecdsa.Sign(rand.Reader, k.priv, digest[:])
	if err != nil {
		return "", fmt.Errorf("sign VAPID JWT: %w", err)
	}
	sig := make([]byte, 64)
	r.FillBytes(sig[:32])
	s.FillBytes(sig[32:])
	return "vapid t=" + signing + "." + encodeB64(sig) + ", k=" + k.PublicKeyB64(), nil
}

// deriveKeys is the RFC 8291 section 3.4 key schedule: it returns the
// content encryption key and nonce.
func deriveKeys(ecdhSecret, authSecret, salt, uaPub, asPub []byte) (cek, nonce []byte, err error) {
	prkKey, err := hkdf.Extract(sha256.New, ecdhSecret, authSecret)
	if err != nil {
		return nil, nil, fmt.Errorf("hkdf extract: %w", err)
	}
	keyInfo := append([]byte("WebPush: info\x00"), uaPub...)
	keyInfo = append(keyInfo, asPub...)
	ikm, err := hkdf.Expand(sha256.New, prkKey, string(keyInfo), 32)
	if err != nil {
		return nil, nil, fmt.Errorf("hkdf expand: %w", err)
	}
	prk, err := hkdf.Extract(sha256.New, ikm, salt)
	if err != nil {
		return nil, nil, fmt.Errorf("hkdf extract: %w", err)
	}
	if cek, err = hkdf.Expand(sha256.New, prk, "Content-Encoding: aes128gcm\x00", 16); err != nil {
		return nil, nil, fmt.Errorf("hkdf expand: %w", err)
	}
	if nonce, err = hkdf.Expand(sha256.New, prk, "Content-Encoding: nonce\x00", 12); err != nil {
		return nil, nil, fmt.Errorf("hkdf expand: %w", err)
	}
	return cek, nonce, nil
}

// encryptAES128GCM encrypts plaintext into one aes128gcm record with an
// explicit sender key and salt, so the RFC's example can be reproduced.
func encryptAES128GCM(plaintext, uaPubBytes, authSecret, salt []byte, asPriv *ecdh.PrivateKey) ([]byte, error) {
	if len(plaintext) > maxPayload {
		return nil, fmt.Errorf("push payload is %d bytes; the limit is %d", len(plaintext), maxPayload)
	}
	if len(authSecret) != 16 {
		return nil, fmt.Errorf("push auth secret must be 16 bytes, got %d", len(authSecret))
	}
	uaPub, err := ecdh.P256().NewPublicKey(uaPubBytes)
	if err != nil {
		return nil, fmt.Errorf("invalid push p256dh key: %w", err)
	}
	secret, err := asPriv.ECDH(uaPub)
	if err != nil {
		return nil, fmt.Errorf("push ECDH: %w", err)
	}
	asPub := asPriv.PublicKey().Bytes()
	cek, nonce, err := deriveKeys(secret, authSecret, salt, uaPubBytes, asPub)
	if err != nil {
		return nil, err
	}
	blk, err := aes.NewCipher(cek)
	if err != nil {
		return nil, fmt.Errorf("push cipher: %w", err)
	}
	gcm, err := cipher.NewGCM(blk)
	if err != nil {
		return nil, fmt.Errorf("push cipher: %w", err)
	}
	// RFC 8188: the last (here only) record ends with the 0x02 delimiter.
	record := gcm.Seal(nil, nonce, append(append([]byte(nil), plaintext...), 0x02), nil)
	var out bytes.Buffer
	out.Write(salt)
	_ = binary.Write(&out, binary.BigEndian, uint32(recordSize))
	out.WriteByte(uncompressedLen)
	out.Write(asPub)
	out.Write(record)
	return out.Bytes(), nil
}

// EncryptPayload encrypts plaintext for a subscription's p256dh key and
// auth secret with a fresh sender key and salt.
func EncryptPayload(plaintext, uaPub, authSecret []byte) ([]byte, error) {
	asPriv, err := ecdh.P256().GenerateKey(rand.Reader)
	if err != nil {
		return nil, fmt.Errorf("generate push key: %w", err)
	}
	salt := make([]byte, 16)
	if _, err := io.ReadFull(rand.Reader, salt); err != nil {
		return nil, fmt.Errorf("generate push salt: %w", err)
	}
	return encryptAES128GCM(plaintext, uaPub, authSecret, salt, asPriv)
}

// SendOptions are the per-request push parameters.
type SendOptions struct {
	// TTL is how long the push service may hold the message, in seconds.
	TTL int
	// Urgency is very-low, low, normal, or high. Empty omits it.
	Urgency string
	// Subject is the VAPID sub claim. Empty is DefaultVAPIDSubject.
	Subject string
	// Now is the JWT clock.
	Now time.Time
}

// SendPush encrypts payload for sub and posts it to the push service. It
// returns ErrSubscriptionGone on HTTP 404 or 410. The endpoint and keys
// are never put in an error.
func SendPush(ctx context.Context, client *http.Client, sub Subscription, payload []byte, key *VAPIDKey, opts SendOptions) error {
	uaPub, err := decodeB64(sub.Keys.P256dh)
	if err != nil {
		return fmt.Errorf("push p256dh key: %w", err)
	}
	auth, err := decodeB64(sub.Keys.Auth)
	if err != nil {
		return fmt.Errorf("push auth secret: %w", err)
	}
	body, err := EncryptPayload(payload, uaPub, auth)
	if err != nil {
		return err
	}
	authz, err := key.AuthHeader(sub.Endpoint, opts.Subject, opts.Now)
	if err != nil {
		return err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, sub.Endpoint, bytes.NewReader(body))
	if err != nil {
		return fmt.Errorf("build push request: %w", err)
	}
	req.Header.Set("Content-Encoding", "aes128gcm")
	req.Header.Set("Content-Type", "application/octet-stream")
	req.Header.Set("TTL", strconv.Itoa(opts.TTL))
	if opts.Urgency != "" {
		req.Header.Set("Urgency", opts.Urgency)
	}
	req.Header.Set("Authorization", authz)
	resp, err := client.Do(req)
	if err != nil {
		// The *url.Error text carries the endpoint; drop it.
		var ue *url.Error
		if errors.As(err, &ue) {
			err = ue.Err
		}
		return fmt.Errorf("push request failed: %w", err)
	}
	defer resp.Body.Close()
	_, _ = io.Copy(io.Discard, io.LimitReader(resp.Body, 4096))
	switch {
	case resp.StatusCode == http.StatusNotFound || resp.StatusCode == http.StatusGone:
		return ErrSubscriptionGone
	case resp.StatusCode < 200 || resp.StatusCode > 299:
		return fmt.Errorf("push service answered HTTP %d", resp.StatusCode)
	}
	return nil
}
