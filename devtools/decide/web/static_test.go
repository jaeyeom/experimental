package web

import (
	"encoding/json"
	"net/http"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestStaticAssets(t *testing.T) {
	h := tokenHandler(t)
	for _, tc := range []struct{ path, ctype string }{
		{"/", "text/html"},
		{"/manifest.webmanifest", "application/manifest+json"},
		{"/sw.js", "text/javascript"},
		{"/static/app.js", "text/javascript"},
		{"/static/store.js", "text/javascript"},
		{"/static/api.js", "text/javascript"},
		{"/static/dom.js", "text/javascript"},
		{"/static/keymap.js", "text/javascript"},
		{"/static/help_sheet.js", "text/javascript"},
		{"/static/inbox_model.js", "text/javascript"},
		{"/static/inbox_view.js", "text/javascript"},
		{"/static/swipe.js", "text/javascript"},
		{"/static/snooze_sheet.js", "text/javascript"},
		{"/static/text_render.js", "text/javascript"},
		{"/static/thread_model.js", "text/javascript"},
		{"/static/draft_sync.js", "text/javascript"},
		{"/static/sheet.js", "text/javascript"},
		{"/static/thread_view.js", "text/javascript"},
		{"/static/compose_model.js", "text/javascript"},
		{"/static/compose_view.js", "text/javascript"},
		{"/static/status_model.js", "text/javascript"},
		{"/static/status_view.js", "text/javascript"},
		{"/static/contacts_model.js", "text/javascript"},
		{"/static/contacts_view.js", "text/javascript"},
		{"/static/push_model.js", "text/javascript"},
		{"/static/settings_view.js", "text/javascript"},
		{"/static/styles.css", "text/css"},
		{"/static/icons/icon.svg", "image/svg+xml"},
	} {
		// The shell is public even when the API needs a token.
		w := do(h, req{method: "GET", path: tc.path})
		if w.Code != http.StatusOK {
			t.Errorf("GET %s = %d, want 200", tc.path, w.Code)
			continue
		}
		if got := w.Header().Get("Content-Type"); !strings.HasPrefix(got, tc.ctype) {
			t.Errorf("GET %s Content-Type = %q, want %q", tc.path, got, tc.ctype)
		}
		if w.Header().Get("Content-Security-Policy") == "" || w.Header().Get("X-Content-Type-Options") != "nosniff" {
			t.Errorf("GET %s lacks security headers", tc.path)
		}
	}
}

func TestStaticNotFoundAndMethods(t *testing.T) {
	h := tokenHandler(t)
	if w := do(h, req{method: "GET", path: "/static/nope.js"}); w.Code != http.StatusNotFound {
		t.Errorf("missing asset = %d, want 404", w.Code)
	}
	if w := do(h, req{method: "GET", path: "/static/../server.go"}); w.Code == http.StatusOK {
		t.Errorf("path traversal served a file")
	}
	// Listing the directory would leak the file set; only files serve.
	if w := do(h, req{method: "GET", path: "/static/"}); w.Code == http.StatusOK && strings.Contains(w.Body.String(), "app.js") {
		t.Errorf("directory listing served")
	}
	// The API stays closed while the shell is open.
	if w := do(h, req{method: "GET", path: "/api/v1/snapshot"}); w.Code != http.StatusUnauthorized {
		t.Errorf("unauthenticated snapshot = %d, want 401", w.Code)
	}
}

func TestManifest(t *testing.T) {
	h := tokenHandler(t)
	w := do(h, req{method: "GET", path: "/manifest.webmanifest"})
	var m map[string]any
	if err := json.Unmarshal(w.Body.Bytes(), &m); err != nil {
		t.Fatalf("manifest is not JSON: %v", err)
	}
	for k, want := range map[string]string{
		"name": "decide inbox", "short_name": "decide", "start_url": "/", "scope": "/", "display": "standalone",
	} {
		if got, _ := m[k].(string); got != want {
			t.Errorf("manifest %s = %q, want %q", k, got, want)
		}
	}
	for _, k := range []string{"theme_color", "background_color"} {
		if got, _ := m[k].(string); got == "" {
			t.Errorf("manifest %s is empty", k)
		}
	}
	icons, _ := m["icons"].([]any)
	if len(icons) == 0 {
		t.Fatal("manifest has no icons")
	}
	for _, i := range icons {
		src, _ := i.(map[string]any)["src"].(string)
		if r := do(h, req{method: "GET", path: src}); r.Code != http.StatusOK {
			t.Errorf("icon %s = %d", src, r.Code)
		}
	}
}

func TestServiceWorker(t *testing.T) {
	w := do(tokenHandler(t), req{method: "GET", path: "/sw.js"})
	if got := w.Header().Get("Cache-Control"); got != "no-cache" {
		t.Errorf("sw.js Cache-Control = %q, want no-cache", got)
	}
	if got := w.Header().Get("Service-Worker-Allowed"); got != "/" {
		t.Errorf("Service-Worker-Allowed = %q, want /", got)
	}
	body := w.Body.String()
	if strings.Contains(body, "__BUILD__") {
		t.Error("build hash placeholder not replaced")
	}
	if !strings.Contains(body, "url.pathname.startsWith('/api/')") {
		t.Error("service worker does not bypass /api/")
	}
	for _, bad := range []string{"cache.put", "cache.add(", "/api/v1/events'"} {
		if strings.Contains(body, bad) {
			t.Errorf("service worker contains %q", bad)
		}
	}
	// A different build must change the worker so phones refresh.
	if buildID() == "" || !strings.Contains(body, buildID()) {
		t.Errorf("sw.js lacks build id %q", buildID())
	}
}

func TestShellHasNoInlineCode(t *testing.T) {
	body := do(tokenHandler(t), req{method: "GET", path: "/"}).Body.String()
	for _, bad := range []string{"<style", "http://", "https://", " onclick=", "javascript:"} {
		if strings.Contains(body, bad) {
			t.Errorf("index.html contains %q, which the CSP or offline use forbids", bad)
		}
	}
	for i := 0; ; {
		j := strings.Index(body[i:], "<script")
		if j < 0 {
			break
		}
		tag := body[i+j : i+j+strings.Index(body[i+j:], ">")]
		if !strings.Contains(tag, " src=") {
			t.Errorf("inline script: %s", tag)
		}
		i += j + 1
	}
}

func TestShellOpenWithoutAuth(t *testing.T) {
	h := newHandler(t, decide.Config{}, nil)
	if w := do(h, req{method: "GET", path: "/"}); w.Code != http.StatusOK {
		t.Errorf("GET / = %d", w.Code)
	}
}
