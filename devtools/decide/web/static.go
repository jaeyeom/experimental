package web

import (
	"bytes"
	"crypto/sha256"
	"embed"
	"encoding/hex"
	"fmt"
	"io/fs"
	"net/http"
	"strings"
	"time"
)

// The shell is public: it holds no data, and the API behind it answers
// 401 until the page logs in. Test files stay out of the binary.
//
//go:embed static/index.html static/*.css static/*.js static/*.webmanifest static/icons
var staticFS embed.FS

// buildID hashes the embedded assets. sw.js carries it, so a new
// decide build changes the worker and phones refresh their cache.

func staticSub() fs.FS {
	sub, err := fs.Sub(staticFS, "static")
	if err != nil {
		panic(err) // "static" is embedded above
	}
	return sub
}

func buildID() string {
	h := sha256.New()
	sub := staticSub()
	_ = fs.WalkDir(sub, ".", func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		b, err := fs.ReadFile(sub, p)
		if err != nil {
			return fmt.Errorf("read %s: %w", p, err)
		}
		h.Write([]byte(p))
		h.Write(b)
		return nil
	})
	return hex.EncodeToString(h.Sum(nil))[:16]
}

func staticRoutes(mux *http.ServeMux) {
	sub := staticSub()
	file := func(name string) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Cache-Control", "no-cache")
			http.ServeFileFS(w, r, sub, name)
		}
	}
	mux.HandleFunc("GET /{$}", file("index.html"))
	mux.HandleFunc("GET /manifest.webmanifest", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/manifest+json")
		file("manifest.webmanifest")(w, r)
	})
	mux.HandleFunc("GET /sw.js", serviceWorker(sub))
	files := http.FileServerFS(sub)
	mux.Handle("GET /static/", http.StripPrefix("/static/", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// No directory listings, no index.html under /static/.
		if r.URL.Path == "" || strings.HasSuffix(r.URL.Path, "/") || strings.HasSuffix(r.URL.Path, "index.html") {
			http.NotFound(w, r)
			return
		}
		w.Header().Set("Cache-Control", "no-cache")
		files.ServeHTTP(w, r)
	})))
}

// serviceWorker serves sw.js from the root scope with the build id
// filled in. no-cache makes Chrome check for a new worker on every load.
func serviceWorker(sub fs.FS) http.HandlerFunc {
	src, err := fs.ReadFile(sub, "sw.js")
	if err != nil {
		panic(err) // embedded above
	}
	body := bytes.ReplaceAll(src, []byte("__BUILD__"), []byte(buildID()))
	return func(w http.ResponseWriter, r *http.Request) {
		h := w.Header()
		h.Set("Content-Type", "text/javascript; charset=utf-8")
		h.Set("Cache-Control", "no-cache")
		h.Set("Service-Worker-Allowed", "/")
		http.ServeContent(w, r, "sw.js", time.Time{}, bytes.NewReader(body))
	}
}
