package memstore

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"testing"
	"time"
)

func TestHTTPServerAndInboxRoundTrip(t *testing.T) {
	ctx := context.Background()
	cfg := setupInboxTestConfig(t)

	// Seed work vault with both engineering/normal and personal/restricted items.
	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "project",
		Slug:               "alpha",
		Title:              "Project Alpha",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Summary:            "Alpha roadmap",
	}, Actor{Caller: "seed"}); err != nil {
		t.Fatalf("PutEntity(alpha): %v", err)
	}
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "project",
		Slug:               "secret-ops",
		Title:              "Secret Ops",
		DefaultCompartment: "personal",
		Sensitivity:        SensitivityRestricted,
		Summary:            "Alpha secret budget",
	}, Actor{Caller: "seed"}); err != nil {
		t.Fatalf("PutEntity(secret-ops): %v", err)
	}
	if err := adminSvc.Close(); err != nil {
		t.Fatalf("Close(admin): %v", err)
	}

	// Bind HTTP server to restricted work-bot profile.
	botSvc, err := OpenService(cfg, "work-bot")
	if err != nil {
		t.Fatalf("OpenService(work-bot): %v", err)
	}
	defer botSvc.Close()

	inboxDir := filepath.Join(t.TempDir(), "server-inbox")
	dirTr := NewDirInboxTransport(inboxDir)
	const token = "test-secret-token"
	srv := NewServer(botSvc, ServerConfig{Inbox: dirTr, Token: token})
	ts := httptest.NewServer(srv)
	defer ts.Close()

	// 1. Healthz works without auth and reports bound profile.
	resp, err := http.Get(ts.URL + "/healthz")
	if err != nil {
		t.Fatalf("GET /healthz: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("/healthz status = %d, want 200", resp.StatusCode)
	}
	var health map[string]string
	if err := json.NewDecoder(resp.Body).Decode(&health); err != nil {
		t.Fatalf("decode /healthz: %v", err)
	}
	if health["profile"] != "work-bot" {
		t.Fatalf("/healthz profile = %q, want work-bot", health["profile"])
	}

	// 2. Bearer auth rejects unauthenticated or wrong-token requests.
	unauthTr := NewHTTPInboxTransport(ts.URL, "wrong-token")
	if _, err := unauthTr.List(ctx, 0); !errors.Is(err, ErrForbidden) {
		t.Fatalf("unauthenticated List err = %v, want ErrForbidden", err)
	}

	// 3. Profile-scoped /v1/search prevents leakage of personal/restricted items.
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, ts.URL+"/v1/search?q=Alpha", nil)
	if err != nil {
		t.Fatalf("NewRequest /v1/search: %v", err)
	}
	req.Header.Set("Authorization", "Bearer "+token)
	sResp, err := ts.Client().Do(req)
	if err != nil {
		t.Fatalf("GET /v1/search: %v", err)
	}
	defer sResp.Body.Close()
	if sResp.StatusCode != http.StatusOK {
		t.Fatalf("/v1/search status = %d, want 200", sResp.StatusCode)
	}
	var hits []SearchHit
	if err := json.NewDecoder(sResp.Body).Decode(&hits); err != nil {
		t.Fatalf("decode /v1/search: %v", err)
	}
	if len(hits) != 1 || hits[0].DocID != "project:alpha" {
		t.Fatalf("/v1/search hits = %+v, want only [project:alpha]", hits)
	}

	// 4. /v1/entities/{ref} returns permitted entity and hides out-of-scope entity.
	eReq, _ := http.NewRequestWithContext(ctx, http.MethodGet, ts.URL+"/v1/entities/project:alpha", nil)
	eReq.Header.Set("Authorization", "Bearer "+token)
	eResp, err := ts.Client().Do(eReq)
	if err != nil {
		t.Fatalf("GET /v1/entities/project:alpha: %v", err)
	}
	eResp.Body.Close()
	if eResp.StatusCode != http.StatusOK {
		t.Fatalf("/v1/entities/project:alpha status = %d, want 200", eResp.StatusCode)
	}

	secretReq, _ := http.NewRequestWithContext(ctx, http.MethodGet, ts.URL+"/v1/entities/project:secret-ops", nil)
	secretReq.Header.Set("Authorization", "Bearer "+token)
	secretResp, err := ts.Client().Do(secretReq)
	if err != nil {
		t.Fatalf("GET /v1/entities/project:secret-ops: %v", err)
	}
	secretResp.Body.Close()
	if secretResp.StatusCode != http.StatusNotFound {
		t.Fatalf("/v1/entities/project:secret-ops status = %d, want 404", secretResp.StatusCode)
	}

	// 5. HTTPInboxTransport push -> pull -> ack round-trip.
	httpTr := NewHTTPInboxTransport(ts.URL, token)
	httpTr.SetHTTPClient(ts.Client())

	// Server rejects out-of-policy envelope on POST /v1/inbox.
	if err := httpTr.Push(ctx, InboxEnvelope{
		ID:          "env_leak",
		Source:      "cloud-run",
		Compartment: "personal",
		Sensitivity: SensitivityRestricted,
		Summary:     "Should be rejected",
	}); !errors.Is(err, ErrForbidden) {
		t.Fatalf("Push out-of-policy err = %v, want ErrForbidden", err)
	}

	// Valid envelope push -> pull -> ack.
	if err := httpTr.Push(ctx, InboxEnvelope{
		ID:          "env_http_1",
		Source:      "cloud-run",
		OccurredAt:  time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC),
		EntityRef:   "project:alpha",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Summary:     "Cloud Run job completed",
	}); err != nil {
		t.Fatalf("Push valid envelope: %v", err)
	}

	report, err := PullInbox(ctx, botSvc, "work", httpTr, PullOptions{Actor: Actor{Caller: "http-sync"}})
	if err != nil {
		t.Fatalf("PullInbox over HTTP: %v", err)
	}
	if report.Ingested != 1 || report.Acked != 1 || report.Refused != 0 {
		t.Fatalf("PullReport = %+v, want ingested=1 acked=1 refused=0", report)
	}

	// Inbox is now empty after ack.
	remaining, err := httpTr.List(ctx, 0)
	if err != nil {
		t.Fatalf("List after ack: %v", err)
	}
	if len(remaining) != 0 {
		t.Fatalf("remaining after ack = %d, want 0", len(remaining))
	}
}
