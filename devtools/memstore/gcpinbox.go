package memstore

import (
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/google/renameio/v2"
)

// DefaultInboxSource is the default source/channel recorded when an
// InboxEnvelope omits Source or Channel.
const DefaultInboxSource = "gcp-inbox"

// Pull item statuses recorded in PullReport.
const (
	PullStatusIngested  = "ingested"
	PullStatusDuplicate = "duplicate"
	PullStatusRefused   = "refused"
	PullStatusDryRun    = "dry-run"
)

// InboxEnvelope is the sanitized payload exchanged between a remote Cloud Run
// collector and a local memstore vault during Pattern B scoped inbox sync.
type InboxEnvelope struct {
	ID          string        `json:"id"`
	Source      string        `json:"source"`
	OccurredAt  time.Time     `json:"occurredAt,omitempty"`
	EventDate   string        `json:"eventDate,omitempty"`
	EntityRef   string        `json:"entityRef,omitempty"`
	Compartment string        `json:"compartment,omitempty"`
	Sensitivity Sensitivity   `json:"sensitivity,omitempty"`
	Channel     string        `json:"channel,omitempty"`
	SourceRef   string        `json:"sourceRef,omitempty"`
	Summary     string        `json:"summary,omitempty"`
	DetailsMD   string        `json:"detailsMd,omitempty"`
	Facts       []FactInput   `json:"facts,omitempty"`
	Proposals   []TriageInput `json:"proposals,omitempty"`
}

// InboxTransport abstracts the remote or local staging store for
// InboxEnvelope items.
type InboxTransport interface {
	Push(ctx context.Context, env InboxEnvelope) error
	List(ctx context.Context, limit int) ([]InboxEnvelope, error)
	Ack(ctx context.Context, id string) error
}

// DirInboxTransport is a directory-backed InboxTransport that stores each
// envelope as an atomic <id>.json file.
type DirInboxTransport struct {
	dir string
}

// NewDirInboxTransport returns a DirInboxTransport rooted at dir.
func NewDirInboxTransport(dir string) *DirInboxTransport {
	return &DirInboxTransport{dir: dir}
}

// Push writes env atomically as <id>.json inside the transport directory.
func (t *DirInboxTransport) Push(ctx context.Context, env InboxEnvelope) error {
	if err := ctx.Err(); err != nil {
		return fmt.Errorf("push inbox envelope: %w", err)
	}
	if env.ID == "" {
		id, err := newID("env")
		if err != nil {
			return err
		}
		env.ID = id
	}
	if err := validateEnvelopeID(env.ID); err != nil {
		return err
	}
	if err := EnsureSecureDir(t.dir); err != nil {
		return err
	}
	data, err := json.MarshalIndent(env, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal envelope %s: %w", env.ID, err)
	}
	data = append(data, '\n')
	path := filepath.Join(t.dir, env.ID+".json")
	if err := renameio.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write envelope %s: %w", env.ID, err)
	}
	return nil
}

// List reads pending *.json envelopes from the transport directory in
// deterministic filename order, up to limit if limit > 0.
func (t *DirInboxTransport) List(ctx context.Context, limit int) ([]InboxEnvelope, error) {
	if err := ctx.Err(); err != nil {
		return nil, fmt.Errorf("list inbox envelopes: %w", err)
	}
	entries, err := os.ReadDir(t.dir)
	if errors.Is(err, fs.ErrNotExist) {
		return []InboxEnvelope{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read inbox dir %s: %w", t.dir, err)
	}

	out := []InboxEnvelope{}
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".json") || strings.HasPrefix(entry.Name(), ".") {
			continue
		}
		data, err := os.ReadFile(filepath.Join(t.dir, entry.Name()))
		if err != nil {
			return nil, fmt.Errorf("read envelope %s: %w", entry.Name(), err)
		}
		var env InboxEnvelope
		if err := json.Unmarshal(data, &env); err != nil {
			return nil, fmt.Errorf("parse envelope %s: %w", entry.Name(), err)
		}
		out = append(out, env)
		if limit > 0 && len(out) >= limit {
			break
		}
	}
	return out, nil
}

// Ack deletes the envelope file for id from the transport directory. Missing
// files are treated as already acknowledged.
func (t *DirInboxTransport) Ack(ctx context.Context, id string) error {
	if err := ctx.Err(); err != nil {
		return fmt.Errorf("ack inbox envelope: %w", err)
	}
	if err := validateEnvelopeID(id); err != nil {
		return err
	}
	path := filepath.Join(t.dir, id+".json")
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("remove envelope %s: %w", id, err)
	}
	return nil
}

func validateEnvelopeID(id string) error {
	if id == "" {
		return fmt.Errorf("envelope id is required")
	}
	if strings.ContainsAny(id, `/\`) || strings.Contains(id, "..") {
		return fmt.Errorf("invalid envelope id %q", id)
	}
	return nil
}

// HTTPInboxTransport talks to a remote memstore serve http inbox endpoint
// using optional bearer-token authentication (typically MEMSTORE_INBOX_TOKEN).
type HTTPInboxTransport struct {
	baseURL string
	token   string
	client  *http.Client
}

// NewHTTPInboxTransport returns an HTTPInboxTransport targeting baseURL with
// bearer token auth when token is non-empty.
func NewHTTPInboxTransport(baseURL, token string) *HTTPInboxTransport {
	return &HTTPInboxTransport{
		baseURL: strings.TrimRight(baseURL, "/"),
		token:   token,
		client:  &http.Client{Timeout: 30 * time.Second},
	}
}

// SetHTTPClient overrides the underlying *http.Client (primarily for tests).
func (t *HTTPInboxTransport) SetHTTPClient(c *http.Client) {
	if c != nil {
		t.client = c
	}
}

// Push sends env to POST /v1/inbox.
func (t *HTTPInboxTransport) Push(ctx context.Context, env InboxEnvelope) error {
	body, err := json.Marshal(env)
	if err != nil {
		return fmt.Errorf("marshal envelope: %w", err)
	}
	return t.doJSON(ctx, http.MethodPost, "/v1/inbox", bytes.NewReader(body), nil)
}

// List fetches pending envelopes from GET /v1/inbox.
func (t *HTTPInboxTransport) List(ctx context.Context, limit int) ([]InboxEnvelope, error) {
	path := "/v1/inbox"
	if limit > 0 {
		path += "?limit=" + strconv.Itoa(limit)
	}
	var out []InboxEnvelope
	if err := t.doJSON(ctx, http.MethodGet, path, nil, &out); err != nil {
		return nil, err
	}
	if out == nil {
		out = []InboxEnvelope{}
	}
	return out, nil
}

// Ack acknowledges envelope id via DELETE /v1/inbox/{id}.
func (t *HTTPInboxTransport) Ack(ctx context.Context, id string) error {
	if err := validateEnvelopeID(id); err != nil {
		return err
	}
	return t.doJSON(ctx, http.MethodDelete, "/v1/inbox/"+url.PathEscape(id), nil, nil)
}

func (t *HTTPInboxTransport) doJSON(ctx context.Context, method, path string, body io.Reader, out any) error {
	req, err := http.NewRequestWithContext(ctx, method, t.baseURL+path, body) //nolint:gosec // baseURL is an operator-configured memstore server URL
	if err != nil {
		return fmt.Errorf("build request %s %s: %w", method, path, err)
	}
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	if t.token != "" {
		req.Header.Set("Authorization", "Bearer "+t.token)
	}
	resp, err := t.client.Do(req) //nolint:gosec // baseURL is an operator-configured memstore server URL
	if err != nil {
		return fmt.Errorf("%s %s: %w", method, path, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return readHTTPError(method+" "+path, resp)
	}
	if out != nil {
		if err := json.NewDecoder(resp.Body).Decode(out); err != nil {
			return fmt.Errorf("decode %s %s response: %w", method, path, err)
		}
	}
	return nil
}

func readHTTPError(op string, resp *http.Response) error {
	msg, _ := io.ReadAll(io.LimitReader(resp.Body, 4096))
	trimmed := strings.TrimSpace(string(msg))
	if resp.StatusCode == http.StatusUnauthorized || resp.StatusCode == http.StatusForbidden {
		return fmt.Errorf("%w: %s returned %d: %s", ErrForbidden, op, resp.StatusCode, trimmed)
	}
	return fmt.Errorf("%s returned %d: %s", op, resp.StatusCode, trimmed)
}

// PullOptions configures PullInbox.
type PullOptions struct {
	Limit  int
	DryRun bool
	Actor  Actor
}

// PullItemStatus describes the outcome for a single envelope in PullInbox.
type PullItemStatus struct {
	EnvelopeID string   `json:"envelopeId"`
	Status     string   `json:"status"`
	Reason     string   `json:"reason,omitempty"`
	EventID    string   `json:"eventId,omitempty"`
	TriageIDs  []string `json:"triageIds,omitempty"`
}

// PullReport summarizes a PullInbox run.
type PullReport struct {
	Vault    string           `json:"vault"`
	Profile  string           `json:"profile"`
	DryRun   bool             `json:"dryRun,omitempty"`
	Listed   int              `json:"listed"`
	Ingested int              `json:"ingested"`
	Skipped  int              `json:"skipped"`
	Refused  int              `json:"refused"`
	Acked    int              `json:"acked"`
	Items    []PullItemStatus `json:"items,omitempty"`
}

// PullInbox lists pending envelopes from transport, validates each against svc's
// profile policy, ingests valid envelopes idempotently into episodic_events and
// triage_inbox, and calls transport.Ack only after the local SQLite commit
// succeeds. Envelopes that violate policy are refused without acking.
func PullInbox(ctx context.Context, svc *Service, vaultName string, transport InboxTransport, opts PullOptions) (PullReport, error) {
	if svc == nil {
		return PullReport{}, fmt.Errorf("service is required")
	}
	if vaultName == "" {
		return PullReport{}, fmt.Errorf("vault name is required")
	}
	if transport == nil {
		return PullReport{}, fmt.Errorf("inbox transport is required")
	}

	envelopes, err := transport.List(ctx, opts.Limit)
	if err != nil {
		return PullReport{}, fmt.Errorf("list inbox: %w", err)
	}

	report := PullReport{
		Vault:   vaultName,
		Profile: svc.policy.name,
		DryRun:  opts.DryRun,
		Listed:  len(envelopes),
	}

	for _, env := range envelopes {
		item, v, err := pullOneEnvelope(ctx, svc, vaultName, env, opts)
		if err != nil {
			return report, err
		}
		report.Items = append(report.Items, item)
		switch item.Status {
		case PullStatusRefused:
			report.Refused++
		case PullStatusDuplicate:
			report.Skipped++
			if !opts.DryRun {
				if err := transport.Ack(ctx, env.ID); err != nil {
					return report, fmt.Errorf("ack duplicate envelope %s: %w", env.ID, err)
				}
				report.Acked++
			}
		case PullStatusDryRun:
			report.Ingested++
		case PullStatusIngested:
			report.Ingested++
			if err := svc.logAccess(ctx, v, opts.Actor, "sync-pull", ""); err != nil {
				return report, err
			}
			if err := transport.Ack(ctx, env.ID); err != nil {
				return report, fmt.Errorf("ack ingested envelope %s: %w", env.ID, err)
			}
			report.Acked++
		}
	}
	return report, nil
}

func pullOneEnvelope(ctx context.Context, svc *Service, vaultName string, env InboxEnvelope, opts PullOptions) (PullItemStatus, *Vault, error) {
	if env.ID == "" {
		return PullItemStatus{Status: PullStatusRefused, Reason: "missing envelope id"}, nil, nil
	}

	v, refusal := authorizeEnvelope(svc, vaultName, env)
	if refusal != "" {
		return PullItemStatus{EnvelopeID: env.ID, Status: PullStatusRefused, Reason: refusal}, nil, nil
	}

	seen, err := hasInboxReceipt(ctx, v.db, env.ID)
	if err != nil {
		return PullItemStatus{}, nil, err
	}
	if seen {
		return PullItemStatus{EnvelopeID: env.ID, Status: PullStatusDuplicate}, v, nil
	}
	if opts.DryRun {
		return PullItemStatus{EnvelopeID: env.ID, Status: PullStatusDryRun}, v, nil
	}

	eventID, triageIDs, duplicate, err := ingestEnvelopeTx(ctx, v, env, opts.Actor)
	if err != nil {
		return PullItemStatus{}, nil, err
	}
	if duplicate {
		return PullItemStatus{EnvelopeID: env.ID, Status: PullStatusDuplicate}, v, nil
	}
	return PullItemStatus{
		EnvelopeID: env.ID,
		Status:     PullStatusIngested,
		EventID:    eventID,
		TriageIDs:  triageIDs,
	}, v, nil
}

func authorizeEnvelope(svc *Service, vaultName string, env InboxEnvelope) (*Vault, string) {
	compartment := defaultCompartment(env.Compartment)
	sens := defaultSensitivity(env.Sensitivity)

	var v *Vault
	if env.Summary != "" || (len(env.Facts) == 0 && len(env.Proposals) == 0) {
		var err error
		v, err = svc.authorizeEpisodicWrite(vaultName, compartment, sens)
		if err != nil {
			return nil, err.Error()
		}
	}

	for _, f := range env.Facts {
		fComp := defaultCompartment(f.Compartment)
		if f.Compartment == "" {
			fComp = compartment
		}
		fSens := defaultSensitivity(f.Sensitivity)
		if f.Sensitivity == "" {
			fSens = sens
		}
		vault, err := svc.authorizeTriageWrite(vaultName, fComp, fSens)
		if err != nil {
			return nil, err.Error()
		}
		if v == nil {
			v = vault
		}
	}

	for _, p := range env.Proposals {
		pComp := defaultCompartment(p.ProposedCompartment)
		if p.ProposedCompartment == "" {
			pComp = compartment
		}
		pSens := defaultSensitivity(p.ProposedSensitivity)
		if p.ProposedSensitivity == "" {
			pSens = sens
		}
		vault, err := svc.authorizeTriageWrite(vaultName, pComp, pSens)
		if err != nil {
			return nil, err.Error()
		}
		if v == nil {
			v = vault
		}
	}
	return v, ""
}

func hasInboxReceipt(ctx context.Context, db *sql.DB, envelopeID string) (bool, error) {
	var exists int
	err := db.QueryRowContext(ctx, `SELECT 1 FROM inbox_receipts WHERE envelope_id = ?`, envelopeID).Scan(&exists)
	if errors.Is(err, sql.ErrNoRows) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("check inbox receipt %s: %w", envelopeID, err)
	}
	return true, nil
}

func ingestEnvelopeTx(ctx context.Context, v *Vault, env InboxEnvelope, actor Actor) (string, []string, bool, error) {
	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return "", nil, false, fmt.Errorf("begin inbox ingest tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	var exists int
	err = tx.QueryRowContext(ctx, `SELECT 1 FROM inbox_receipts WHERE envelope_id = ?`, env.ID).Scan(&exists)
	if err == nil {
		return "", nil, true, nil
	}
	if !errors.Is(err, sql.ErrNoRows) {
		return "", nil, false, fmt.Errorf("check inbox receipt in tx %s: %w", env.ID, err)
	}

	now := v.now().UTC()
	nowUnix := now.Unix()
	compartment := defaultCompartment(env.Compartment)
	sens := defaultSensitivity(env.Sensitivity)
	source := env.Source
	if source == "" {
		source = DefaultInboxSource
	}

	var eventID string
	if env.Summary != "" {
		id, err := ingestEnvelopeEventTx(ctx, tx, env, compartment, sens, source, now, actor)
		if err != nil {
			return "", nil, false, err
		}
		eventID = id
	}

	triageIDs, err := ingestEnvelopeTriageTx(ctx, tx, env, compartment, sens, source, nowUnix, actor)
	if err != nil {
		return "", nil, false, err
	}

	if _, err := tx.ExecContext(ctx, `INSERT INTO inbox_receipts (envelope_id, ingested_at) VALUES (?, ?)`, env.ID, nowUnix); err != nil {
		return "", nil, false, fmt.Errorf("record inbox receipt %s: %w", env.ID, err)
	}

	if err := tx.Commit(); err != nil {
		return "", nil, false, fmt.Errorf("commit inbox ingest %s: %w", env.ID, err)
	}
	return eventID, triageIDs, false, nil
}

func ingestEnvelopeEventTx(ctx context.Context, tx *sql.Tx, env InboxEnvelope, compartment string, sens Sensitivity, source string, now time.Time, actor Actor) (string, error) {
	id, err := newID("evt")
	if err != nil {
		return "", err
	}
	occurredAt := env.OccurredAt.UTC()
	if occurredAt.IsZero() && env.EventDate != "" {
		if parsed, parseErr := time.Parse("2006-01-02", env.EventDate); parseErr == nil {
			occurredAt = parsed.UTC()
		}
	}
	if occurredAt.IsZero() {
		occurredAt = now
	}
	eventDate := env.EventDate
	if eventDate == "" {
		eventDate = occurredAt.Format("2006-01-02")
	}

	channel := env.Channel
	if channel == "" {
		channel = source
	}
	sourceRef := env.SourceRef
	if sourceRef == "" {
		sourceRef = env.ID
	}

	entityID, err := resolveLocalEntityIDTx(ctx, tx, env.EntityRef)
	if err != nil {
		return "", err
	}

	in := EventInput{
		OccurredAt:  occurredAt,
		EntityID:    entityID,
		Compartment: compartment,
		Sensitivity: sens,
		Channel:     channel,
		SourceRef:   sourceRef,
		Summary:     env.Summary,
		DetailsMD:   env.DetailsMD,
		Reporter:    source,
	}
	if err := insertEventRowTx(ctx, tx, id, eventDate, occurredAt.Unix(), in, compartment, sens, now.Unix(), actor); err != nil {
		return "", err
	}
	return id, nil
}

func resolveLocalEntityIDTx(ctx context.Context, tx *sql.Tx, ref string) (string, error) {
	if ref == "" {
		return "", nil
	}
	var id string
	err := tx.QueryRowContext(ctx, `SELECT id FROM entities WHERE id = ?`, ref).Scan(&id)
	if err == nil {
		return id, nil
	}
	if !errors.Is(err, sql.ErrNoRows) {
		return "", fmt.Errorf("lookup entity %q: %w", ref, err)
	}
	return "", nil
}

func ingestEnvelopeTriageTx(ctx context.Context, tx *sql.Tx, env InboxEnvelope, compartment string, sens Sensitivity, source string, nowUnix int64, actor Actor) ([]string, error) {
	var ids []string
	if len(env.Facts) > 0 {
		payloadBytes, err := json.Marshal(TriagePayload{Facts: env.Facts})
		if err != nil {
			return nil, fmt.Errorf("marshal envelope facts %s: %w", env.ID, err)
		}
		triID, err := newID("tri")
		if err != nil {
			return nil, err
		}
		in := TriageInput{
			Source:              source,
			ProposedCompartment: compartment,
			ProposedSensitivity: sens,
			TargetEntityID:      env.EntityRef,
			PayloadJSON:         string(payloadBytes),
			DecideRef:           "inbox:" + env.ID,
		}
		if err := insertTriageRowTx(ctx, tx, triID, in, compartment, sens, nowUnix, actor); err != nil {
			return nil, err
		}
		ids = append(ids, triID)
	}

	for _, p := range env.Proposals {
		if p.Source == "" {
			p.Source = source
		}
		pComp := defaultCompartment(p.ProposedCompartment)
		if p.ProposedCompartment == "" {
			pComp = compartment
		}
		pSens := defaultSensitivity(p.ProposedSensitivity)
		if p.ProposedSensitivity == "" {
			pSens = sens
		}
		if p.TargetEntityID == "" {
			p.TargetEntityID = env.EntityRef
		}
		if p.DecideRef == "" {
			p.DecideRef = "inbox:" + env.ID
		}
		triID, err := newID("tri")
		if err != nil {
			return nil, err
		}
		if err := insertTriageRowTx(ctx, tx, triID, p, pComp, pSens, nowUnix, actor); err != nil {
			return nil, err
		}
		ids = append(ids, triID)
	}
	return ids, nil
}
