package web

import (
	"net/http"

	"github.com/jaeyeom/experimental/devtools/decide"
)

type putDraftReq struct {
	Draft   *decide.Draft `json:"draft"`
	BaseRev int64         `json:"baseRev"`
}

func draftKey(r *http.Request) (string, error) {
	key := r.PathValue("key")
	if !validID.MatchString(key) {
		return "", badRequest("invalid draft key %q", key)
	}
	return key, nil
}

// putDraft saves a draft. A baseRev that is not the stored revision
// saves the text as a conflict copy and answers 409 with the draft.
func (s *Server) putDraft(w http.ResponseWriter, r *http.Request) {
	key, err := draftKey(r)
	if err != nil {
		fail(w, err, 0)
		return
	}
	var body putDraftReq
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, 0)
		return
	}
	switch {
	case body.Draft == nil:
		fail(w, badRequest("draft is required"), 0)
		return
	case body.Draft.Key != "" && body.Draft.Key != key:
		fail(w, badRequest("draft key %q does not match the path key %q", body.Draft.Key, key), 0)
		return
	case body.BaseRev < 0:
		fail(w, badRequest("baseRev must not be negative"), 0)
		return
	}
	body.Draft.Key = key
	saved, conflict, err := s.Backend.PutDraft(*body.Draft, body.BaseRev)
	if err != nil {
		fail(w, err, http.StatusBadRequest)
		return
	}
	s.Watcher().Signal()
	status := http.StatusOK
	if conflict {
		status = http.StatusConflict
	}
	resp := map[string]any{"draft": saved, "draftConflict": conflict}
	if conflict {
		resp["error"] = "draft conflict: your text was saved as a conflict copy"
	}
	writeJSON(w, status, resp)
}

func (s *Server) deleteDraft(w http.ResponseWriter, r *http.Request) {
	key, err := draftKey(r)
	if err != nil {
		fail(w, err, 0)
		return
	}
	conflictID := r.URL.Query().Get("conflictId")
	if len(conflictID) > 128 {
		fail(w, badRequest("conflictId is too long"), 0)
		return
	}
	if err := s.Backend.DeleteDraft(key, conflictID); err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	s.Watcher().Signal()
	resp := map[string]any{"ok": true}
	if conflictID != "" {
		// Hand back what is left so the client has the new revision.
		_, drafts, err := s.drafts()
		if err != nil {
			fail(w, err, http.StatusInternalServerError)
			return
		}
		for _, d := range drafts {
			if d.Key == key {
				resp["draft"] = d
			}
		}
	}
	writeJSON(w, http.StatusOK, resp)
}
