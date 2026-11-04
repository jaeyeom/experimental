package jira

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
)

// ErrMalformedLink is returned when an issuelinks entry has no type.outward
// or type.inward. The check cannot tell a blocks link from any other link.
var ErrMalformedLink = errors.New("issuelink missing type.outward or type.inward")

// Status is the inline status needed to tell Done from still-open.
type Status struct {
	CategoryKey string
}

// Link is one issuelinks entry. Empty OutwardKey or InwardKey means that
// side of the link is absent.
type Link struct {
	Outward        string
	Inward         string
	OutwardKey     string
	OutwardSummary string
	OutwardStatus  *Status
	InwardKey      string
	InwardSummary  string
	InwardStatus   *Status
}

// Issue is a Jira issue viewed for the unblock check.
type Issue struct {
	Key     string
	Summary string
	Status  *Status
	Links   []Link
}

type rawIssue struct {
	Key    string `json:"key"`
	Fields struct {
		Summary    string          `json:"summary"`
		Status     json.RawMessage `json:"status"`
		IssueLinks []rawLink       `json:"issuelinks"`
	} `json:"fields"`
}

type rawLink struct {
	Type *struct {
		Outward *string `json:"outward"`
		Inward  *string `json:"inward"`
	} `json:"type"`
	OutwardIssue *rawLinked `json:"outwardIssue"`
	InwardIssue  *rawLinked `json:"inwardIssue"`
}

type rawLinked struct {
	Key    string `json:"key"`
	Fields *struct {
		Summary string          `json:"summary"`
		Status  json.RawMessage `json:"status"`
	} `json:"fields"`
}

// ParseIssue decodes a `jira issue view KEY --raw` body.
func ParseIssue(body string) (Issue, error) {
	var raw rawIssue
	if err := json.Unmarshal([]byte(body), &raw); err != nil {
		return Issue{}, fmt.Errorf("decode issue: %w", err)
	}
	status, err := parseStatus(raw.Fields.Status)
	if err != nil {
		return Issue{}, err
	}
	links := make([]Link, 0, len(raw.Fields.IssueLinks))
	for _, item := range raw.Fields.IssueLinks {
		link, err := parseLink(item)
		if err != nil {
			return Issue{}, err
		}
		links = append(links, link)
	}
	return Issue{
		Key:     raw.Key,
		Summary: raw.Fields.Summary,
		Status:  status,
		Links:   links,
	}, nil
}

func parseLink(raw rawLink) (Link, error) {
	if raw.Type == nil || blank(raw.Type.Outward) || blank(raw.Type.Inward) {
		return Link{}, ErrMalformedLink
	}
	link := Link{Outward: *raw.Type.Outward, Inward: *raw.Type.Inward}
	if raw.OutwardIssue != nil {
		link.OutwardKey = raw.OutwardIssue.Key
		link.OutwardSummary, link.OutwardStatus = linkedFields(raw.OutwardIssue)
	}
	if raw.InwardIssue != nil {
		link.InwardKey = raw.InwardIssue.Key
		link.InwardSummary, link.InwardStatus = linkedFields(raw.InwardIssue)
	}
	return link, nil
}

func linkedFields(raw *rawLinked) (string, *Status) {
	if raw.Fields == nil {
		return "", nil
	}
	status, err := parseStatus(raw.Fields.Status)
	if err != nil {
		return raw.Fields.Summary, nil
	}
	return raw.Fields.Summary, status
}

func parseStatus(raw json.RawMessage) (*Status, error) {
	if len(bytes.TrimSpace(raw)) == 0 || bytes.Equal(bytes.TrimSpace(raw), []byte("null")) {
		return nil, nil
	}
	var payload struct {
		StatusCategory *struct {
			Key string `json:"key"`
		} `json:"statusCategory"`
	}
	if err := json.Unmarshal(raw, &payload); err != nil {
		return nil, fmt.Errorf("decode status: %w", err)
	}
	if payload.StatusCategory == nil || payload.StatusCategory.Key == "" {
		return nil, nil
	}
	return &Status{CategoryKey: payload.StatusCategory.Key}, nil
}

func blank(s *string) bool {
	return s == nil || *s == ""
}
