package slackwatch

import (
	"context"
	"strings"
	"testing"
	"time"
)

type userListFunc func(context.Context, string) (UserPage, error)

func (f userListFunc) ListUsers(ctx context.Context, cursor string) (UserPage, error) {
	return f(ctx, cursor)
}

func TestUserIDCacheHitSkipsList(t *testing.T) {
	s := openTestStore(t)
	now := testNow()
	if err := s.PutUser("ada", "UADA", now); err != nil {
		t.Fatal(err)
	}
	var calls int
	id, err := (UserLookup{
		Store: s, Now: func() time.Time { return now }, TTL: DefaultChannelCacheTTL,
		List: userListFunc(func(context.Context, string) (UserPage, error) {
			calls++
			return UserPage{}, context.Canceled
		}),
	}).UserID(context.Background(), "Ada")
	if err != nil || id != "UADA" || calls != 0 {
		t.Fatalf("UserID = %q, %v, calls %d", id, err, calls)
	}
}

func TestUserIDAmbiguousListsCandidates(t *testing.T) {
	s := openTestStore(t)
	id, err := (UserLookup{
		Store: s, Now: testNow, TTL: DefaultChannelCacheTTL,
		List: userListFunc(func(context.Context, string) (UserPage, error) {
			return UserPage{Users: []NamedUser{
				{ID: "U2", Name: "sam2", Display: "Sam"},
				{ID: "U1", Name: "sam", Display: "Sam"},
			}}, nil
		}),
	}).UserID(context.Background(), "Sam")
	if err == nil || id != "" {
		t.Fatalf("UserID = %q, %v", id, err)
	}
	if !strings.Contains(err.Error(), "U1") || !strings.Contains(err.Error(), "U2") {
		t.Errorf("error = %v, want both ids", err)
	}
	if _, _, ok, err := s.UserByName("sam"); err != nil || ok {
		t.Errorf("ambiguous name cached = %v, %v", ok, err)
	}
}
