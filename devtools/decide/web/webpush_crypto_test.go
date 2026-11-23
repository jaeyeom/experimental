package web

import (
	"bytes"
	"context"
	"crypto/aes"
	"crypto/cipher"
	"crypto/ecdh"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/sha256"
	"encoding/binary"
	"encoding/json"
	"fmt"
	"math/big"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

func b64(t *testing.T, s string) []byte {
	t.Helper()
	b, err := decodeB64(s)
	if err != nil {
		t.Fatalf("decode %q: %v", s, err)
	}
	return b
}

// TestRFC8291AppendixA reproduces the example in RFC 8291 Appendix A.
func TestRFC8291AppendixA(t *testing.T) {
	plaintext := []byte("When I grow up, I want to be a watermelon")
	asPriv, err := ecdh.P256().NewPrivateKey(b64(t, "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"))
	if err != nil {
		t.Fatal(err)
	}
	uaPub := b64(t, "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4")
	authSecret := b64(t, "BTBZMqHH6r4Tts7J_aSIgg")
	salt := b64(t, "DGv6ra1nlYgDCS1FRnbzlw")

	got, err := encryptAES128GCM(plaintext, uaPub, authSecret, salt, asPriv)
	if err != nil {
		t.Fatal(err)
	}
	want := "DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A_yl95bQpu6cVPTpK4Mqgkf1CXztLVBSt2Ks3oZwbuwXPXLWyouBWLVWGNWQexSgSxsj_Qulcy4a-fN"
	if enc := encodeB64(got); enc != want {
		t.Fatalf("ciphertext\n got %s\nwant %s", enc, want)
	}

	// The receiver side from the RFC's user-agent private key.
	uaPriv, err := ecdh.P256().NewPrivateKey(b64(t, "q1dXpw3UpT5VOmu_cf_v6ih07Aems3njxI-JWgLcM94"))
	if err != nil {
		t.Fatal(err)
	}
	back, err := decryptAES128GCM(got, uaPriv, authSecret)
	if err != nil || !bytes.Equal(back, plaintext) {
		t.Fatalf("decrypt = %q, %v", back, err)
	}
}

func TestEncryptRoundTripRandomKeys(t *testing.T) {
	ua, err := ecdh.P256().GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	auth := make([]byte, 16)
	_, _ = rand.Read(auth)
	msg := []byte(`{"itemId":"A-870"}`)
	ct, err := EncryptPayload(msg, ua.PublicKey().Bytes(), auth)
	if err != nil {
		t.Fatal(err)
	}
	back, err := decryptAES128GCM(ct, ua, auth)
	if err != nil || !bytes.Equal(back, msg) {
		t.Fatalf("round trip = %q, %v", back, err)
	}
	if _, err := EncryptPayload(make([]byte, maxPayload+1), ua.PublicKey().Bytes(), auth); err == nil {
		t.Fatal("oversize payload accepted")
	}
	if _, err := EncryptPayload(msg, ua.PublicKey().Bytes(), auth[:8]); err == nil {
		t.Fatal("short auth secret accepted")
	}
}

// decryptAES128GCM is the receiver side of RFC 8291 for one record.
func decryptAES128GCM(msg []byte, uaPriv *ecdh.PrivateKey, authSecret []byte) ([]byte, error) {
	salt := msg[:16]
	rs := binary.BigEndian.Uint32(msg[16:20])
	idlen := int(msg[20])
	asPubBytes := msg[21 : 21+idlen]
	body := msg[21+idlen:]
	if uint64(len(body)) > uint64(rs) {
		panic("multi record")
	}
	asPub, err := ecdh.P256().NewPublicKey(asPubBytes)
	if err != nil {
		return nil, fmt.Errorf("sender key: %w", err)
	}
	secret, err := uaPriv.ECDH(asPub)
	if err != nil {
		return nil, fmt.Errorf("ecdh: %w", err)
	}
	cek, nonce, err := deriveKeys(secret, authSecret, salt, uaPriv.PublicKey().Bytes(), asPubBytes)
	if err != nil {
		return nil, err
	}
	blk, _ := aes.NewCipher(cek)
	gcm, _ := cipher.NewGCM(blk)
	plain, err := gcm.Open(nil, nonce, body, nil)
	if err != nil {
		return nil, fmt.Errorf("open: %w", err)
	}
	i := bytes.LastIndexByte(plain, 0x02)
	return plain[:i], nil
}

func TestVAPIDJWTVerifies(t *testing.T) {
	k, err := NewVAPIDKey()
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	hdr, err := k.AuthHeader("https://push.example.com/send/abc", "mailto:me@example.com", now)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasPrefix(hdr, "vapid t=") || !strings.Contains(hdr, ", k="+k.PublicKeyB64()) {
		t.Fatalf("header = %q", hdr)
	}
	jwt := strings.TrimPrefix(strings.SplitN(hdr, ", k=", 2)[0], "vapid t=")
	parts := strings.Split(jwt, ".")
	if len(parts) != 3 {
		t.Fatalf("jwt = %q", jwt)
	}
	sig := b64(t, parts[2])
	if len(sig) != 64 {
		t.Fatalf("sig len %d", len(sig))
	}
	pub, err := ecdsa.ParseUncompressedPublicKey(elliptic.P256(), b64(t, k.PublicKeyB64()))
	if err != nil {
		t.Fatal(err)
	}
	digest := sha256.Sum256([]byte(parts[0] + "." + parts[1]))
	if !ecdsa.Verify(pub, digest[:], new(big.Int).SetBytes(sig[:32]), new(big.Int).SetBytes(sig[32:])) {
		t.Fatal("signature does not verify")
	}
	var h map[string]string
	_ = json.Unmarshal(b64(t, parts[0]), &h)
	if h["alg"] != "ES256" || h["typ"] != "JWT" {
		t.Fatalf("header = %v", h)
	}
	var c struct {
		Aud, Sub string
		Exp      int64
	}
	_ = json.Unmarshal(b64(t, parts[1]), &c)
	if c.Aud != "https://push.example.com" || c.Sub != "mailto:me@example.com" || c.Exp != now.Add(12*time.Hour).Unix() {
		t.Fatalf("claims = %+v", c)
	}
}

func TestVAPIDKeyRoundTrip(t *testing.T) {
	k, _ := NewVAPIDKey()
	k2, err := ParseVAPIDKey(k.PrivateKeyB64())
	if err != nil || k2.PublicKeyB64() != k.PublicKeyB64() {
		t.Fatalf("parse = %v", err)
	}
	if _, err := ParseVAPIDKey("AAAA"); err == nil {
		t.Fatal("bad key accepted")
	}
}

func TestSendPush(t *testing.T) {
	ua, _ := ecdh.P256().GenerateKey(rand.Reader)
	auth := bytes.Repeat([]byte{7}, 16)
	var got *http.Request
	var body []byte
	status := http.StatusCreated
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		got = r
		body = make([]byte, r.ContentLength)
		_, _ = r.Body.Read(body)
		w.WriteHeader(status)
	}))
	defer srv.Close()
	sub := Subscription{Endpoint: srv.URL + "/push/x", Keys: SubscriptionKeys{P256dh: encodeB64(ua.PublicKey().Bytes()), Auth: encodeB64(auth)}}
	k, _ := NewVAPIDKey()
	opts := SendOptions{TTL: 60, Urgency: "normal", Subject: "mailto:x@y.z", Now: testNow()}
	if err := SendPush(context.Background(), srv.Client(), sub, []byte("hi"), k, opts); err != nil {
		t.Fatal(err)
	}
	if got.Header.Get("Content-Encoding") != "aes128gcm" || got.Header.Get("TTL") != "60" || got.Header.Get("Urgency") != "normal" ||
		!strings.HasPrefix(got.Header.Get("Authorization"), "vapid t=") {
		t.Fatalf("headers = %v", got.Header)
	}
	if p, err := decryptAES128GCM(body, ua, auth); err != nil || string(p) != "hi" {
		t.Fatalf("payload = %q, %v", p, err)
	}
	for _, code := range []int{http.StatusNotFound, http.StatusGone} {
		status = code
		if err := SendPush(context.Background(), srv.Client(), sub, []byte("hi"), k, opts); err != ErrSubscriptionGone { //nolint:errorlint // sentinel
			t.Fatalf("status %d: err = %v", code, err)
		}
	}
	status = http.StatusInternalServerError
	if err := SendPush(context.Background(), srv.Client(), sub, []byte("hi"), k, opts); err == nil || err == ErrSubscriptionGone { //nolint:errorlint // sentinel
		t.Fatalf("500: err = %v", err)
	}
}
