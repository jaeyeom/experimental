package adapter

import "context"

type forwardedCallerKey struct{}

// WithForwardedCaller marks ctx as a request that came in on the hub
// socket from handle. The daemon has already checked handle, so
// Identify returns it instead of reading the caller's environment.
func WithForwardedCaller(ctx context.Context, handle string) context.Context {
	return context.WithValue(ctx, forwardedCallerKey{}, handle)
}

// ForwardedCaller returns the handle WithForwardedCaller stored.
func ForwardedCaller(ctx context.Context) (string, bool) {
	handle, ok := ctx.Value(forwardedCallerKey{}).(string)
	return handle, ok && handle != ""
}
