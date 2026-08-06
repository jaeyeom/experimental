//go:build !darwin

package credentials

import (
	"context"
	"fmt"
)

func defaultKeychainLoader(ctx context.Context, service, account string) (string, error) {
	_ = ctx
	_ = service
	_ = account
	return "", fmt.Errorf("%w", ErrUnsupportedPlatform)
}
