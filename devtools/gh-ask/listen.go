package ghask

import (
	"context"
	"fmt"
	"net"
)

// Listen binds addr after CheckListenAddr accepts it.
func Listen(addr string) (net.Listener, error) {
	if err := CheckListenAddr(addr); err != nil {
		return nil, err
	}
	ln, err := (&net.ListenConfig{}).Listen(context.Background(), "tcp4", addr)
	if err != nil {
		return nil, fmt.Errorf("listen: %w", err)
	}
	return ln, nil
}
