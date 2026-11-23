package main

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"time"

	"github.com/jaeyeom/experimental/devtools/memstore"
	"golang.org/x/sys/unix"
)

func (a *app) cmdServe(args []string) int {
	if len(args) == 0 || args[0] != "http" {
		a.errorf("usage: memstore serve http --listen 127.0.0.1:8787 --profile P [--inbox-dir <dir>]")
		return exitUsage
	}
	return a.cmdServeHTTP(args[1:])
}

func (a *app) cmdServeHTTP(args []string) int {
	fs := a.flags("serve http", "memstore serve http --listen 127.0.0.1:8787 --profile P [--inbox-dir <dir>]")
	var listenAddr, inboxDir string
	var opts commonOpts
	fs.StringVar(&listenAddr, "listen", "127.0.0.1:8787", "listen address (host:port)")
	fs.StringVar(&inboxDir, "inbox-dir", "", "optional inbox directory for /v1/inbox endpoints")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}

	svc, _, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	var inbox memstore.InboxTransport
	if inboxDir != "" {
		inbox = memstore.NewDirInboxTransport(inboxDir)
	}
	srv := memstore.NewServer(svc, memstore.ServerConfig{
		Inbox: inbox,
		Token: a.getenv("MEMSTORE_SERVER_TOKEN"),
	})

	httpSrv := &http.Server{
		Addr:              listenAddr,
		Handler:           srv,
		ReadHeaderTimeout: 10 * time.Second,
	}

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, unix.SIGTERM)
	defer stop()

	errCh := make(chan error, 1)
	go func() {
		fmt.Fprintf(a.stdout, "listening on %s (profile=%s)\n", listenAddr, opts.profile)
		if err := httpSrv.ListenAndServe(); err != nil && !errors.Is(err, http.ErrServerClosed) {
			errCh <- err
			return
		}
		errCh <- nil
	}()

	select {
	case err := <-errCh:
		if err != nil {
			a.errorf("serve http: %v", err)
			return exitError
		}
		return exitOK
	case <-ctx.Done():
		shutCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		_ = httpSrv.Shutdown(shutCtx)
		if err := <-errCh; err != nil {
			a.errorf("serve http: %v", err)
			return exitError
		}
		return exitOK
	}
}
