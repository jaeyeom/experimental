package ghcli

import "os"

// BinEnv names the environment variable that holds the gh executable, such
// as a rate-limiting wrapper like gh-throttle.
const BinEnv = "GH_BIN"

// Bin returns the gh executable to run: $GH_BIN when set, otherwise "gh".
func Bin() string {
	if bin := os.Getenv(BinEnv); bin != "" {
		return bin
	}
	return "gh"
}
