package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/argparser"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

const (
	version = "1.0.0"
)

func main() {
	if len(os.Args) < 2 {
		showUsage()
		os.Exit(1)
	}

	command := os.Args[1]
	args := os.Args[2:]

	executeCommand(command, args)
}

func executeCommand(command string, args []string) {
	handler := getCommandHandler(command)
	if handler != nil {
		handler(args)
		return
	}

	if isHelpCommand(command) {
		showUsage()
		return
	}

	if command == "version" {
		fmt.Printf("gh-storage version %s\n", version)
		return
	}

	fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
	showUsage()
	os.Exit(1)
}

func getCommandHandler(command string) func([]string) {
	handlers := map[string]func([]string){
		"init":    handleInit,
		"info":    handleInfo,
		"ls":      handleList,
		"get":     handleGet,
		"set":     handleSet,
		"delete":  handleDelete,
		"backup":  handleBackup,
		"restore": handleRestore,
		"clean":   handleClean,
		"vacuum":  handleVacuum,
		"lock":    handleLock,
		"export":  handleExport,
		"import":  handleImport,
		"verify":  handleVerify,
	}
	return handlers[command]
}

func isHelpCommand(command string) bool {
	return command == "help" || command == "-h" || command == "--help"
}

func showUsage() {
	fmt.Printf(`gh-storage - Unified Storage Management CLI

Usage: gh-storage <command> [options]

Commands:
  init                    Initialize storage system
  info [path]             Show storage information
  ls [path]               List storage contents
  get <key>               Get value from storage
  set <key> <value>       Set value in storage
  delete <key>            Delete value from storage
  backup [path]           Create backup of storage data
  restore <backup-id>     Restore from backup
  clean                   Clean up old/temporary data
  vacuum                  Optimize storage (defragment, compress)
  lock                    Manage file locks
  export <path>           Export storage data
  import <path>           Import storage data
  verify                  Verify storage integrity
  version                 Show version information
  help                    Show this help message

Environment Variables:
  GH_STORAGE_HOME         Storage directory [default: ~/.config/gh-nudge/storage]
  GH_STORAGE_BACKUP_DIR   Backup directory [default: ~/.config/gh-nudge/backups]
  GH_STORAGE_LOCK_TIMEOUT Lock timeout in seconds [default: 30]
  GH_STORAGE_DEBUG        Enable debug logging [default: false]

Examples:
  gh-storage init
  gh-storage info
  gh-storage ls repos/owner/repo/
  gh-storage get repos/owner/repo/pull/123/metadata
  gh-storage set cache/github/users/user1 --file user1.json
  gh-storage backup --all
  gh-storage clean --older-than 30d
`)
}

func getStorageHome() string {
	if home := os.Getenv("GH_STORAGE_HOME"); home != "" {
		return home
	}

	userHome, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting home directory: %v\n", err)
		os.Exit(1)
	}

	return filepath.Join(userHome, ".config", "gh-nudge", "storage")
}

func getBackupDir() string {
	if backupDir := os.Getenv("GH_STORAGE_BACKUP_DIR"); backupDir != "" {
		return backupDir
	}

	userHome, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting home directory: %v\n", err)
		os.Exit(1)
	}

	return filepath.Join(userHome, ".config", "gh-nudge", "backups")
}

func handleInit(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-storage init [options]")
		fmt.Println("  --force    Reinitialize existing storage")
		return
	}

	if err := parser.ValidateOptions([]string{"force"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(0, "gh-storage init [options]"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	force := parser.GetBoolOption("force")

	storageHome := getStorageHome()

	if err := storage.Initialize(storageHome, force); err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing storage: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Storage initialized at %s\n", storageHome)
}

func handleInfo(args []string) {
	var path string
	var detailed bool
	format := "table"

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--detailed":
			detailed = true
		case "--format":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--format requires a value\n")
				os.Exit(1)
			}
			format = args[i+1]
			i++
		case "-h", "--help":
			fmt.Println("Usage: gh-storage info [path] [options]")
			fmt.Println("  --detailed        Show detailed statistics")
			fmt.Println("  --format FORMAT   Output format (table, json, yaml)")
			return
		default:
			if path == "" {
				path = args[i]
			} else {
				fmt.Fprintf(os.Stderr, "Too many arguments\n")
				os.Exit(1)
			}
		}
	}

	storageHome := getStorageHome()
	infoDisplayer, err := storage.NewCLIInfoDisplayer(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating info displayer: %v\n", err)
		os.Exit(1)
	}

	if err := infoDisplayer.ShowInfo(path, detailed, format); err != nil {
		fmt.Fprintf(os.Stderr, "Error showing info: %v\n", err)
		os.Exit(1)
	}
}

func handleList(args []string) {
	path, recursive, format, filter, err := parseListArgs(args)
	if err != nil {
		if err.Error() == "help requested" {
			return
		}
		fmt.Fprintf(os.Stderr, "Error parsing arguments: %v\n", err)
		os.Exit(1)
	}

	storageHome := getStorageHome()
	infoDisplayer, err := storage.NewCLIInfoDisplayer(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating info displayer: %v\n", err)
		os.Exit(1)
	}

	if err := infoDisplayer.ListFiles(path, recursive, format, filter); err != nil {
		fmt.Fprintf(os.Stderr, "Error listing storage: %v\n", err)
		os.Exit(1)
	}
}

func parseListArgs(args []string) (string, bool, string, string, error) {
	var path string
	var recursive bool
	format := "table"
	var filter string

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--recursive":
			recursive = true
		case "--format":
			if i+1 >= len(args) {
				return "", false, "", "", fmt.Errorf("--format requires a value")
			}
			format = args[i+1]
			i++
		case "--filter":
			if i+1 >= len(args) {
				return "", false, "", "", fmt.Errorf("--filter requires a value")
			}
			filter = args[i+1]
			i++
		case "-h", "--help":
			showListUsage()
			return "", false, "", "", fmt.Errorf("help requested")
		default:
			if path == "" {
				path = args[i]
			} else {
				return "", false, "", "", fmt.Errorf("too many arguments")
			}
		}
	}

	return path, recursive, format, filter, nil
}

func showListUsage() {
	fmt.Println("Usage: gh-storage ls [path] [options]")
	fmt.Println("  --recursive       List recursively")
	fmt.Println("  --format FORMAT   Output format (table, json, tree)")
	fmt.Println("  --filter PATTERN  Filter by pattern")
}

func handleGet(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-storage get <key> [options]")
		fmt.Println("  --format FORMAT   Output format (json, yaml, raw)")
		fmt.Println("  --pretty          Pretty-print output")
		return
	}

	if err := parser.ValidateOptions([]string{"format", "pretty"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(1, "gh-storage get <key> [options]"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	key := parser.GetPositional(0)
	format := parser.GetOption("format")
	if format == "" {
		format = "json"
	}
	pretty := parser.GetBoolOption("pretty")

	storageHome := getStorageHome()
	store, err := storage.NewFileSystemStore(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating store: %v\n", err)
		os.Exit(1)
	}

	formatter := storage.NewCLIFormatter(store)
	if err := formatter.GetFormatted(key, format, pretty); err != nil {
		fmt.Fprintf(os.Stderr, "Error getting value: %v\n", err)
		os.Exit(1)
	}
}

func handleSet(args []string) {
	if len(args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: gh-storage set <key> <value> [options]\n")
		os.Exit(1)
	}

	key := args[0]
	value := args[1]
	var fromFile bool
	var isJSON bool
	var isYAML bool

	for i := 2; i < len(args); i++ {
		switch args[i] {
		case "--file":
			fromFile = true
		case "--json":
			isJSON = true
		case "--yaml":
			isYAML = true
		case "-h", "--help":
			fmt.Println("Usage: gh-storage set <key> <value> [options]")
			fmt.Println("  --file   Read value from file")
			fmt.Println("  --json   Parse value as JSON")
			fmt.Println("  --yaml   Parse value as YAML")
			return
		default:
			fmt.Fprintf(os.Stderr, "Unknown option: %s\n", args[i])
			os.Exit(1)
		}
	}

	storageHome := getStorageHome()
	store, err := storage.NewFileSystemStore(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating store: %v\n", err)
		os.Exit(1)
	}

	formatter := storage.NewCLIFormatter(store)
	if err := formatter.SetFormatted(key, value, fromFile, isJSON, isYAML); err != nil {
		fmt.Fprintf(os.Stderr, "Error setting value: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Set %s\n", key)
}

func handleDelete(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-storage delete <key>")
		return
	}

	if err := parser.ValidateOptions([]string{}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(1, "gh-storage delete <key>"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	key := parser.GetPositional(0)

	storageHome := getStorageHome()
	store, err := storage.NewFileSystemStore(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating store: %v\n", err)
		os.Exit(1)
	}

	if err := store.Delete(key); err != nil {
		fmt.Fprintf(os.Stderr, "Error deleting value: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Deleted %s\n", key)
}

func handleBackup(args []string) {
	var path string
	var all, compress bool
	var description string

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--all":
			all = true
		case "--compress":
			compress = true
		case "--description":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--description requires a value\n")
				os.Exit(1)
			}
			description = args[i+1]
			i++
		case "-h", "--help":
			fmt.Println("Usage: gh-storage backup [path] [options]")
			fmt.Println("  --all                Backup entire storage")
			fmt.Println("  --compress           Compress backup files")
			fmt.Println("  --description TEXT   Backup description")
			return
		default:
			if path == "" {
				path = args[i]
			} else {
				fmt.Fprintf(os.Stderr, "Too many arguments\n")
				os.Exit(1)
			}
		}
	}

	storageHome := getStorageHome()
	backupDir := getBackupDir()

	backupID, err := storage.CreateBackup(storageHome, backupDir, path, all, compress, description)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating backup: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Backup created: %s\n", backupID)
}

func handleRestore(args []string) {
	backupID, path, list, preview, err := parseRestoreArgs(args)
	if err != nil {
		if err.Error() == "help requested" {
			return
		}
		fmt.Fprintf(os.Stderr, "Error parsing arguments: %v\n", err)
		os.Exit(1)
	}

	storageHome := getStorageHome()
	backupDir := getBackupDir()

	if list {
		if err := storage.ListBackups(backupDir); err != nil {
			fmt.Fprintf(os.Stderr, "Error listing backups: %v\n", err)
			os.Exit(1)
		}
		return
	}

	if err := storage.RestoreBackup(storageHome, backupDir, backupID, path, preview); err != nil {
		fmt.Fprintf(os.Stderr, "Error restoring backup: %v\n", err)
		os.Exit(1)
	}

	if preview {
		fmt.Println("Restore preview completed")
	} else {
		fmt.Printf("Restored from backup: %s\n", backupID)
	}
}

func parseRestoreArgs(args []string) (string, string, bool, bool, error) {
	if len(args) < 1 {
		return "", "", false, false, fmt.Errorf("backup-id is required")
	}

	backupID := args[0]
	var path string
	var list, preview bool

	for i := 1; i < len(args); i++ {
		switch args[i] {
		case "--list":
			list = true
		case "--preview":
			preview = true
		case "-h", "--help":
			showRestoreUsage()
			return "", "", false, false, fmt.Errorf("help requested")
		default:
			if path == "" {
				path = args[i]
			} else {
				return "", "", false, false, fmt.Errorf("too many arguments")
			}
		}
	}

	return backupID, path, list, preview, nil
}

func showRestoreUsage() {
	fmt.Println("Usage: gh-storage restore <backup-id> [path] [options]")
	fmt.Println("  --list     List available backups")
	fmt.Println("  --preview  Preview restore operation")
}

func handleClean(args []string) {
	olderThan, cleanType, dryRun, err := parseCleanArgs(args)
	if err != nil {
		if err.Error() == "help requested" {
			return
		}
		fmt.Fprintf(os.Stderr, "Error parsing arguments: %v\n", err)
		os.Exit(1)
	}

	storageHome := getStorageHome()

	if err := storage.Clean(storageHome, olderThan, cleanType, dryRun); err != nil {
		fmt.Fprintf(os.Stderr, "Error cleaning storage: %v\n", err)
		os.Exit(1)
	}

	if dryRun {
		fmt.Println("Clean preview completed")
	} else {
		fmt.Println("Storage cleaned")
	}
}

func parseCleanArgs(args []string) (time.Duration, string, bool, error) {
	var olderThanStr, cleanType string
	var dryRun bool

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--older-than":
			if i+1 >= len(args) {
				return 0, "", false, fmt.Errorf("--older-than requires a value")
			}
			olderThanStr = args[i+1]
			i++
		case "--type":
			if i+1 >= len(args) {
				return 0, "", false, fmt.Errorf("--type requires a value")
			}
			cleanType = args[i+1]
			i++
		case "--dry-run":
			dryRun = true
		case "-h", "--help":
			showCleanUsage()
			return 0, "", false, fmt.Errorf("help requested")
		default:
			return 0, "", false, fmt.Errorf("unknown option: %s", args[i])
		}
	}

	var olderThan time.Duration
	if olderThanStr != "" {
		var err error
		olderThan, err = time.ParseDuration(olderThanStr)
		if err != nil {
			return 0, "", false, fmt.Errorf("invalid duration %q: %w", olderThanStr, err)
		}
	}

	return olderThan, cleanType, dryRun, nil
}

func showCleanUsage() {
	fmt.Println("Usage: gh-storage clean [options]")
	fmt.Println("  --older-than DURATION  Remove data older than duration (e.g., 720h, 24h)")
	fmt.Println("  --type TYPE            Data type to clean (cache, temp, all)")
	fmt.Println("  --dry-run              Preview cleanup without changes")
}

func handleVacuum(args []string) {
	var opts []storage.VacuumOption

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--compress":
			opts = append(opts, storage.VacuumCompress{})
		case "--defragment":
			opts = append(opts, storage.VacuumDefragment{})
		case "--verify":
			opts = append(opts, storage.VacuumVerify{})
		case "-h", "--help":
			fmt.Println("Usage: gh-storage vacuum [options]")
			fmt.Println("  --compress     Compress storage files")
			fmt.Println("  --defragment   Defragment storage indexes")
			fmt.Println("  --verify       Verify integrity after vacuum")
			return
		default:
			fmt.Fprintf(os.Stderr, "Unknown option: %s\n", args[i])
			os.Exit(1)
		}
	}

	storageHome := getStorageHome()

	if err := storage.Vacuum(storageHome, opts...); err != nil {
		fmt.Fprintf(os.Stderr, "Error vacuuming storage: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Storage vacuumed")
}

func handleLock(args []string) {
	var list, release, status, force bool
	var path string

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--list":
			list = true
		case "--release":
			release = true
		case "--status":
			status = true
		case "--force":
			force = true
		case "-h", "--help":
			fmt.Println("Usage: gh-storage lock [options]")
			fmt.Println("  --list            List active locks")
			fmt.Println("  --release PATH    Release lock on path")
			fmt.Println("  --status PATH     Show lock status for path")
			fmt.Println("  --force           Force release stuck locks")
			return
		default:
			if path == "" {
				path = args[i]
			} else {
				fmt.Fprintf(os.Stderr, "Too many arguments\n")
				os.Exit(1)
			}
		}
	}

	storageHome := getStorageHome()

	if err := storage.ManageLocks(storageHome, list, release, status, force, path); err != nil {
		fmt.Fprintf(os.Stderr, "Error managing locks: %v\n", err)
		os.Exit(1)
	}
}

func handleExport(args []string) {
	if len(args) < 1 {
		fmt.Fprintf(os.Stderr, "Usage: gh-storage export <path> [options]\n")
		os.Exit(1)
	}

	path := args[0]
	format := "json"
	var compress, includeMetadata bool

	for i := 1; i < len(args); i++ {
		switch args[i] {
		case "--format":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--format requires a value\n")
				os.Exit(1)
			}
			format = args[i+1]
			i++
		case "--compress":
			compress = true
		case "--include-metadata":
			includeMetadata = true
		case "-h", "--help":
			fmt.Println("Usage: gh-storage export <path> [options]")
			fmt.Println("  --format FORMAT        Export format (json, yaml, tar)")
			fmt.Println("  --compress             Compress exported data")
			fmt.Println("  --include-metadata     Include metadata in export")
			return
		default:
			fmt.Fprintf(os.Stderr, "Unknown option: %s\n", args[i])
			os.Exit(1)
		}
	}

	storageHome := getStorageHome()

	if err := storage.Export(storageHome, path, format, compress, includeMetadata); err != nil {
		fmt.Fprintf(os.Stderr, "Error exporting: %v\n", err)
		os.Exit(1)
	}
}

func handleImport(args []string) {
	if len(args) < 1 {
		fmt.Fprintf(os.Stderr, "Usage: gh-storage import <path> [options]\n")
		os.Exit(1)
	}

	path := args[0]
	format := "json"
	var merge, overwrite bool

	for i := 1; i < len(args); i++ {
		switch args[i] {
		case "--format":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--format requires a value\n")
				os.Exit(1)
			}
			format = args[i+1]
			i++
		case "--merge":
			merge = true
		case "--overwrite":
			overwrite = true
		case "-h", "--help":
			fmt.Println("Usage: gh-storage import <path> [options]")
			fmt.Println("  --format FORMAT   Import format (json, yaml, tar)")
			fmt.Println("  --merge           Merge with existing data")
			fmt.Println("  --overwrite       Overwrite existing data")
			return
		default:
			fmt.Fprintf(os.Stderr, "Unknown option: %s\n", args[i])
			os.Exit(1)
		}
	}

	storageHome := getStorageHome()

	if err := storage.Import(storageHome, path, format, merge, overwrite); err != nil {
		fmt.Fprintf(os.Stderr, "Error importing: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Imported to %s\n", path)
}

func handleVerify(args []string) {
	for _, arg := range args {
		switch arg {
		case "-h", "--help":
			fmt.Println("Usage: gh-storage verify")
			return
		default:
			if strings.HasPrefix(arg, "-") {
				fmt.Fprintf(os.Stderr, "Unknown option: %s\n", arg)
				os.Exit(1)
			}
		}
	}

	storageHome := getStorageHome()

	if err := storage.Verify(storageHome); err != nil {
		fmt.Fprintf(os.Stderr, "Storage verification failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Storage verification passed")
}
