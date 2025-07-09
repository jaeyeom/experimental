package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

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

	switch command {
	case "init":
		handleInit(args)
	case "info":
		handleInfo(args)
	case "ls":
		handleList(args)
	case "get":
		handleGet(args)
	case "set":
		handleSet(args)
	case "delete":
		handleDelete(args)
	case "migrate":
		handleMigrate(args)
	case "backup":
		handleBackup(args)
	case "restore":
		handleRestore(args)
	case "clean":
		handleClean(args)
	case "vacuum":
		handleVacuum(args)
	case "lock":
		handleLock(args)
	case "export":
		handleExport(args)
	case "import":
		handleImport(args)
	case "verify":
		handleVerify(args)
	case "version":
		fmt.Printf("gh-storage version %s\n", version)
	case "help", "-h", "--help":
		showUsage()
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
		showUsage()
		os.Exit(1)
	}
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
  migrate                 Migrate legacy data to unified storage
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
		fmt.Println("  --migrate  Automatically migrate legacy data")
		return
	}

	if err := parser.ValidateOptions([]string{"force", "migrate"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(0, "gh-storage init [options]"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	force := parser.HasOption("force")
	migrate := parser.HasOption("migrate")

	storageHome := getStorageHome()

	if err := storage.Initialize(storageHome, force, migrate); err != nil {
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
	store, err := storage.NewFileSystemStorage(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening storage: %v\n", err)
		os.Exit(1)
	}

	if err := store.ShowInfo(path, detailed, format); err != nil {
		fmt.Fprintf(os.Stderr, "Error showing info: %v\n", err)
		os.Exit(1)
	}
}

func handleList(args []string) {
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
				fmt.Fprintf(os.Stderr, "--format requires a value\n")
				os.Exit(1)
			}
			format = args[i+1]
			i++
		case "--filter":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--filter requires a value\n")
				os.Exit(1)
			}
			filter = args[i+1]
			i++
		case "-h", "--help":
			fmt.Println("Usage: gh-storage ls [path] [options]")
			fmt.Println("  --recursive       List recursively")
			fmt.Println("  --format FORMAT   Output format (table, json, tree)")
			fmt.Println("  --filter PATTERN  Filter by pattern")
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
	store, err := storage.NewFileSystemStorage(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening storage: %v\n", err)
		os.Exit(1)
	}

	if err := store.ListFiles(path, recursive, format, filter); err != nil {
		fmt.Fprintf(os.Stderr, "Error listing storage: %v\n", err)
		os.Exit(1)
	}
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
	pretty := parser.HasOption("pretty")

	storageHome := getStorageHome()
	store, err := storage.NewFileSystemStorage(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening storage: %v\n", err)
		os.Exit(1)
	}

	if err := store.GetFormatted(key, format, pretty); err != nil {
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
	store, err := storage.NewFileSystemStorage(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening storage: %v\n", err)
		os.Exit(1)
	}

	if err := store.SetFormatted(key, value, fromFile, isJSON, isYAML); err != nil {
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
	store, err := storage.NewFileSystemStorage(storageHome)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening storage: %v\n", err)
		os.Exit(1)
	}

	if err := store.Delete(key); err != nil {
		fmt.Fprintf(os.Stderr, "Error deleting value: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Deleted %s\n", key)
}

func handleMigrate(args []string) {
	var from, to string
	var dryRun, backup bool

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--from":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--from requires a value\n")
				os.Exit(1)
			}
			from = args[i+1]
			i++
		case "--to":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--to requires a value\n")
				os.Exit(1)
			}
			to = args[i+1]
			i++
		case "--dry-run":
			dryRun = true
		case "--backup":
			backup = true
		case "-h", "--help":
			fmt.Println("Usage: gh-storage migrate [options]")
			fmt.Println("  --from PATH     Source format/path")
			fmt.Println("  --to PATH       Target format/path")
			fmt.Println("  --dry-run       Preview migration without changes")
			fmt.Println("  --backup        Create backup before migration")
			return
		default:
			fmt.Fprintf(os.Stderr, "Unknown option: %s\n", args[i])
			os.Exit(1)
		}
	}

	storageHome := getStorageHome()

	if err := storage.Migrate(storageHome, from, to, dryRun, backup); err != nil {
		fmt.Fprintf(os.Stderr, "Error migrating: %v\n", err)
		os.Exit(1)
	}

	if dryRun {
		fmt.Println("Migration preview completed")
	} else {
		fmt.Println("Migration completed")
	}
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
	if len(args) < 1 {
		fmt.Fprintf(os.Stderr, "Usage: gh-storage restore <backup-id> [path] [options]\n")
		os.Exit(1)
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
			fmt.Println("Usage: gh-storage restore <backup-id> [path] [options]")
			fmt.Println("  --list     List available backups")
			fmt.Println("  --preview  Preview restore operation")
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

func handleClean(args []string) {
	var olderThan, cleanType string
	var dryRun bool

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--older-than":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--older-than requires a value\n")
				os.Exit(1)
			}
			olderThan = args[i+1]
			i++
		case "--type":
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "--type requires a value\n")
				os.Exit(1)
			}
			cleanType = args[i+1]
			i++
		case "--dry-run":
			dryRun = true
		case "-h", "--help":
			fmt.Println("Usage: gh-storage clean [options]")
			fmt.Println("  --older-than DURATION  Remove data older than duration (30d, 1w, etc.)")
			fmt.Println("  --type TYPE            Data type to clean (cache, logs, temp)")
			fmt.Println("  --dry-run              Preview cleanup without changes")
			return
		default:
			fmt.Fprintf(os.Stderr, "Unknown option: %s\n", args[i])
			os.Exit(1)
		}
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

func handleVacuum(args []string) {
	var compress, defragment, verify bool

	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--compress":
			compress = true
		case "--defragment":
			defragment = true
		case "--verify":
			verify = true
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

	if err := storage.Vacuum(storageHome, compress, defragment, verify); err != nil {
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
