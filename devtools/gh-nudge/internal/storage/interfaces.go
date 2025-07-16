package storage

// Store defines the core storage interface for basic CRUD operations.
// This is the minimal interface that most consumers will need.
type Store interface {
	Get(key string, dest interface{}) error
	Set(key string, data interface{}) error
	Delete(key string) error
	Exists(key string) bool
}

// Locker provides exclusive locking functionality for storage operations.
type Locker interface {
	WithLock(key string, fn func() error) error
}

// Lister provides directory and listing operations.
type Lister interface {
	List(prefix string) ([]string, error)
	GetChildren(path string) ([]string, error)
}

// MetadataManager handles metadata operations for stored items.
type MetadataManager interface {
	GetMetadata(key string) (*Metadata, error)
	SetMetadata(key string, metadata *Metadata) error
}

// Interface compliance checks.
var (
	_ Store           = (*FileSystemStore)(nil)
	_ Locker          = (*FileLockManager)(nil)
	_ Lister          = (*FileSystemLister)(nil)
	_ MetadataManager = (*FileSystemMetadataManager)(nil)
)
