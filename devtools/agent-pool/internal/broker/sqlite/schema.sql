CREATE TABLE IF NOT EXISTS tokens (
    id TEXT PRIMARY KEY,
    role TEXT NOT NULL,
    name TEXT NOT NULL,
    allowed_repos_json TEXT NOT NULL,
    salt BLOB NOT NULL,
    token_hash BLOB NOT NULL,
    created_at INTEGER NOT NULL,
    revoked_at INTEGER
);

CREATE TABLE IF NOT EXISTS catalogs (
    repo TEXT PRIMARY KEY,
    default_branch TEXT NOT NULL,
    catalog_sha TEXT NOT NULL,
    catalog_json BLOB NOT NULL,
    updated_at INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS workers (
    worker_id TEXT PRIMARY KEY,
    alias TEXT NOT NULL,
    supported_repos_json TEXT NOT NULL,
    supported_skills_json TEXT NOT NULL,
    accepting_jobs INTEGER NOT NULL,
    concurrency_available INTEGER NOT NULL,
    quota_json TEXT NOT NULL,
    last_heartbeat INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS jobs (
    job_id TEXT PRIMARY KEY,
    requester_id TEXT NOT NULL,
    idempotency_key TEXT NOT NULL DEFAULT '',
    repo TEXT NOT NULL,
    default_branch TEXT NOT NULL DEFAULT '',
    catalog_sha TEXT NOT NULL DEFAULT '',
    skill TEXT NOT NULL,
    args_json BLOB,
    status TEXT NOT NULL,
    attempts INTEGER NOT NULL DEFAULT 0,
    max_attempts INTEGER NOT NULL DEFAULT 1,
    worker_id TEXT NOT NULL DEFAULT '',
    worker_alias TEXT NOT NULL DEFAULT '',
    lease_token TEXT NOT NULL DEFAULT '',
    lease_expires_at INTEGER,
    cancel_requested INTEGER NOT NULL DEFAULT 0,
    deliverable_json BLOB,
    usage_json BLOB,
    progress_json BLOB,
    error_code TEXT NOT NULL DEFAULT '',
    error_message TEXT NOT NULL DEFAULT '',
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_jobs_requester_idempotency
    ON jobs (requester_id, idempotency_key)
    WHERE idempotency_key != '';

CREATE INDEX IF NOT EXISTS idx_jobs_status_repo_created
    ON jobs (status, repo, created_at);
