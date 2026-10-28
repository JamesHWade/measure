"""Local SQLite storage shared by analytical records and the experiment registry."""
from __future__ import annotations

from contextlib import contextmanager
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import sqlite3
from typing import Any, Callable, Iterator


def canonical(value: object) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False)


def sha256(value: object) -> str:
    return hashlib.sha256(canonical(value).encode("utf-8")).hexdigest()


def now() -> str:
    return datetime.now(timezone.utc).isoformat()


def db_path() -> Path:
    root = Path(os.environ.get("MEASURE_MCP_DATA_DIR", str(Path.home() / ".local" / "share" / "measure-mcp")))
    root.mkdir(mode=0o700, parents=True, exist_ok=True)
    return root / "measure.sqlite3"


def connect() -> sqlite3.Connection:
    con = sqlite3.connect(db_path(), timeout=30)
    con.execute("PRAGMA foreign_keys=ON")
    con.executescript("""
    CREATE TABLE IF NOT EXISTS datasets (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, digest TEXT NOT NULL,
      payload TEXT NOT NULL, created_at TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS analyses (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, dataset_id TEXT NOT NULL,
      trace_id TEXT NOT NULL, kind TEXT NOT NULL, method TEXT NOT NULL,
      result TEXT NOT NULL, provenance TEXT NOT NULL, created_at TEXT NOT NULL,
      FOREIGN KEY(dataset_id) REFERENCES datasets(id)
    );
    CREATE TABLE IF NOT EXISTS dataset_sources (
      dataset_id TEXT PRIMARY KEY, kind TEXT NOT NULL, digest TEXT NOT NULL,
      content TEXT NOT NULL, mapping TEXT NOT NULL,
      FOREIGN KEY(dataset_id) REFERENCES datasets(id)
    );
    CREATE TABLE IF NOT EXISTS executable_methods (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, title TEXT NOT NULL,
      payload TEXT NOT NULL, input_digest TEXT NOT NULL,
      program_digest TEXT NOT NULL, created_at TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS methods_owner_order
      ON executable_methods(owner, created_at, id);
    CREATE TABLE IF NOT EXISTS experiments (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, payload TEXT NOT NULL,
      digest TEXT NOT NULL, created_at TEXT NOT NULL
    );
    CREATE INDEX IF NOT EXISTS experiments_owner_order ON experiments(owner, created_at, id);
    CREATE TABLE IF NOT EXISTS experiment_attempts (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, experiment_id TEXT NOT NULL,
      latest_revision INTEGER NOT NULL, created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES experiments(id)
    );
    CREATE INDEX IF NOT EXISTS attempts_owner_experiment
      ON experiment_attempts(owner, experiment_id, created_at, id);
    CREATE TABLE IF NOT EXISTS attempt_revisions (
      attempt_id TEXT NOT NULL, revision INTEGER NOT NULL, payload TEXT NOT NULL,
      digest TEXT NOT NULL, reason TEXT NOT NULL, created_at TEXT NOT NULL,
      PRIMARY KEY(attempt_id, revision),
      FOREIGN KEY(attempt_id) REFERENCES experiment_attempts(id)
    );
    CREATE TABLE IF NOT EXISTS experiment_results (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, attempt_id TEXT NOT NULL,
      attempt_revision INTEGER NOT NULL, metric TEXT NOT NULL,
      analyte_reference TEXT, supersedes_id TEXT UNIQUE,
      payload TEXT NOT NULL, digest TEXT NOT NULL, created_at TEXT NOT NULL,
      FOREIGN KEY(attempt_id, attempt_revision) REFERENCES attempt_revisions(attempt_id, revision),
      FOREIGN KEY(supersedes_id) REFERENCES experiment_results(id)
    );
    CREATE INDEX IF NOT EXISTS results_owner_attempt
      ON experiment_results(owner, attempt_id, metric, analyte_reference, created_at, id);
    CREATE TABLE IF NOT EXISTS registry_receipts (
      owner TEXT NOT NULL, request_key TEXT NOT NULL, digest TEXT NOT NULL,
      response TEXT NOT NULL, created_at TEXT NOT NULL,
      PRIMARY KEY(owner, request_key)
    );
    """)
    return con


@contextmanager
def registry_connection(write: bool = False) -> Iterator[sqlite3.Connection]:
    """Serialize registry mutations, atomically committing records and retry receipts."""
    con = connect()
    con.row_factory = sqlite3.Row
    try:
        con.execute("BEGIN IMMEDIATE" if write else "BEGIN")
        yield con
        con.commit()
    except BaseException:
        con.rollback()
        raise
    finally:
        con.close()


def registry_mutation(owner: str, key: str, request: dict,
                      action: Callable[[sqlite3.Connection], dict]) -> dict[str, Any]:
    """Commit a bounded registry mutation and its retry receipt in one transaction."""
    if not isinstance(key, str) or not 1 <= len(key) <= 100 or not key.strip():
        raise ValueError("A nonempty idempotency key of at most 100 characters is required")
    serialized = canonical(request)
    if len(serialized.encode("utf-8")) > 64_000:
        raise ValueError("Registry record or result batch exceeds the byte limit")
    digest = sha256(request)
    with registry_connection(write=True) as con:
        receipt = con.execute("SELECT digest, response FROM registry_receipts WHERE owner=? AND request_key=?", (owner, key)).fetchone()
        if receipt is not None:
            if receipt["digest"] != digest:
                raise ValueError("Idempotency key was already used for a different registry request")
            return json.loads(receipt["response"])
        response = action(con)
        con.execute("INSERT INTO registry_receipts VALUES (?, ?, ?, ?, ?)", (owner, key, digest, canonical(response), now()))
        return response
