"""Immutable executable methods linked to owner-scoped experiment records."""
from __future__ import annotations

import json
import sqlite3
import uuid

from methods import MethodConstraints, MethodDeclaration, evaluate_method
from storage import canonical, now, registry_connection, registry_mutation, sha256

MAX_METHOD_PAGE = 50
MAX_METHOD_BYTES = 64_000


def _owned_method(con: sqlite3.Connection, owner: str, method_id: str) -> sqlite3.Row:
    row = con.execute("SELECT * FROM executable_methods WHERE id=? AND owner=?", (method_id, owner)).fetchone()
    if row is None:
        raise ValueError("Executable method was not found or is not accessible")
    return row


def method_link(con: sqlite3.Connection, owner: str, method_id: str | None) -> dict | None:
    """Resolve an immutable method and retain its exact identity within a transaction."""
    if method_id is None:
        return None
    row = _owned_method(con, owner, method_id)
    return {"method_id": row["id"], "schema_version": "1.0",
            "input_sha256": row["input_digest"], "canonical_program_sha256": row["program_digest"]}


def _record(row: sqlite3.Row) -> dict:
    return {"schema_version": "1.0", "method_id": row["id"], "title": row["title"],
            **json.loads(row["payload"]), "input_sha256": row["input_digest"],
            "canonical_program_sha256": row["program_digest"], "created_at": row["created_at"]}


def register_method(owner: str, title: str, declaration: MethodDeclaration,
                    constraints: MethodConstraints | None, key: str) -> dict:
    if not isinstance(title, str) or not title.strip() or len(title) > 200:
        raise ValueError("A method title of 1 to 200 characters is required")
    limits = MethodConstraints() if constraints is None else constraints
    requested = {"title": title, "declaration": declaration.model_dump(mode="json"),
                 "constraints": limits.model_dump(mode="json")}

    def write(con: sqlite3.Connection) -> dict:
        evaluation = evaluate_method(declaration, limits)
        if not evaluation.feasible:
            codes = sorted({v.code for v in evaluation.violations})
            raise ValueError("Executable method violates declared constraints: " + ", ".join(codes))
        retained = {"evaluation": evaluation.model_dump(mode="json"), "constraints": requested["constraints"]}
        serialized = canonical(retained)
        if len(serialized.encode("utf-8")) > MAX_METHOD_BYTES:
            raise ValueError("Executable method record exceeds the byte limit")
        method_id = str(uuid.uuid4())
        con.execute("INSERT INTO executable_methods VALUES (?, ?, ?, ?, ?, ?, ?)",
                    (method_id, owner, title, serialized, sha256(requested),
                     sha256(evaluation.canonical.model_dump(mode="json")), now()))
        response = _record(_owned_method(con, owner, method_id))
        if len(canonical(response).encode("utf-8")) > MAX_METHOD_BYTES:
            raise ValueError("Executable method record exceeds the byte limit")
        return response

    return registry_mutation(owner, key, {"operation": "register_executable_method", **requested}, write)


def get_method(owner: str, method_id: str) -> dict:
    with registry_connection() as con:
        return _record(_owned_method(con, owner, method_id))


def list_methods(owner: str, offset: int = 0, limit: int = 20) -> dict:
    if isinstance(offset, bool) or not isinstance(offset, int) or offset < 0:
        raise ValueError("Method offset must be a nonnegative integer")
    if isinstance(limit, bool) or not isinstance(limit, int) or not 1 <= limit <= MAX_METHOD_PAGE:
        raise ValueError("Method page limit must be between 1 and 50")
    with registry_connection() as con:
        count = con.execute("SELECT COUNT(*) FROM executable_methods WHERE owner=?", (owner,)).fetchone()[0]
        rows = con.execute("SELECT id, title, input_digest, program_digest, created_at FROM executable_methods WHERE owner=? ORDER BY created_at, id LIMIT ? OFFSET ?",
                           (owner, limit, offset)).fetchall()
    items = [{"method_id": r["id"], "title": r["title"], "input_sha256": r["input_digest"],
              "canonical_program_sha256": r["program_digest"], "created_at": r["created_at"]} for r in rows]
    return {"schema_version": "1.0", "offset": offset, "count": count, "items": items,
            "next_offset": offset + limit if offset + limit < count else None}
