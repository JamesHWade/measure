# public import errors explain ignored columns and malformed numeric fields

    Code
      measure_record_from_long(data, record_import_columns, "source-1")
    Condition
      Error:
      ! Unmapped input columns require `allow_extra_columns = TRUE`: `note`.

---

    Code
      read_measure_record_delim(path, columns, "source-1")
    Condition
      Error:
      ! Invalid numeric token in field `value` at row 1.

# quoted numeric cells do not hide a trailing newline

    Code
      read_measure_record_delim(path, columns, "source-1")
    Condition
      Error:
      ! Invalid numeric token in field `value` at row 1.

# ignored extra columns still require strict UTF-8

    Code
      read_measure_record_delim(path, columns, "source-1", allow_extra_columns = TRUE)
    Condition
      Error:
      ! Delimited input must be valid UTF-8.
