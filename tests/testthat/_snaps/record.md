# empty records have the complete typed schema

    Code
      print(record)
    Output
      <measure_record>
        Samples: 0
        Injections: 0
        Channels: 0
        Curves: 0
        Sources: 0

# the record print method shows entity counts

    Code
      print(record)
    Output
      <measure_record>
        Samples: 1
        Injections: 1
        Channels: 1
        Curves: 1
        Sources: 0

# constructor rejects unknown and duplicate columns

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample", note = "extra"))
    Condition
      Error in `.record_canonical_table()`:
      ! Table samples has unknown column: note.

---

    Code
      measure_record(samples = duplicate)
    Condition
      Error in `.record_canonical_table()`:
      ! Table samples has duplicate column sample_id.

# constructor and validator enforce relation keys and references

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "absent"))
    Condition
      Error in `.record_validate()`:
      ! Every injection must refer to a row in samples.

---

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_unique_key()`:
      ! channels$channel_id values must be unique.

# one injection cannot have two curves for the same channel

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = c("curve-a", "curve-b"),
      injection_id = c("inj", "inj"), channel_id = c("uv", "uv"), .measures = list(
        new_measure_tbl(1, 2), new_measure_tbl(2, 3))))
    Condition
      Error in `.record_validate()`:
      ! Each injection_id and channel_id pair may identify only one curve.

# known curve sources must resolve to declared source rows

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", source_id = "source-1", .measures = list(new_measure_tbl(1,
          2))))
    Condition
      Error in `.record_validate()`:
      ! Every known curve source ID must refer to a row in sources.

# IDs, metadata, and source hashes have explicit types

    Code
      measure_record(samples = tibble::tibble(sample_id = factor("007")))
    Condition
      Error in `.record_text()`:
      ! samples$sample_id must be a one-dimensional character vector.

---

    Code
      measure_record(samples = tibble::tibble(sample_id = byte_id))
    Condition
      Error in `.record_text()`:
      ! samples$sample_id must contain UTF-8 strings, not byte-marked text.

---

    Code
      measure_record(samples = tibble::tibble(sample_id = matrix_id))
    Condition
      Error in `.record_text()`:
      ! samples$sample_id must be a one-dimensional character vector.

---

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_validate_text_column()`:
      ! samples$sample_id must be a one-dimensional character vector.

---

    Code
      measure_record(samples = tibble::tibble(sample_id = intToUtf8(160)))
    Condition
      Error in `.record_text()`:
      ! samples$sample_id must not contain blank strings.

---

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", axis_unit = "   ", .measures = list(new_measure_tbl(1, 2))))
    Condition
      Error in `.record_text()`:
      ! curves$axis_unit must not contain blank strings.

---

    Code
      measure_record(sources = tibble::tibble(source_id = "source", source_sha256 = "abc"))
    Condition
      Error in `.record_validate()`:
      ! Known sources$source_sha256 values must be 64 lowercase hexadecimal characters.

# curve payloads reject invalid numeric values and unsupported dimensions

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", .measures = list(new_measure_tbl(c(1, 1), c(2, 3)))))
    Condition
      Error in `FUN()`:
      ! Curve 1 locations must be strictly increasing or strictly decreasing.

---

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", .measures = list(new_measure_tbl(1:2, c(2, Inf)))))
    Condition
      Error in `.record_numeric()`:
      ! curve 1 value must contain only finite values or allowed missing values.

---

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", .measures = list(new_measure_nd_tbl(location_1 = 1:2,
        location_2 = c(2, 3), value = c(4, 5)))))
    Condition
      Error in `FUN()`:
      ! Curve 1 has an nD payload; only 1D curves are supported.

# source row ordinals are positive, aligned, and unique per curve

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", .measures = list(new_measure_tbl(1:2, c(4, 3))),
        .source_rows = list(c(1L, 1L))))
    Condition
      Error in `FUN()`:
      ! Curve 1 source rows must be unique within the curve.

---

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", .measures = list(new_measure_tbl(1:2, c(4, 3))),
        .source_rows = list(c(1, 2.5))))
    Condition
      Error in `FUN()`:
      ! Curve 1 source rows must be positive integers or missing.

# source row columns reject matrix list columns at both boundaries

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", .measures = list(new_measure_tbl(1:2, c(4, 3))),
        .source_rows = source_rows))
    Condition
      Error in `.record_canonical_source_rows()`:
      ! Column curves$.source_rows must be a list with one vector per curve.

---

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_validate()`:
      ! Column curves$.source_rows must be a list aligned with curves.

# empty nD measurement lists are rejected at both boundaries

    Code
      measure_record(curves = empty_curves)
    Condition
      Error in `.record_canonical_measures()`:
      ! Column curves$.measures does not support nD payloads.

---

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_validate()`:
      ! Column curves$.measures does not support nD payloads.

# dimensional measure lists are rejected at both boundaries

    Code
      measure_record(samples = tibble::tibble(sample_id = "sample"), injections = tibble::tibble(
        injection_id = "inj", sample_id = "sample"), channels = tibble::tibble(
        channel_id = "uv"), curves = tibble::tibble(curve_id = "curve", injection_id = "inj",
        channel_id = "uv", .measures = measures))
    Condition
      Error in `.record_canonical_measures()`:
      ! Column curves$.measures must be a <measure_list> or a list of 1D data frames.

---

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_validate()`:
      ! Column curves$.measures must be a one-dimensional measure_list.

# constructor canonicalizes negative zero and validator rechecks it

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_numeric()`:
      ! curve 1 value must use canonical positive zero.

# validator rejects changed schemas and orphaned curve relationships

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_validate()`:
      ! A <measure_record> must have exactly these components in order: samples, injections, channels, curves, and sources.

---

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_validate()`:
      ! Component channels must be a tibble with canonical columns channel_id and channel_name.

---

    Code
      validate_measure_record(record)
    Condition
      Error in `.record_validate()`:
      ! Every injection must refer to a row in samples.
