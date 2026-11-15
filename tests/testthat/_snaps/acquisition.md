# acquisition construction rejects invalid declarations

    Code
      measure_acquisition(record, streams = streams, scans = transform(scans,
        scan_index = 0L))
    Condition
      Error in `.acq_integer()`:
      ! scans$scan_index must contain positive integers or missing values.

---

    Code
      measure_acquisition(record, streams = streams, scans = transform(scans,
        stream_id = "nd-stream"))
    Condition
      Error in `.acq_validate()`:
      ! Every scan must refer to an existing MS stream.

---

    Code
      measure_acquisition(record, streams = streams, arrays = tibble::tibble(
        array_id = "a", stream_id = "ms-stream", .measures = list(new_measure_nd_tbl(
          location_1 = 1, location_2 = 2, value = 3))))
    Condition
      Error in `.acq_validate()`:
      ! Every array must refer to an existing ND stream.

---

    Code
      measure_acquisition(record, streams = tibble::tibble(stream_id = "nd-stream",
        injection_id = "i", channel_id = "nd", kind = "nd", time_unit = "min"))
    Condition
      Error in `.acq_validate()`:
      ! ND stream time_unit and mz_unit must be missing.

---

    Code
      measure_acquisition(record, streams = streams, scans = tibble::tibble(scan_id = "scan",
        stream_id = "ms-stream", scan_index = 1L, scan_status = "acquired",
        .measures = named_measures))
    Condition
      Error in `.acq_check_attributes()`:
      ! scans$.measures has unsupported attributes.

# acquisitions reject invalid mutations during revalidation

    Code
      validate_measure_acquisition(bad_index)
    Condition
      Error in `.acq_validate_integer_column()`:
      ! scans$scan_index must be a canonical integer vector.

---

    Code
      validate_measure_acquisition(bad_mask)
    Condition
      Error in `FUN()`:
      ! scans$.saturated[[1]] must align with its measurement points.

---

    Code
      validate_measure_acquisition(bad_payload)
    Condition
      Error in `.acq_plain_vector()`:
      ! scan 1 payload$value must not have vector attributes.

---

    Code
      validate_measure_acquisition(bad_record)
    Condition
      Error in `.acq_check_attributes()`:
      ! record$curves has unsupported attributes.

# acquisition validation rejects malformed table and payload shapes

    Code
      measure_acquisition(record, streams, scans)
    Condition
      Error in `.acq_validate_measure_tbl()`:
      ! Columns in scan 1 payload must each have a length matching its row count.

---

    Code
      measure_acquisition(record, streams = malformed_input_streams)
    Condition
      Error in `.acq_check_table_shape()`:
      ! Columns in input table streams must each have a length matching its row count.

---

    Code
      validate_measure_acquisition(bad_payload)
    Condition
      Error in `.acq_validate_measure_tbl()`:
      ! Columns in scan 1 payload must each have a length matching its row count.

---

    Code
      validate_measure_acquisition(bad_stream_table)
    Condition
      Error in `.acq_check_table_shape()`:
      ! Columns in component streams must each have a length matching its row count.

---

    Code
      validate_measure_acquisition(bad_record_table)
    Condition
      Error in `.acq_check_table_shape()`:
      ! Columns in record table channels must each have a length matching its row count.
