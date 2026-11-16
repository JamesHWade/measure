# unknown scan selectors error only when the row could match

    Code
      measure_ms_trace(ambiguous, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L)
    Condition
      Error:
      ! A scan with unknown polarity or MS level could match the requested selectors.

# unknown time or m/z units prevent the extraction that needs them

    Code
      measure_ms_trace(unknown_time, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L)
    Condition
      Error:
      ! A centroid trace requires a known declared time unit.

---

    Code
      measure_ms_trace(unknown_mz, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L, mz_range = c(90, 110))
    Condition
      Error:
      ! An XIC requires a known declared m/z unit for the stream.

# trace extraction rejects unsupported scan and coordinate states

    Code
      measure_ms_trace(profile, stream_id = "ms-1", polarity = "positive", ms_level = 1L)
    Condition
      Error:
      ! Acquired scan `profile` must declare centroid spectrum type.

---

    Code
      measure_ms_trace(unknown_type, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L)
    Condition
      Error:
      ! Acquired scan `unknown-type` must declare centroid spectrum type.

---

    Code
      measure_ms_trace(bad_mz, stream_id = "ms-1", polarity = "positive", ms_level = 1L,
        mz_range = c(90, 110))
    Condition
      Error:
      ! Every m/z in acquired selected scan `bad-mz` must be finite and positive.

---

    Code
      measure_ms_trace(bad_time, stream_id = "ms-1", polarity = "positive", ms_level = 1L)
    Condition
      Error:
      ! Every selected scan must have a known finite retention time.

# selected invalid intensities and unrepresentable sums error

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L)
    Condition
      Error:
      ! Selected centroid intensities in scan `invalid` must be finite and nonnegative, or ordinary NA.

---

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L)
    Condition
      Error:
      ! Selected centroid intensities in scan `invalid` must be finite and nonnegative, or ordinary NA.

---

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L)
    Condition
      Error:
      ! Selected centroid intensities in scan `invalid` must be finite and nonnegative, or ordinary NA.

---

    Code
      measure_ms_trace(overflow, stream_id = "ms-1", polarity = "positive", ms_level = 1L)
    Condition
      Error:
      ! Selected centroid intensities in scan `overflow` have an unrepresentable sum.

# selectors and XIC intervals are explicit plain values

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive")
    Condition
      Error in `measure_ms_trace()`:
      ! argument "ms_level" is missing, with no default

---

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "unknown",
        ms_level = 1L)
    Condition
      Error:
      ! `polarity` must be exactly `positive` or `negative`.

---

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive",
        ms_level = 0L)
    Condition
      Error:
      ! `ms_level` must be one positive whole-number value.

---

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L, mz_range = c(100, Inf))
    Condition
      Error:
      ! `mz_range` must be a plain finite positive interval with lower <= upper.

---

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive",
        ms_level = 1L, mz_range = c(101, 100))
    Condition
      Error:
      ! `mz_range` must be a plain finite positive interval with lower <= upper.

---

    Code
      measure_ms_trace(acquisition, stream_id = "missing-stream", polarity = "positive",
        ms_level = 1L)
    Condition
      Error:
      ! `stream_id` must identify exactly one declared MS stream.

---

    Code
      measure_ms_trace(acquisition, stream_id = "ms-1", polarity = "positive",
        ms_level = 2L)
    Condition
      Error:
      ! No scans match the requested stream, polarity, and MS level.
