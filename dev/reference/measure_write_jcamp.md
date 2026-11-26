# Write one measurement-record curve as a JCAMP-DX IR spectrum

`measure_write_jcamp()` writes one explicitly selected one-dimensional
curve from a validated
[`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md)
as a JCAMP-DX 4.24 IR spectrum. The output uses the human-readable AFFN
`##XYPOINTS= (XY..XY)` representation, with one pair per line. Point
order, irregular spacing, descending axes, finite doubles, and missing
signal values are retained. Missing signal values are represented by the
JCAMP-DX `?` token.

## Usage

``` r
measure_write_jcamp(record, path, curve_id, title, origin, owner)
```

## Arguments

- record:

  A validated
  [`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md)
  containing one-dimensional curves.

- path:

  A new local file path, usually ending in `.jdx` or `.dx`.

- curve_id:

  The exact identifier of the one curve to export.

- title:

  A printable ASCII, single-line JCAMP title.

- origin:

  A printable ASCII JCAMP origin declaration. CRLF and LF may separate
  literal continuation lines.

- owner:

  A printable ASCII JCAMP owner declaration. CRLF and LF may separate
  literal continuation lines.

## Value

The normalized destination path, invisibly.

## Details

The title and source unit declarations must be printable ASCII
single-line values. `origin` and `owner` may contain multiple printable
ASCII lines; CRLF and LF line endings are normalized to LF. Values are
written literally and are not inferred from record identities. In
particular, this export contains only the selected curve and does not
include sample, injection, channel, source, or other-curve identities.

The destination must be new. The file is staged beside `path` and
committed without replacing an existing file, so validation or
serialization errors do not leave a partial destination file.

## References

[IUPAC JCAMP-DX
specifications](https://iupac.org/what-we-do/digital-standards/jcamp-dx/)
