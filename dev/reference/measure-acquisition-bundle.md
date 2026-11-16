# Read and write native measurement acquisition bundles

Acquisition bundles use one fixed DCF manifest and twelve flat CSV or
TSV payloads. The profile retains linked scalar record metadata, raw MS
scans, and two-dimensional measurement arrays. It does not infer missing
scans, units, interpretation, or source identity.

## Usage

``` r
write_measure_acquisition(x, path, delim = c(",", "\t"))

read_measure_acquisition(path, max_bytes = 100 * 1024^2)
```

## Arguments

- x:

  A validated `measure_acquisition` object.

- path:

  For writing, a new local directory path whose parent exists. For
  reading, a directory containing a complete acquisition bundle.

- delim:

  A comma or tab field delimiter used for all payloads.

- max_bytes:

  A positive whole-number limit for actual cumulative bytes read from
  the manifest and twelve selected payloads.

## Value

`write_measure_acquisition()` invisibly returns the normalized
destination path. `read_measure_acquisition()` returns a validated
`measure_acquisition` object.

## Bundle version 1

The selected files and exact ordered headers are specified in
`inst/specs/measure-acquisition-v1.md`. All selected members have fixed
names; manifest paths are never interpreted. Payload hashes cover exact
bytes and detect changes, but do not authenticate a source.

The writer validates supported object attributes before staging, reads
back the staged bundle and checks fidelity, then reserves a new
destination and publishes the manifest last. Existing paths are never
replaced.
