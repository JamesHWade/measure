# Native measure acquisition profile, version 1

This profile stores one `measure_acquisition` object as fixed flat files. Its
identifier is `Format: measure-acquisition`, `Version: 1`. It composes the
existing scalar `measure_record` representation with raw one-dimensional MS
scans and existing two-dimensional measurement arrays. Construction and storage
do not imply analytical interpretation or instrument qualification.

The R interface is `write_measure_acquisition(x, path, delim = c(",", "\t"))`
and `read_measure_acquisition(path, max_bytes = 100 * 1024^2)`. Existing scalar
record and nD bundle formats remain independent; each reader rejects the other
profiles.

## Bundle files and manifest

A bundle contains one `manifest.dcf` and exactly twelve selected regular,
nonsymlink CSV or TSV payload files. The manifest selects the delimiter, never
paths. Unselected directory entries are ignored. The fixed payload names are:

| Prefix | CSV | TSV |
| --- | --- | --- |
| `Samples` | `samples.csv` | `samples.tsv` |
| `Injections` | `injections.csv` | `injections.tsv` |
| `Channels` | `channels.csv` | `channels.tsv` |
| `Sources` | `sources.csv` | `sources.tsv` |
| `Curves` | `curves.csv` | `curves.tsv` |
| `Points` | `points.csv` | `points.tsv` |
| `Streams` | `streams.csv` | `streams.tsv` |
| `Scans` | `scans.csv` | `scans.tsv` |
| `ScanPoints` | `scan_points.csv` | `scan_points.tsv` |
| `Arrays` | `arrays.csv` | `arrays.tsv` |
| `ArrayAxes` | `array_axes.csv` | `array_axes.tsv` |
| `ArrayPoints` | `array_points.csv` | `array_points.tsv` |

The manifest has exactly `Format`, `Version`, `Delimiter`, then
`{Prefix}-Rows`, `{Prefix}-Columns`, and `{Prefix}-SHA256` for each of those
twelve prefixes. Field order is not significant. Counts are canonical
nonnegative decimal whole numbers within the R integer range; each declared
column count must match its fixed schema. SHA256 values are 64 lowercase
hexadecimal characters and cover exact payload bytes. Hashes detect changed
bytes; they do not authenticate a source.

Every payload has its exact ordered header below. Readers parse all payload
fields as character before semantic reconstruction and reject missing, extra,
reordered, or duplicate columns. The writer emits UTF-8, quoted fields and LF
line endings. Literal `NA` is text in metadata fields. An empty cell denotes
unknown nullable metadata. Numeric codecs use `NA` for missing values as
specified below.

## Tables

```text
Samples: row_index,sample_id
Injections: row_index,injection_id,sample_id
Channels: row_index,channel_id,channel_name
Sources: row_index,source_id,source_name,source_sha256
Curves: row_index,curve_id,injection_id,channel_id,source_id,axis_name,axis_unit,value_name,value_unit
Points: curve_id,point_index,location,value,source_row
Streams: row_index,stream_id,injection_id,channel_id,kind,source_id,time_unit,mz_unit,value_name,value_unit
Scans: row_index,scan_id,stream_id,scan_index,scan_status,retention_time,polarity,ms_level,spectrum_type,source_scan_id,location_type,value_type,point_count
ScanPoints: scan_id,point_index,location,value,source_row,saturated
Arrays: row_index,array_id,stream_id,dim_names_present,dim_units_present,value_type,point_count
ArrayAxes: array_id,axis_index,storage_type,axis_name,axis_unit
ArrayPoints: array_id,point_index,location_1,location_2,value,source_row,saturated
```

`row_index` is a positive contiguous key within each metadata table (`Samples`,
`Injections`, `Channels`, `Sources`, `Curves`, `Streams`, `Scans`, and `Arrays`).
It restores original table row order independently of physical file order.
`point_index` is positive, unique and contiguous within each curve, scan or
array and restores raw point order. `axis_index` contains exactly 1 and 2 for
each array. These keys are separate from the user-declared `scan_index`, which
is positive, unique within its stream and may contain gaps.

The scalar record tables reuse the existing `measure-record` v1 text and
numeric conventions. Metadata row indexes are removed before the existing
record builder is called. Scalar `Points.location` and `Points.value` use the
existing positive-zero canonical codec. Scalar record lineage, relationships,
and stored order are retained.

`Streams.kind` is `ms` or `nd`. Stream, injection, channel, and kind fields are
required. `source_id` and the remaining stream metadata are nullable; a known
source identifier must refer to `record.sources`, while an unknown source stays
unknown. Injection and channel identifiers must refer to the record. ND streams
have missing `time_unit` and `mz_unit`; array axes own their separate
declarations. Unit strings are opaque metadata, without conversion. Multiple
streams may refer to the same injection and channel.

`Scans` describes raw MS records. `scan_status` is `acquired` or `missing`;
missing scans have zero points. `retention_time` is a double and may be missing,
but not NaN or infinite. Polarity is `positive`, `negative`, or unknown. An
optional MS level is a positive integer. `spectrum_type` is `centroid`,
`profile`, or unknown. `location_type` and `value_type` preserve each scan's
integer or double vector storage. Counts may be zero, retaining the distinction
between an acquired empty scan and an explicitly missing scan.

`ScanPoints.location` and `.value` use the nD integer/double codecs. They retain
the original numeric storage, point order, repeated or unsorted m/z values,
signed zero, NA, NaN, and infinities. A `source_row` is a positive parsed
source-record ordinal or unknown; repeated ordinals are allowed. `saturated`
uses `1` for true, `0` for false, and an empty cell for unknown. The mask and
source row vectors are aligned point-by-point.

`Arrays` holds two-dimensional `measure_nd_tbl` payloads. `dim_names_present`
and `dim_units_present` are `1` or `0`, preserving `NULL` separately from a
present vector of unknown values. `value_type` and each axis `storage_type`
are `integer` or `double`. If a dimension vector is absent, all matching
`ArrayAxes` cells must be empty. If present, an empty axis cell means an
unknown value and literal `NA` remains text. Array metadata and numeric storage
are retained per array; grids need not match. `ArrayPoints` preserves raw point
order, repeated coordinates, sparse structure, numeric states, source ordinals,
and tri-state saturation masks. Two dimensions alone do not establish a DAD
interpretation.

## Numeric, key, and reference validation

The integer/double numeric tokens used by scans, arrays, their coordinates, and
double retention time are the existing native nD v1 tokens. Integer `NA` is
allowed; other integer values use canonical signed decimal notation in
`[-2147483647, 2147483647]`. Double tokens are finite decimal values or exactly
`NA`, `NaN`, `Inf`, `-Inf`, and `-0`. Negative decimal zero retains its sign.
The writer uses round-trip finite decimals and `-0` for negative zero. Numeric
columns have no empty-cell encoding.

Optional `ms_level` and acquisition `source_row` ordinals use an empty cell for
unknown and otherwise a strict positive integer. `source_scan_id` is opaque
nullable text and is not an ordinal. Scan and array masks accept only `1`, `0`,
or empty. All descriptor counts must agree with their point rows. Duplicate,
gapped, out-of-range, or orphan row/point/axis keys reject. Foreign keys,
stream kinds, schema enums, numeric types and dimension metadata are validated
before objects are reconstructed.

## Supported attributes, staging, and byte limit

Only the standard core object classes and documented attributes are in scope.
Metadata tables use canonical columns, tibble classes and automatic row names.
Measurement numeric and metadata vectors are attribute-free. Scan measurement
lists and per-point source-row/mask lists are plain unnamed lists. Arrays use
strict standard `measure_nd_tbl` attributes and exactly two dimensions.
Additional subclasses, row labels, named payload vectors, custom attributes, or
unsupported list attributes reject rather than being discarded.

The writer validates the complete object before creating staging files. It
writes beside the destination, checks the staged bundle with the public reader
and compares supported-object fidelity, reserves a new destination exclusively,
copies payloads without overwriting and publishes `manifest.dcf` last. It never
replaces an existing file, directory or dangling link. A failed copy may leave
an incomplete destination without a manifest; readers reject it.

The reader checks selected members are regular nonsymlink files, reads only the
manifest and twelve fixed payload names, and applies `max_bytes` to their actual
cumulative bytes. It verifies hashes before parsing payloads, then checks exact
schemas, counts, types, keys, references and masks before reconstruction.
Unselected directory entries are outside the profile. Ordinary filesystem
checks are not a claim of adversarial stat/open atomicity or source
authentication.

## Capability boundaries

This profile does not define precursor/adduct/ion identity, feature alignment,
MS/MS library matching, profile integration, instrument/method/pump metadata,
executable gradients, arbitrary-dimensional acquisition records, vendor
readers/writers, or a processing graph. Native flat-file support and named
external-reader interoperability are separate capabilities. A scalar record
export cannot retain many-to-one extraction ancestry; preserve raw acquisition
and linked trace output separately when that lineage matters.
