# Read and write native n-dimensional measurement bundles

A native nD bundle is a directory containing a fixed manifest and three
UTF-8 CSV or TSV tables. It preserves the ordered coordinates and
values, dimension labels and units, list member order and labels, and
the storage type of each numeric column. Sparse and repeated coordinate
tuples are retained as observed.

## Usage

``` r
write_measure_nd(x, path, delim = c(",", "\t"))

read_measure_nd(path, max_bytes = 100 * 1024^2)
```

## Arguments

- x:

  A structurally valid `measure_nd_tbl` or `measure_nd_list` using the
  standard core classes and supported attributes.

- path:

  For writing, a new local directory path whose parent already exists.
  For reading, a directory containing a complete bundle.

- delim:

  A comma or tab field delimiter used for all three payloads.

- max_bytes:

  A positive whole-number limit for the actual cumulative bytes read
  from the manifest and the three selected payloads.

## Value

`write_measure_nd()` returns the normalized destination path invisibly.
`read_measure_nd()` returns a `measure_nd_tbl` or `measure_nd_list`
matching the root kind stored in the bundle.

## Details

The bundle format is a bounded numerical interchange profile. It does
not store acquisition provenance, detector or sample identity, signal
units, source row lineage, masks, or transformation history. The writer
accepts only the standard core nD classes with attribute-free numeric
and metadata vectors; unsupported attributes are rejected instead of
being discarded. See the package's `measure-nd-v1.md` profile
specification for the exact table and manifest schema.

The reader selects only the fixed payload names from the manifest. Other
directory entries are ignored and are not protected by the manifest.
Hashes detect payload changes but do not authenticate a bundle.

## Bundle version 1

The fixed payloads are `members.csv`, `axes.csv`, and `points.csv`, or
the corresponding `.tsv` files. The manifest has the exact fields
`Format`, `Version`, `Delimiter`, `Object-Type`, `Dimensions`,
`List-Names`, and `-Rows`, `-Columns`, and `-SHA256` fields for each
payload. The tables store member descriptors (including a list prototype
at index 0), per-member axis metadata and storage types, and ordered
numerical points. Member, axis, and point indices are explicit keys, so
readers may restore order after table row reordering. Double tokens
preserve NA, NaN, infinities and signed zero. The writer emits canonical
finite decimal tokens; the reader also accepts equivalent finite
spellings allowed by the scalar numeric parser.

Dimensions are limited to 2 through 1024. Existing objects with
unsupported attributes, non-automatic row names, or attributed numeric
or metadata vectors are outside this profile. The writer validates the
full object before creating staging files, verifies its staged bundle
with the strict reader, and never replaces an existing destination. A
failed copy can leave an incomplete destination without a manifest; the
reader rejects it.

## Examples

``` r
measurement <- new_measure_nd_tbl(
  location_1 = c(1, 2, 2),
  location_2 = c(254L, 280L, 280L),
  value = c(0, -0, NA_real_),
  dim_names = c("time", "wavelength"),
  dim_units = c("min", "nm")
)
local({
  bundle <- tempfile("measure-nd-")
  on.exit(unlink(bundle, recursive = TRUE), add = TRUE)
  write_measure_nd(measurement, bundle)
  restored <- read_measure_nd(bundle)
  identical(measurement$location_2, restored$location_2)
})
#> [1] TRUE
```
