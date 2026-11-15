# Native nD measurement profile, version 1

This profile stores the supported numerical and dimensional contents of the
existing `measure_nd_tbl` and `measure_nd_list` classes. Its identifier is
`Format: measure-nd`, `Version: 1`. It is separate from the one-dimensional
`measure-record` version-1 profile; neither reader accepts the other format.

The R interface is `write_measure_nd(x, path, delim = c(",", "\t"))` and
`read_measure_nd(path, max_bytes = 100 * 1024^2)`. A reader restores the original
root kind. This profile neither introduces a record class nor changes the
scalar record schema.

## Supported contents

Preserve member and point order, canonical `location_1` through `location_D`
and `value` columns, each column's integer/double storage, numerical states,
and each member's dimension names and units. Collections also preserve names
and their validated zero-row prototype. Supported dimensions are 2 through
1024. Preserve `NULL` metadata separately from explicit all-unknown vectors.
List names may be absent, empty, missing, duplicated or literal `NA`; they are
labels and never inferred acquisition identifiers.

Finite doubles retain their IEEE754 values, including the sign of zero.
Integer missing values, double missing values, NaN and both infinities remain
distinct states. NaN payload bits/sign and R's internal missing-value bit
pattern or character encoding marker are outside this contract. Strings
preserve their UTF-8 content, including quoted separators and multiline text.

Sparse and repeated coordinate tuples, irregular and descending coordinates,
unequal member grids and zero-row members are stored without sorting,
deduplication, interpolation, filling, alignment or unit inference. Nonfinite
coordinates remain numerical states; storing them does not qualify a grid for
analysis. The format contains no implicit Cartesian-product representation.

Only supported core attributes are serialized. Standard table class is
`c("measure_nd_tbl", "measure_tbl", "tbl_df", "tbl", "data.frame")`;
standard collection class is `c("measure_nd_list", "measure_list",
"vctrs_list_of", "vctrs_vctr", "list")`. Table attributes are limited to
`class`, `names`, `row.names`, `ndim`, `dim_order`, `dim_names`, `dim_units`;
collection attributes to `class`, `names`, `ptype`. Row labels must have the
ordinary meaning 1 through the number of rows. Numeric columns and dimension
metadata vectors must be attribute-free. Collection names must be a plain
character vector. Extra subclasses, custom row labels, named numeric vectors
or other attributes reject before staging or output creation, including on
the prototype. This is a bounded interchange profile, not arbitrary R-object
serialization.

## Bundle files and manifest

A bundle is one directory with four selected regular nonsymlink files:
`manifest.dcf` and `members.csv`, `axes.csv`, `points.csv`, or the same three
payload basenames with `.tsv`. Every selected payload uses the declared
separator, UTF-8 and quoted fields; the writer emits LF. Filenames are fixed,
not supplied as manifest paths. Unrelated directory entries are ignored and
are not protected or interpreted by this profile.

The manifest contains exactly these fields, in any order:

| Field | Value |
| --- | --- |
| `Format` | `measure-nd` |
| `Version` | `1` |
| `Delimiter` | `csv` or `tsv` |
| `Object-Type` | `tbl` or `list` |
| `Dimensions` | Integer 2 through 1024 |
| `List-Names` | `absent` or `present` |
| `Members-Rows`, `Members-Columns`, `Members-SHA256` | Actual table shape and lowercase SHA256 |
| `Axes-Rows`, `Axes-Columns`, `Axes-SHA256` | Actual table shape and lowercase SHA256 |
| `Points-Rows`, `Points-Columns`, `Points-SHA256` | Actual table shape and lowercase SHA256 |

Unknown, duplicated or missing fields reject. Counts and indices are unsigned
decimal integers within the supported R integer range; schema-specific lower
bounds apply. Counts must agree with actual parsed rows and references and
cannot authorize an unchecked allocation. `Dimensions` is checked before
constructing dynamic point headers. Hashes cover exact payload bytes and
detect changed bytes, not source authenticity.

### Members

Exact ordered columns:

```
member_index,member_name,member_name_missing,dim_names_present,dim_units_present,value_type,point_count
```

Real members have contiguous indices 1 through N. A table root has exactly
member 1 and `List-Names: absent`. A collection additionally has exactly one
index-0 descriptor for its zero-row prototype, with `point_count=0` and no
point rows. An empty collection still has this prototype descriptor. Its
storage types and dimension metadata are retained, rather than replaced with
a default 2D prototype.

The three flags are exactly `0` or `1`. `value_type` is `integer` or `double`.
For the prototype and a table root, `member_name_missing=1` and the name cell
is blank. The same applies to every real member of an unnamed collection.
For a named collection, missing flag 1 means NA and requires a blank cell;
flag 0 retains the cell verbatim, including an empty string or literal `NA`.
The manifest preserves whether a zero-member collection has explicitly
present zero-length names.

Dimension presence flags distinguish NULL (0) from explicit vectors (1).
Absent metadata require all corresponding axes cells to be blank. A declared
vector can consist entirely of unknown entries.

### Axes

Exact ordered columns:

```
member_index,axis_index,storage_type,axis_name,axis_unit
```

Every member descriptor, including prototype 0, has exactly one row for each
axis 1 through D. `storage_type` is `integer` or `double`. Blank metadata cells
represent unknown entries when the presence flag is 1; literal `NA` is text.
Known labels/units must be nonblank under the core Unicode-whitespace rule;
known dimension names must be unique within a member. Units may repeat.

### Points

Exact ordered columns for the declared D:

```
member_index,point_index,location_1,...,location_D,value
```

Each real member's point indices are positive, unique and exactly 1 through
its declared point count. The prototype has no point rows. Reader restores
members, axes and points by these explicit keys, allowing physical table row
reordering after hashes are recomputed. Orphans, duplicated or missing keys,
point-count disagreement and prototype point rows reject. Coordinate tuples
need not be unique. A member's declared storage types govern its own tokens,
even when another member uses different storage for that column.

## Numeric tokens

Integers use `NA` or canonical signed decimal notation in
[-2147483647, 2147483647]. Reject the reserved -2147483648 representation,
fractions, exponents, leading plus, leading zeros, negative zero, blanks,
nonfinite tokens and out-of-range values. Check range before casting.

Double special tokens are exactly `NA`, `NaN`, `Inf`, `-Inf` and `-0`. Finite
writer tokens use a dot-decimal, 17-significant-digit round-trip format. A
reader also accepts equivalent valid finite decimal spellings, with no padding
whitespace. Every accepted negative decimal zero spelling retains its sign;
the canonical writer spelling is `-0`. Overflow and nonzero decimal
underflow-to-zero reject. JSON nonfinite constants are not involved in the
file format. There are no empty numeric cells in this profile.

## Publication and bounded reading

Writer validates the supported object completely before staging, writes beside
the destination, reads back staged content and checks fidelity, then hashes the
actual bytes. It reserves a new destination exclusively, copies without
overwriting, and publishes the manifest last. Existing files, directories and
dangling-link destinations reject. A failed copy can leave an incomplete
reserved destination without a manifest; readers reject it. Only the writer's
own staging directory is cleaned.

Reader reads only the four selected basenames and applies `max_bytes`
cumulatively to actual bytes, including the manifest. It checks hashes before
parsing payloads, then schemas, shapes, key sets, types and metadata before
constructing objects. Selected symlinks, missing members, directories, FIFOs
and devices reject without opening them as payloads. Root or parent directory
symlinks can resolve normally; hard-linked regular members are regular files.
The byte limit is not a claim of equal parser-memory usage. No adversarial
stat/open race immunity or source authentication is promised.

## Capability boundaries

This is native numerical interchange. It does not encode samples/injections,
acquisition modes, detector or signal quantities, source-row lineage, masks,
method/gradient/pump/column/instrument metadata or transformation history.
Those require their own explicit record contract. A tutorial mapping an
extracted trace to original member/point keys is evidence for that tutorial,
not a new persisted acquisition-lineage API.

Independent reading/writing of this same profile by the companion Python
standard-library qualification is distinct from interoperability with an
established scientific reader. This profile does not implement mzML, MS/ragged
scan records, NPZ, vendor interchange, hosted ownership or physical analytical
qualification.
