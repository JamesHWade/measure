# Record interchange qualification

This small command-line qualification exercises the installed `measure`
package's public record and bundle APIs against fixed, checksummed CSV and TSV
inputs. The fixture covers four curves across three injections, repeated
sample IDs, two channels, quoted multiline text, one descending grid, and
missing signal data. It also writes a seeded 20,008-value binary64 corpus so
Python can check both numeric bundles independently of R's parser.

Run these four commands from the repository root with an installed `measure`
package that includes `measure_record()`, `read_measure_record_delim()`,
`write_measure_record()`, and `read_measure_record()`. The scripts resolve
their fixed inputs relative to their own directory, so they can also be called
by absolute path from another working directory.

```sh
Rscript tools/record-interchange/qualify.R produce /tmp/measure-record-qualification
python3 tools/record-interchange/verify.py /tmp/measure-record-qualification
python3 tools/record-interchange/mutate.py /tmp/measure-record-qualification
Rscript tools/record-interchange/qualify.R reject /tmp/measure-record-qualification
```

`produce` requires a path that does not exist, including a dangling symlink,
and an existing parent directory. It creates the new qualification root and
retains partial output if a check fails. It verifies the four CSV/TSV import
and bundle round trips, then writes both numeric corpus bundles and a
little-endian binary signal oracle emitted from the canonical record. The
20,008-value corpus uses seed `81107` with explicitly recorded
`Mersenne-Twister`, `Inversion`, and `Rejection` RNG kinds. Its
receipt records the installed package path and version, R and dependency
versions, the `session-info.txt` path, and the qualification-tool checkout HEAD
separately. It does not claim that the installed package was built from that
checkout.

`verify.py` reads the four fixture bundles and both corpus bundles using only
the Python standard library. It verifies fixed schemas, manifest counts and
hashes, text and row order, exact fixture values, and all 20,008 binary64
signals in each corpus bundle against that oracle. It writes one new verification receipt and
refuses to overwrite an existing one. `mutate.py` copies seven fixed corruption
cases and one physically reordered but valid bundle into new paths only.
`reject` requires the owned qualification root, checks that all seven
corruptions fail, and confirms the valid reordered points restore by index.
The scripts retain failed outputs and never remove or overwrite existing
qualification data.

Requirements are R with the installed `measure` package and its declared
dependencies, plus Python 3.10 or newer with its standard library. No
`devtools::load_all()`, network access, Python package installation, or source
checkout install is needed.

The fixtures are synthetic software checks for this fixed scalar-record
contract and these CSV/TSV/native bundles. They do not qualify instruments,
hosts, vendor formats, multidimensional detector data, physical measurements,
or optimization. The CSV/TSV files are faithful machine interchange and make
no spreadsheet-safety claim.
