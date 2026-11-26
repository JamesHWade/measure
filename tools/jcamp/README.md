# Infrared JCAMP-DX interoperability

From the repository root, install the candidate `measure` and CRAN's
`readJDX` 0.6.4, then run:

```sh
Rscript tools/jcamp/qualify.R
```

The script downloads two [NIST Chemistry WebBook ethanol
spectra](https://webbook.nist.gov/cgi/cbook.cgi?ID=C64175&Mask=80) to the R
session's temporary directory. To use local files, pass their paths in
[manifest order](fixture.json). Both paths are checked against the same
SHA-256 hashes. The original spectra and their attribution remain at NIST.

The gas-phase absorbance spectrum has 880 points on a four-wavenumber grid.
The digitized transmittance spectrum has 2,733 points in micrometers, with
rounded checkpoints and a nominal spacing. The script compares every decoded
coordinate and ordinate against [readJDX](https://bryanhanson.github.io/readJDX/)
and checks one independently specified pair for each source. It also sends the
absorbance spectrum back through readJDX as explicit XY pairs with the source
title, origin, and owner.

The last check exports a descending irregular grid with an interior missing
ordinate from the package's simulated film. readJDX 0.6.4 emits numeric
coercion warnings for the JCAMP `?` marker. In the row written as `1670,?`, readJDX returns
`NA` for both columns. The native reader retains the wavenumber. The script verifies
this difference and compares every finite pair. Limits for these fixtures are absolute
errors of 1e-11 in the declared coordinate units and 1e-14 in signal units.
