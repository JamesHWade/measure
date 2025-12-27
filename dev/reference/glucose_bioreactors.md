# Raman Spectra Bioreactor Data

Kuhn and Johnson (2013) used these two data sets to model the glucose
yeild in large- and small-scale bioreactors:

## Source

Kuhn and Johnson (2020), *Feature Engineering and Selection*, Chapman
and Hall/CRC . <https://bookdown.org/max/FES/> and
<https://github.com/topepo/FES>

## Value

Two tibbles. For each, there are 2,651 columns whose names are numbers
and these are the measured assay values (and the names are the wave
numbers). The numeric column `glucose` has the outcome data, `day` is
the number of days in the bioreactor, the `batch_id` is the reactor
identifier (with "L" for large and "S" for small), and `batch_sample`
that is the ID and the day.

## Details

- Fifteen small-scale (5 liters) bioreactors were seeded with cells and
  were monitored daily for 14 days.

- Three large-scale bioreactors were also seeded with cells from the
  same batch and monitored daily for 14 days.

Samples were collected each day from all bioreactors and glucose was
measured. The goal would be to create models on the data from the more
numerous small-scale bioreactors and then evaluate if these results can
accurately predict what is happening in the large-scale bioreactors.

## Examples

``` r
data(glucose_bioreactors)
dim(bioreactors_small)
#> [1]  210 2655
```
