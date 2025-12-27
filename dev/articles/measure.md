# measure

``` r
library(measure)
#> Loading required package: recipes
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: 'recipes'
#> The following object is masked from 'package:stats':
#> 
#>     step
```

To understand measure, we need to re-introduce some concepts from
[recipes](https://recipes.tidymodels.org/articles/recipes.html), namely
**variables** and **roles**. Variables refer the the data columnn in a
data frame or tibble that contain raw data. Roles define how we will use
these variables in our model. For analytical measurements, we need more
than the roles that recipes provides. Recipes was designed to be
extensible in exactly this way.

Starting from Max Kuhn’s
[suggestions](https://community.rstudio.com/t/a-recipes-extension-for-spectrometry-spectroscopy-chromatographic-or-similar-data/143714/2),
additional roles to be included in this package are:

- **measurment variables**: raw data from an instrument (e.g., time,
  absorbance, millivolts, volume, etc.)
- **sample identifies**: define the subset of the data that we are
  interested in (e.g., sample, condition, replicate, day, etc.)
- **experimental conditions**: define the conditions under which the
  measurement was made (e.g., temperature, pH, reagents, column type,
  flow rate, etc.)

Our challenge is to strike the right balance between a flexibile package
and a useful one.

## Scope of Data Sources

Analytical measurement systems are as varied as the applications they
are used in. The field is necessarily broad and growing. The American
Chemical Society [defines the
field](https://www.acs.org/content/acs/en/careers/chemical-sciences/areas/analytical-chemistry.html)
as:

> the science of obtaining, processing, and communicating information
> about the composition and structure of matter. In other words, it is
> the art and science of determining what matter is and how much of it
> exists.

Common analytical techniques include chromatography, optical
spectroscopy, mass spectrometry, microscopy, and many others. The
initial scope of [measure](https://jameshwade.github.io/measure) is on
common techniques that can represent data in a tibble or series of
tibbles.

Researchers who are experts in performing these types of measurements
refer their equipment as instruments. However, instrumental variables
already have a meaning in the statistics literature, and so we will
avoid that terminology.

## Data Sources

There are a number of data sets in
[modeldata](https://modeldata.tidymodels.org) and
[prospectr](https://github.com/l-ramirez-lopez/prospectr) packages.
These include
[`modeldata::meats`](https://modeldata.tidymodels.org/reference/meats.html)
and
[`prospectr::NIRsoil`](https://rdrr.io/pkg/prospectr/man/NIRsoil.html).

Mendeley Data offers data inlcuded as part of various publications. A
few possibilities include:

- [Machine Learning of MS
  dataset](https://data.mendeley.com/datasets/2rcc8488hx)
- [Data for: A Sensitive Quantitative Analysis of Abiotically
  Synthesized Short Homopeptides using Ultraperformance Liquid
  Chromatography and Time-of-Flight Mass
  Spectrometry](https://data.mendeley.com/datasets/msy7k3myrx)

There are also some repositories that might be of use:

- [Crystallography Open Database](https://www.crystallography.net/cod/)
- [NMRShiftDB](https://nmrshiftdb.nmr.uni-koeln.de/nmrshiftdb/media-type/html/user/anon/page/default.psml/js_pane/P-Help;jsessionid=8DC5FC88F23A161E2276E00C85C184CA)
- [Spectral Database for Organic Compounds,
  SDBS](https://sdbs.db.aist.go.jp/sdbs/cgi-bin/cre_index.cgi?lang=eng)
- [NIST Chemistry WebBook](https://webbook.nist.gov/chemistry/#Search)
