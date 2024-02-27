
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortSymmetry <img src="man/figures/CSHex.png" align="right" height="139"/>

<!-- badges: start -->
<!-- badges: end -->

The goal of CohortSymmetry is to carry out the necessary calculations
for Sequence Symmetry Analysis (SSA). It is highly recommended that this
method is tested beforehand against well-known positive benchmarks and
negative benchmarks. Such benchmarks could be found using the paper in
the Reference.

## Installation

You can install the development version of CohortSymmetry from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/CohortSymmetry")
```

## Example

### Create a reference to data in the OMOP CDM format

The CohortSymmetry package is designed to work with data in the OMOP CDM
(Common Data Model) format, so our first step is to create a reference
to the data using the CDMConnector package.

As an example, we will be using Eunomia data set.

``` r
library(CDMConnector)
library(dplyr)
library(DBI)
library(duckdb)
 
db <- DBI::dbConnect(duckdb::duckdb(), 
                     dbdir = CDMConnector::eunomia_dir())
cdm <- cdm_from_con(
  con = db,
  cdm_schema = "main",
  write_schema = "main"
)
```

### Step 0: Instantiate two cohorts in the cdm reference

This will be entirely user’s choice on how to generate such cohorts.
Minimally, this package requires two cohort tables in the cdm reference,
namely the index_cohort and the marker_cohort.

If one wants to generate two drugs cohorts in cdm, DrugUtilisation is
recommended. As an example, amiodarone and levothyroxine are used. This
is a known positive benchmark reported in the literature.<sup>1</sup>

``` r
library(DrugUtilisation)
library(CodelistGenerator)
library(dplyr)
 
index_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "amiodarone")
marker_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "levothyroxine")
 
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort_index",
    conceptSet = index_drug
  )
 
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort_marker",
    conceptSet = marker_drug
  )
```

### Step 1: generateSequenceCohortSet

In order to initiate the calculations, the two cohorts tables need to be
intersected using `generateSequenceCohortSet()` function. This process
will filter out the individuals who appeared on both tables according to
a user-specified parameters. This includes `timeGap`, `washoutWindow`,
`indexMarkerGap` and `daysPriorObservation`. Details on these parameters
could be found on the vignette.

``` r
library(CohortSymmetry)
 
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm,
                   indexTable = "cohort_index",
                   markerTable = "cohort_marker",
                   name = "amiodarone_levothyroxine",
                   combinationWindow = c(0, Inf))
 
cdm$amiodarone_levothyroxine %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 6
#> Database: DuckDB v0.9.2 [eburn@Windows 10 x64:R 4.2.1/C:\Users\eburn\AppData\Local\Temp\RtmpUjUxdr\file36ec1b47952.duckdb]
#> $ cohort_definition_id <int> 1
#> $ subject_id           <int> 2006
#> $ cohort_start_date    <date> 2014-01-17
#> $ cohort_end_date      <date> 2017-12-31
#> $ index_date           <date> 2014-01-17
#> $ marker_date          <date> 2017-12-31
```

### Step 2: getSequenceRatio

To get the sequence ratios, we would need the output of the
generateSequenceCohortSet() function. The output of this process
contains cSR(crude sequence ratio), aSR(adjusted sequence ratio) and
confidence intervals.

``` r
res <- CohortSymmetry::getSequenceRatios(cdm = cdm,
                                         sequenceCohortSet = "amiodarone_levothyroxine")
 
res %>% glimpse()
#> Rows: 10
#> Columns: 15
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ result_type      <chr> "sequence_symmetry", "sequence_symmetry", "sequence_s…
#> $ package_name     <chr> "CohortSymmetry", "CohortSymmetry", "CohortSymmetry",…
#> $ package_version  <chr> "0.0.0.9000", "0.0.0.9000", "0.0.0.9000", "0.0.0.9000…
#> $ group_name       <chr> "index_cohort_name and marker_cohort_name", "index_co…
#> $ group_level      <chr> "amiodarone and levothyroxine", "amiodarone and levot…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "first_pharmac", "first_pharmac", "first_pharmac", "f…
#> $ variable_level   <chr> "index", "index", "marker", "marker", "crude", "adjus…
#> $ estimate_name    <chr> "count", "percentage", "count", "percentage", "point_…
#> $ estimate_type    <chr> "integer", "numeric", "integer", "numeric", "numeric"…
#> $ estimate_value   <chr> "1", "100", "0", "0", "Inf", NA, "0.0934922743508363"…
#> $ additional_name  <chr> "days_prior_observation and washout_window and index_…
#> $ additional_level <chr> "0 and 0 and Inf and (0,Inf) and 95 and 548", "0 and …
```

## References

1.  Pratt N, Chan EW, Choi NK, et al. Prescription sequence symmetry
    analysis: assessing risk, temporality, and consistency for adverse
    drug reactions across datasets in five countries. Pharmacoepidemiol
    Drug Saf. 2015;24(8):858-864.
