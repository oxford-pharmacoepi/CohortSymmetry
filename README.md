
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

### Step 1: getCohortSequence

In order to initiate the calculations, the two cohorts tables need to be
intersected using `getCohortSequence()` function. This process will
filter out the individuals who appeared on both tables according to a
user-specified parameters. This includes `timeGap`, `washoutWindow`,
`indexMarkerGap` and `daysPriorObservation`. Details on these parameters
could be found on the vignette.

``` r
library(CohortSymmetry)
 
cdm <- CohortSymmetry::getCohortSequence(cdm,
                   indexTable = "cohort_index",
                   markerTable = "cohort_marker",
                   name = "amiodarone_levothyroxine",
                   combinationWindow = c(0, Inf))
 
cdm$amiodarone_levothyroxine %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 13
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\Rtmp2Peon4\file1a5425e522bc.duckdb]
#> $ index_id               <int> 1
#> $ marker_id              <int> 1
#> $ subject_id             <int> 2006
#> $ index_date             <date> 2014-01-17
#> $ marker_date            <date> 2017-12-31
#> $ first_date             <date> 2014-01-17
#> $ second_date            <date> 2017-12-31
#> $ days_prior_observation <dbl> 0
#> $ washout_window         <dbl> 0
#> $ index_marker_gap       <dbl> 1e+11
#> $ combination_window     <chr> "(0,99999999999)"
#> $ index_name             <chr> "amiodarone"
#> $ marker_name            <chr> "levothyroxine"
```

### Step 2: getSequenceRatio

To get the sequence ratios, we would need the output of the
getCohortSequence() function. The output of this process contains
cSR(crude sequence ratio), aSR(adjusted sequence ratio) and confidence
intervals.

``` r
res <- CohortSymmetry::getSequenceRatios(cdm = cdm,
                                         outcomeTable = "amiodarone_levothyroxine")
 
res %>% glimpse()
#> Rows: 10
#> Columns: 21
#> $ cdm_name                    <chr> "Synthea synthetic health database", "Synt…
#> $ result_type                 <chr> "sequence_symmetry", "sequence_symmetry", …
#> $ package_name                <chr> "CohortSymmetry", "CohortSymmetry", "Cohor…
#> $ package_version             <chr> "0.0.0.9000", "0.0.0.9000", "0.0.0.9000", …
#> $ group_name_reference        <chr> "index_cohort_name", "index_cohort_name", …
#> $ group_level_reference       <chr> "amiodarone", "amiodarone", "amiodarone", …
#> $ strata_name_reference       <chr> "overall", "overall", "overall", "overall"…
#> $ strata_level_reference      <chr> "overall", "overall", "overall", "overall"…
#> $ group_name_comparator       <chr> "marker_cohort_name", "marker_cohort_name"…
#> $ group_level_comparator      <chr> "levothyroxine", "levothyroxine", "levothy…
#> $ strata_name_comparator      <chr> "overall", "overall", "overall", "overall"…
#> $ strata_level_comparator     <chr> "overall", "overall", "overall", "overall"…
#> $ variable_name               <chr> "first_pharmac", "first_pharmac", "first_p…
#> $ variable_level              <chr> "index", "index", "marker", "marker", "cru…
#> $ estimate_name               <chr> "count", "percentage", "count", "percentag…
#> $ estimate_type               <chr> "integer", "numeric", "integer", "numeric"…
#> $ estimate_value              <chr> "1", "100", "0", "0", "Inf", NA, "0.093492…
#> $ additional_name_reference   <chr> "days_prior_observation and washout_window…
#> $ additional_level_reference  <chr> "0 and 0 and 99999999999 and (0,9999999999…
#> $ additional_name_comparator  <chr> "additional_name_reference", "additional_n…
#> $ additional_level_comparator <chr> "additional_level_reference", "additional_…
```

## References

1.  Pratt N, Chan EW, Choi NK, et al. Prescription sequence symmetry
    analysis: assessing risk, temporality, and consistency for adverse
    drug reactions across datasets in five countries. Pharmacoepidemiol
    Drug Saf. 2015;24(8):858-864.
