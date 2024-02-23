
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

This is a basic example which carries out the necessary calculations for
Sequence Symmetry Analysis:

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
in other words, the columns of these tables should contain
cohort_definition_id, subject_id, cohort_start_date and cohort_end_date.

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
 
cdm$cohort_index %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpCmACiv\file2c2c75a2e80.duckdb]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <int> 1101, 1169, 1605, 1632, 1635, 4668, 123, 1015, 10…
#> $ cohort_start_date    <date> 1982-08-22, 2012-02-20, 2019-05-12, 1988-11-13, …
#> $ cohort_end_date      <date> 1982-08-22, 2012-02-20, 2019-05-12, 1988-11-14, …
 
cdm$cohort_marker %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpCmACiv\file2c2c75a2e80.duckdb]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <int> 2909, 1079, 4532, 1537, 3224, 4960, 19, 2829, 430…
#> $ cohort_start_date    <date> 1999-02-12, 2016-03-05, 1964-04-03, 2010-05-31, …
#> $ cohort_end_date      <date> 1999-02-12, 2016-03-05, 1964-04-03, 2010-05-31, …
```

### Step 1: getCohortSequence

In order to initiate the calculations, the two cohorts tables need to be
combined together. The first cohort is considered to be the index cohort
while the second is the marker.

This process will filter out the individuals who appeared on both tables
according to a user-specified combinationWindow. The default is c(0,
365), meaning the gap between two inititaions of the drugs should be
more than 0 days but less than or equal to 365 days.

There are further parameters that could be specified by the users,
including DateRange, indexId, markerId, daysPriorObservation,
washoutWindow and indexMarkerGap. Details on these parameters could be
found on the vignette.

``` r
library(CohortSymmetry)
 
cdm <- CohortSymmetry::getCohortSequence(cdm,
                   indexTable ="cohort_index",
                   markerTable = "cohort_marker",
                   name = "amiodarone_levothyroxine",
                   combinationWindow = c(0, Inf))
 
cdm$amiodarone_levothyroxine %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 13
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpCmACiv\file2c2c75a2e80.duckdb]
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

cdm$amiodarone_levothyroxine
#> # Source:   table<amiodarone_levothyroxine> [1 x 13]
#> # Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpCmACiv\file2c2c75a2e80.duckdb]
#>   index_id marker_id subject_id index_date marker_date first_date second_date
#>      <int>     <int>      <int> <date>     <date>      <date>     <date>     
#> 1        1         1       2006 2014-01-17 2017-12-31  2014-01-17 2017-12-31 
#> # ℹ 6 more variables: days_prior_observation <dbl>, washout_window <dbl>,
#> #   index_marker_gap <dbl>, combination_window <chr>, index_name <chr>,
#> #   marker_name <chr>
```

### Step 2: getSequenceRatio

To get the sequence ratios, we would need the output of the
getCohortSequence() function. The output of this process contains
cSR(crude sequence ratio), aSR(adjusted sequence ratio) and confidence
intervals.

``` r
library(CohortSymmetry)
 
res <- CohortSymmetry::getSequenceRatios(cdm = cdm,
                                         outcomeTable = "amiodarone_levothyroxine")
 
res
#> # A tibble: 10 × 21
#>    cdm_name        result_type package_name package_version group_name_reference
#>    <chr>           <chr>       <chr>        <chr>           <chr>               
#>  1 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  2 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  3 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  4 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  5 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  6 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  7 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  8 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#>  9 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#> 10 Synthea synthe… sequence_s… CohortSymme… 0.0.0.9000      index_cohort_name   
#> # ℹ 16 more variables: group_level_reference <chr>,
#> #   strata_name_reference <chr>, strata_level_reference <chr>,
#> #   group_name_comparator <chr>, group_level_comparator <chr>,
#> #   strata_name_comparator <chr>, strata_level_comparator <chr>,
#> #   variable_name <chr>, variable_level <chr>, estimate_name <chr>,
#> #   estimate_type <chr>, estimate_value <chr>, additional_name_reference <chr>,
#> #   additional_level_reference <chr>, additional_name_comparator <chr>, …
```

## References

1.  Pratt N, Chan EW, Choi NK, et al. Prescription sequence symmetry
    analysis: assessing risk, temporality, and consistency for adverse
    drug reactions across datasets in five countries. Pharmacoepidemiol
    Drug Saf. 2015;24(8):858-864.
