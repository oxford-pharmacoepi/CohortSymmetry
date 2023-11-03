
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortSymmetry

<!-- badges: start -->
<!-- badges: end -->

The goal of CohortSymmetry is to carry out the necessary calculations
for Sequence Symmetry Analysis (SSA).

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

As an example, we will be utilising the Eunomia data set.

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
Minimally, this package requires two tables in the cdm reference to be
cohort tables; in other words, the columns of these tables should
contain cohort_definition_id, subject_id, cohort_start_date and
cohort_end_date.

If one wants to generate two drugs cohorts in cdm, DrugUtilisation is
recommended. As an example, amiodarone and levothyroxine are used.
Famously, this is a known positive control in [Pratt et
al.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4690514/)

``` r
library(DrugUtilisation)
library(CodelistGenerator)
library(dplyr)
 
index_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "amiodarone")
marker_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "levothyroxine")
 
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort1",
    conceptSetList = index_drug,
    summariseMode = "FirstEra"
  )
 
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort2",
    conceptSetList = marker_drug,
    summariseMode = "FirstEra"
  )
 
cdm$cohort1 %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpCUs87o\file555843c829a8.duckdb]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <dbl> 1101, 1169, 1605, 1632, 1635, 4668, 123, 1015, 10…
#> $ cohort_start_date    <date> 1982-08-22, 2012-02-20, 2019-05-12, 1988-11-13, …
#> $ cohort_end_date      <date> 1982-08-22, 2012-02-20, 2019-05-12, 1988-11-14, …
 
cdm$cohort2 %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpCUs87o\file555843c829a8.duckdb]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <dbl> 2909, 1079, 4532, 1537, 3224, 4960, 19, 2829, 430…
#> $ cohort_start_date    <date> 1999-02-12, 2016-03-05, 1964-04-03, 2010-05-31, …
#> $ cohort_end_date      <date> 1999-02-12, 2016-03-05, 1964-04-03, 2010-05-31, …
```

### Step 1: getCohortSequence

In order to initiate the calculations, the two cohorts tables need to be
combined together. The first cohort is considered to be the index cohort
while the second is the marker.

This process will filter out the individuals who appeared on both tables
according to a user-specified timeGap (default 365, it can also be
changed to Inf if no such restriction is preferred).

The user also has the freedom to specify more than 1 IDs from either
table that can be considered for this process. More concretely, if an
user specifies m IDs from the first cohort and n IDs from the second
cohort, this process will output mn combinations. If IDs are not
specified then all IDs will be considered.

``` r
library(CohortSymmetry)
 
cdm <- CohortSymmetry::getCohortSequence(cdm,
                   indexTable ="cohort1",
                   markerTable = "cohort2",
                   timeGap = Inf)
 
cdm$joined_cohorts %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpCUs87o\file555843c829a8.duckdb]
#> $ subject_id  <dbl> 2006
#> $ index_id    <int> 1
#> $ marker_id   <int> 1
#> $ index_date  <date> 2014-01-17
#> $ marker_date <date> 2017-12-31
#> $ first_date  <date> 2014-01-17
#> $ time_gap    <chr> "Infinity"
#> $ cdm_name    <chr> "Synthea synthetic health database"
```
