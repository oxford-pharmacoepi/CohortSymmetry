
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
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\Rtmp0MfAeN\file3a0c2a6c61fb.duckdb]
#> $ cohort_definition_id <int> 1
#> $ subject_id           <int> 2006
#> $ cohort_start_date    <date> 2014-01-17
#> $ cohort_end_date      <date> 2017-12-31
#> $ index_date           <date> 2014-01-17
#> $ marker_date          <date> 2017-12-31
```

### Step 2: summariseSequenceRatio

To get the sequence ratios, we would need the output of the
generateSequenceCohortSet() function. The output of this process
contains cSR(crude sequence ratio), aSR(adjusted sequence ratio) and
confidence intervals.

``` r
res <- CohortSymmetry::summariseSequenceRatio(cdm = cdm,
                                         sequenceCohortSet = "amiodarone_levothyroxine")
#> -- 1 combination of 1 had index always before marker
#> Joining with `by = join_by(days_prior_observation, washout_window, index_marker_gap, combination_window, confidence_interval, moving_average_restriction, cdm_name)`
#> ! The following column type were changed:
#> • result_id: from character to integer
 
res %>% glimpse()
#> Rows: 16
#> Columns: 16
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ result_type      <chr> "sequence_ratios", "sequence_ratios", "sequence_ratio…
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
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

### Step 3: visualise the results

The user could then visualise their results using a wide array of
provided tools.

``` r
nice_results <- CohortSymmetry::tableSequenceRatios(result = res)
#> Joining with `by = join_by(estimate_name)`

nice_results
```

<div id="tpcmoakrfj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tpcmoakrfj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#tpcmoakrfj thead, #tpcmoakrfj tbody, #tpcmoakrfj tfoot, #tpcmoakrfj tr, #tpcmoakrfj td, #tpcmoakrfj th {
  border-style: none;
}
&#10;#tpcmoakrfj p {
  margin: 0;
  padding: 0;
}
&#10;#tpcmoakrfj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#tpcmoakrfj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#tpcmoakrfj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#tpcmoakrfj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#tpcmoakrfj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#tpcmoakrfj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#tpcmoakrfj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#tpcmoakrfj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#tpcmoakrfj .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#tpcmoakrfj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#tpcmoakrfj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#tpcmoakrfj .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#tpcmoakrfj .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#tpcmoakrfj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#tpcmoakrfj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tpcmoakrfj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#tpcmoakrfj .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#tpcmoakrfj .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#tpcmoakrfj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tpcmoakrfj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#tpcmoakrfj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tpcmoakrfj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#tpcmoakrfj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tpcmoakrfj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#tpcmoakrfj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tpcmoakrfj .gt_left {
  text-align: left;
}
&#10;#tpcmoakrfj .gt_center {
  text-align: center;
}
&#10;#tpcmoakrfj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#tpcmoakrfj .gt_font_normal {
  font-weight: normal;
}
&#10;#tpcmoakrfj .gt_font_bold {
  font-weight: bold;
}
&#10;#tpcmoakrfj .gt_font_italic {
  font-style: italic;
}
&#10;#tpcmoakrfj .gt_super {
  font-size: 65%;
}
&#10;#tpcmoakrfj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#tpcmoakrfj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#tpcmoakrfj .gt_indent_1 {
  text-indent: 5px;
}
&#10;#tpcmoakrfj .gt_indent_2 {
  text-indent: 10px;
}
&#10;#tpcmoakrfj .gt_indent_3 {
  text-indent: 15px;
}
&#10;#tpcmoakrfj .gt_indent_4 {
  text-indent: 20px;
}
&#10;#tpcmoakrfj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="Database name">Database name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="Index">Index</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="Marker">Marker</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="Study population">Study population</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="Index first, N (%)">Index first, N (%)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="Marker first, N (%)">Marker first, N (%)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="CSR (95% CI)">CSR (95% CI)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="ASR (95% CI)">ASR (95% CI)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Database name" class="gt_row gt_left" style="text-align: left;">Synthea synthetic health database</td>
<td headers="Index" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Amiodarone</td>
<td headers="Marker" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Levothyroxine</td>
<td headers="Study population" class="gt_row gt_right" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">1</td>
<td headers="Index first, N (%)" class="gt_row gt_right" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">1 (100.0 %)</td>
<td headers="Marker first, N (%)" class="gt_row gt_right" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">0 (0.0 %)</td>
<td headers="CSR (95% CI)" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Inf (0.09 - 58.75)</td>
<td headers="ASR (95% CI)" class="gt_row gt_left" style="text-align: left;">NA (NA - NA)</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r

CDMConnector::cdmDisconnect(cdm = cdm)
```

### Disconnect from the cdm database connection

``` r

CDMConnector::cdmDisconnect(cdm = cdm)
```

## References

1.  Pratt N, Chan EW, Choi NK, et al. Prescription sequence symmetry
    analysis: assessing risk, temporality, and consistency for adverse
    drug reactions across datasets in five countries. Pharmacoepidemiol
    Drug Saf. 2015;24(8):858-864.
