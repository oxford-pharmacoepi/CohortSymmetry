### Input a cdm table and a study period and output a cleaned version of table, so that it can be used in asr
tableCleaning <- function(table, study_time = NULL){
  colChecks(table, c("cohort_definition_id", "cohort_start_date"))
  if (!setequal((table %>% pull(cohort_definition_id) %>% unique()), c(1,2)))
    stop("table doesn't have the right format, cohort_definition_id should have both 1 and 2 and only 1 and 2.")
  if (is.null(study_time)){
    dat <- 
      table %>%
      select(cohort_definition_id, subject_id, cohort_start_date) %>%
      pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>% 
      rename("dateIndexDrug" = `1`, "dateMarkerDrug" = `2`) %>%
      mutate(gap = dateMarkerDrug - dateIndexDrug) %>%
      dplyr::filter(!is.na(gap)) %>%
      dplyr::filter(!gap==0) %>%
      select(-gap) %>%
      collect() %>%
      select(-subject_id)
  }
  else{
    dat <-
      table %>%
      select(cohort_definition_id, subject_id, cohort_start_date) %>%
      pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>% 
      rename("dateIndexDrug" = `1`, "dateMarkerDrug" = `2`) %>%
      mutate(gap = dateMarkerDrug - dateIndexDrug) %>%
      dplyr::filter(!is.na(gap)) %>%
      dplyr::filter(!gap==0) %>%
      dplyr::filter(-study_time<= gap & gap <= study_time) %>%
      select(-gap) %>%
      collect() %>%
      select(-subject_id)
  }
  return(dat)
}

# CI
getConfidenceInterval <- function(table, confidence_interval_level = 0.025){
  colChecks(table, c("marker_first", "index_first"))
  
  counts <- tibble(
    index_first = table %>% pull(index_first) %>% sum(.),
    marker_first = table %>% pull(marker_first) %>% sum(.)
  )
  
  if (counts$index_first == 0 & counts$marker_first == 0){
    counts$lowerCI <- counts$upperCI <- NA
  } else if (counts$index_first == 0){
    counts$index_first <-  0.5
    counts$lowerCI <- qbeta(confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCI <- qbeta(1-confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    
    counts$lowerCI <- counts$lowerCI/(1-counts$lowerCI)
    counts$upperCI <- counts$upperCI/(1-counts$upperCI)
  } else if (counts$marker_first == 0){
    counts$marker_first <-  0.5
    counts$lowerCI <- qbeta(confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCI <- qbeta(1-confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    
    counts$lowerCI <- counts$lowerCI/(1-counts$lowerCI)
    counts$upperCI <- counts$upperCI/(1-counts$upperCI)
  } else {
    counts$lowerCI <- qbeta(confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upperCI <- qbeta(1-confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
    
    counts$lowerCI <- counts$lowerCI/(1-counts$lowerCI)
    counts$upperCI <- counts$upperCI/(1-counts$upperCI)
  }
  return(counts)
}

#Histogram
getHistogram <- function (pssa_output, time_scale = "weeks"){
  colChecks(pssa_output[[1]], c("dateIndexDrug", "dateMarkerDrug"))
  # added in additional columns that calculate gap in days/weeks/months etc
  table <- pssa_output[[1]]
  prep <- table %>%
    mutate(gap_days = as.integer(dateMarkerDrug - dateIndexDrug)) %>%
    mutate(gap_weeks = round((gap_days / 7),2)) %>% 
    mutate(gap_months = round((gap_days / 31),2)) %>% 
    mutate(drug_initiation_order = ifelse(dateMarkerDrug > dateIndexDrug, "Index -> Marker", "Marker -> Index")) 
  # %>%
  #   filter(gap_weeks <= 52) %>%
  #   filter(gap_weeks >= - 52) # saw a paper where they only look at 1 year either side
  
  
  #calculate the number of bins so we have a nice distribution
  if( (nrow(prep)%%2) == 0) {
    bins <- nrow(prep)
  } else {
    bins <- nrow(prep) + 1 # basically add 1 if the number is odd
  }
  
  if(time_scale == "weeks") {
    
    #max and min values for breaks for axis
    max_val <- plyr::round_any(max(prep$gap_weeks), 10, f = ceiling)
    min_val <- plyr::round_any(min(prep$gap_weeks), 10, f = floor)
    
    p <- ggplot(prep, aes(x=gap_weeks, color=drug_initiation_order, fill=drug_initiation_order)) + 
      geom_histogram(bins = bins) +
      geom_hline(yintercept=0, colour="white", size=0.5) + # this removes the green line at the bottom
      geom_vline(xintercept = 0, linewidth = 1, color = "red", linetype ="dashed") +
      #labs(title = paste0("Time difference between the initiation of index and marker drugs"))+
      scale_y_continuous(expand = c(0, 0)) + # this removed the gap between the y axis and bottom on the bars so now they rest flush on the axis
      scale_x_continuous(breaks=seq(min_val, max_val, 8)) + # creates set breaks in your time axis
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Weeks before and after index drug initiation") + ylab("Number of Patients") 
    
    return(p)
    
  } else if(time_scale == "days") {
    
    max_val <- plyr::round_any(max(prep$gap_days), 10, f = ceiling)
    min_val <- plyr::round_any(min(prep$gap_days), 10, f = floor)
    
    p <- ggplot(prep, aes(x=gap_days, color=drug_initiation_order, fill=drug_initiation_order)) + 
      geom_histogram(bins = bins) +
      geom_hline(yintercept=0, colour="white", size=0.5) +
      geom_vline(xintercept = 0, linewidth = 1, color = "red", linetype ="dashed") +
      #labs(title = paste0("Time difference between the initiation of index and marker drugs"))+
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(breaks=seq(min_val, max_val, 60)) + # creates set breaks in your time axis
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Days before and after index drug initiation") + ylab("Number of Patients") 
    
    return(p)
    
  }  else if(time_scale == "months") {
    
    max_val <- plyr::round_any(max(prep$gap_months), 10, f = ceiling)
    min_val <- plyr::round_any(min(prep$gap_months), 10, f = floor)
    
    p <- ggplot(prep, aes(x=gap_months, color=drug_initiation_order, fill=drug_initiation_order)) + 
      geom_histogram(bins = bins) +
      geom_hline(yintercept=0, colour="white", size=0.5) +
      geom_vline(xintercept = 0, linewidth = 1, color = "red", linetype ="dashed") +
      #labs(title = paste0("Time difference between the initiation of index and marker drugs"))+
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(breaks=seq(min_val, max_val, 3)) + # creates set breaks in your time axis
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Months before and after index drug initiation") + ylab("Number of Patients") 
    
    return(p)
    
  } 
}

#generate cohort 
generateDrugCohortPSSA <- function(cdm, index, marker, table_name = "pssa", prior_obs = 365, start_date, end_date){
  index_drug <- list()
  marker_drug <- list()
  
  for (i in (1: length(index))){
    if (index[[i]][2] == "ingredient"){
      index_drug[[i]] <- getDrugIngredientCodes(cdm = cdm, name = index[[i]][1])
    } else {
      index_drug[[i]] <- getATCCodes(cdm = cdm, name = index[[i]][1], level = c(index[[i]][2]))
    }
  }
  
  for (i in (1: length(marker))){
    if (marker[[i]][2] == "ingredient"){
      marker_drug[[i]] <- getDrugIngredientCodes(cdm = cdm, name = marker[[i]][1])
    } else {
      marker_drug[[i]] <- getATCCodes(cdm = cdm, name = marker[[i]][1], level = c(marker[[i]][2]))
    }
  }
  
  conceptSetList <- c()
  for (i in (1:length(index_drug))){
    conceptSetList <- c(conceptSetList, index_drug[[i]])
  }
  
  for (i in (1: length(marker_drug))){
    conceptSetList <- c(conceptSetList, marker_drug[[i]])
  }
  
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = table_name,
    conceptSetList = conceptSetList,
    summariseMode = "FirstEra",
    daysPriorObservation = prior_obs,
    cohortDateRange = as.Date(c(start_date, end_date))
  )
  
  index_length <- 0
  for (i in (1:length(index_drug))){
    index_length <- index_length + length(index_drug[[i]])
  }
  
  cdm[[table_name]] <- cdm[[table_name]] %>%
    collect() %>%
    dplyr::mutate(cohort_definition_id = case_when(cohort_definition_id <= index_length ~ 1,
                                                   cohort_definition_id > index_length ~ 2)) %>% 
    dplyr::mutate(cohort_definition_id = as.integer(cohort_definition_id)) %>%
    group_by(cohort_definition_id, subject_id) %>%
    arrange(cohort_start_date, .by_group =T) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    compute()
  
  raw_table <- cdm[[table_name]] 
  
  return(raw_table)
}

##### getPSSA (complete approach) 
##### either give both index and marker names or 
##### cohort_table (latter most preferably being generated from generateDrugCohortPSSA())
getPSSA <- function(cdm,
                    index, 
                    marker, 
                    cohort_table = NULL, 
                    table_name = "pssa", 
                    study_time = NULL, 
                    confidence_interval_level = 0.025, 
                    prior_obs,
                    start_date,
                    end_date # set both as NA for full 
                    ){
  if (!is.null(cohort_table)){
    colChecks(cohort_table, c("cohort_definition_id", "subject_id", "cohort_start_date"))
    table <- cohort_table
  } else {
    table <- generateDrugCohortPSSA(cdm = cdm, index = index, marker = marker, table_name = table_name, prior_obs = prior_obs, start_date = start_date, end_date = end_date)
  }
  table_cleaned <- tableCleaning(table = table, study_time = study_time)
  csr<-crudeSequenceRatio(summaryTable(table_cleaned))
  asr<-adjustedSequenceRatio(summaryTable(table_cleaned))
  counts <- getConfidenceInterval(summaryTable(table_cleaned))
  
  results <- tibble(name = table_name, 
                    csr = csr, 
                    asr = asr)
  
  results <- cbind(results, counts)
  
  result <- list(table_cleaned, results)
  
  return(result)
}

### Waiting Time Distribution
generateSingleDrugCohort <- function(cdm, drug, table_name = "wtd", start_date, end_date, prior_obs = 365){
  drug_name <- list()
  
  for (i in (1: length(drug))){
    if (drug[[i]][2] == "ingredient"){
      drug_name[[i]] <- getDrugIngredientCodes(cdm = cdm, name = drug[[i]][1])
    } else {
      drug_name[[i]] <- getATCCodes(cdm = cdm, name = drug[[i]][1], level = c(drug[[i]][2]))
    }
  }
  
  conceptSetList <- c()
  for (i in (1:length(drug_name))){
    conceptSetList <- c(conceptSetList, drug_name[[i]])
  }
  
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = table_name,
    conceptSetList = conceptSetList,
    summariseMode = "FirstEra",
    daysPriorObservation = 365,
    cohortDateRange = as.Date(c(start_date, end_date))
  )
  
  raw_table <- cdm[[table_name]] %>% collect()
  
  return(raw_table)
}

getWaitingTimeDistribution <- function(cdm,
                                       drug,
                                       single_drug_cohort = NULL,
                                       table_name = "wtd", 
                                       start_date, 
                                       end_date, 
                                       prior_obs = 365
                                       ){
  if (!is.null(single_drug_cohort)){
    colChecks(single_drug_cohort, c("cohort_definition_id", "subject_id", "cohort_start_date"))
    table <- single_drug_cohort
  } else {
    table <- generateSingleDrugCohort(cdm = cdm, drug = drug, table_name = table_name, start_date = start_date, end_date = end_date, prior_obs = prior_obs)
  }
  table <- table %>% mutate(gap = cohort_start_date - as.Date(start_date))
  n_months <- interval(as.Date(start_date), as.Date(end_date)) %/% months(1)
  
  p <- ggplot(table, aes(x=gap)) + 
    geom_histogram(bins = n_months, color="black") +
    labs(title = paste0("Waiting Time Distribution for the chosen drug(s)")) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Days after the start date") + ylab("Number of Patients") 
return(p)
}

# pssa based on a subset of a condition - requires predefined jsons to be instantiated
getPSSASubset <- function(cdm, index, marker, subset_name, subset_id, cohort_table = NULL, table_name = "pssa", study_time = NULL, confidence_interval_level = 0.025){
  cdm[["subset"]] <- cdm[[subset_name]] %>% filter(cohort_definition_id == subset_id)
  subset_cdm <- cdmSubsetCohort(cdm, "subset")
  subset_result <- getPSSA(cdm = subset_cdm,
                           index = index, 
                           marker = marker,
                           cohort_table = cohort_table, 
                           table_name = table_name, 
                           study_time = study_time, 
                           confidence_interval_level = confidence_interval_level) 
  return(subset_result)
}

getPSSAStrata <- function(cdm,
                          ageGroup, #e.g., list(c(0,50), c(50,150), c(0,150))
                          sex, #e.g., sex = c("Male", "Female", "Both")
                          index,
                          marker,
                          prior_obs =365,
                          start_date,
                          end_date,
                          table_name = "pssa", 
                          study_time = NULL, 
                          confidence_interval_level = 0.025){
  
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(cdm = cdm,
                                                           ageGroup = ageGroup,
                                                           sex = sex)
  strata_results <- list()
  for (i in (1:nrow(cohortSet(cdm$denominator)))){
    subject_ids <- cdm$denominator %>% filter(cohort_definition_id == i) %>% pull(subject_id)
    
    drug_cohort <- generateDrugCohortPSSA(cdm = cdm,
                                          index = index, 
                                          marker = marker,
                                          prior_obs = prior_obs,
                                          start_date = start_date,
                                          end_date = end_date) %>%
      filter(subject_id %in% subject_ids)
    
    cohort_groups <- cohortSet(cdm$denominator) %>% mutate(group = paste(age_group, " ", sex))
    
    strata_results[[cohort_groups %>% filter(cohort_definition_id == i) %>% pull(group)]]<-getPSSA(cohort_table = drug_cohort, study_time = study_time, confidence_interval_level = confidence_interval_level)
    
  }
  return(strata_results)
}



# ### Intake two IDs and generate two cohort sets using capr
# generatePSSACohortDefinitions <- function (DrugId){
#   cohort(
#     entry = entry(drug(cs(descendants(DrugId))),
#                   primaryCriteriaLimit = "First"),
#     exit = exit(endStrategy = fixedExit(index = "startDate", offsetDays = 0L)))
# }


### Credit to Ty - checking if the dataframe has the required columns
colChecks <- function(df, cols) {

  if(!(("data.frame" %in% class(df))|("GeneratedCohortSet" %in% class(df))))
    stop("df input is not a data.frame or a CohortSet object")

  if(!("character" %in% class(cols)))
    stop("col input is not a atomic character vector")

  col_names <- colnames(df)
  cols_found <- cols %in% col_names
  if (!all(cols_found)) {
    stop(paste0(cols[!cols_found], collapse=", "), " not found in input data.frame")
  } else {
    return(TRUE)
  }
}

### A summary table required to compute asr, csr and nsr
# and produces a summary table with one column indicating how many days it has been since the the very first drug
# and the number of cases where the marker was prescribed first and the index was prescribed first.

summaryTable <- function(table, dateIndexDrug = "dateIndexDrug", dateMarkerDrug = "dateMarkerDrug") {
  
  # allocating column names
  column_names <- colnames(table)
  column_names[column_names == dateIndexDrug] <- "dateIndexDrug"
  column_names[column_names == dateMarkerDrug] <- "dateMarkerDrug"
  colnames(table) <- column_names
  
  # creating order, orderBA is TRUE if index is AFTER marker
  table <-
    table %>%
    dplyr::filter((!is.na(dateIndexDrug)) & (!is.na(dateMarkerDrug))) %>%
    mutate(orderBA = dateIndexDrug >= dateMarkerDrug)
  
  # min date of any drug start
  date_start <- min(pull(table, dateIndexDrug), pull(table, dateMarkerDrug))
  
  # lubridate package used, days(1), as_date() and %--%.
  table <-
    table %>%
    mutate(
      date_first = lubridate::as_date(ifelse(orderBA, dateMarkerDrug, dateIndexDrug)), # setting which date is first and which is second
      date_second = lubridate::as_date(ifelse(orderBA, dateIndexDrug, dateMarkerDrug)),
      days_first = as.integer((date_start %--% date_first) / lubridate::days(1)), # gap between the first drug of a person and the first drug of the whole population
      days_second = as.integer((date_first %--% date_second) / lubridate::days(1)) # gap between two drugs of a person
    )
  
  #max number of digits in days_first
  max_dig <- nchar(as.character(max(pull(table, days_first))))
  ch_format <- paste0("%0", max_dig, ".0f")
  
  table <-
    table %>%
    mutate(
      days_first_ch_format = sprintf(ch_format, days_first) # make a column with days_first having the same number of digits - Why?
    ) %>%
    arrange(days_first)
 
  ### final output, a dataframe with four columns, days_first_ch_format, days_first, marker_first and index_first.
  # days_first_ch_format: days_first in ch format
  # days_first: gap between the first drug date of an individual to the first drug date of everyone
  # marker_first: for a given days_first, how many were marker first
  # index_first: for a given days_first, how many were index first
  dat <-
    table %>%
    group_by(days_first_ch_format, days_first) %>%
    summarise(marker_first = sum(orderBA), index_first = sum(!orderBA), .groups = "drop") %>%
    ungroup()
  
  return(dat)
}

### ASR
adjustedSequenceRatio <- function(table) {
  
  return(crudeSequenceRatio(table) / nullSequenceRatio(table))
  
}

### CSR
crudeSequenceRatio <- function(table) {
  
  colChecks(table, c("days_first", "index_first", "marker_first"))
  
  n_index_before_marker <- table %>% pull(index_first) %>% sum(.) #how many occasions are there that index was taken before marker
  n_marker_before_index <- table %>% pull(marker_first) %>% sum(.) #how many occasions are there that index was taken after marker
  
  crudeSequenceRatio <- n_index_before_marker / n_marker_before_index
  
  return(crudeSequenceRatio)
  
}

### NSR (uses summary table, days_first, marker_first and index_first)
nullSequenceRatio <- function(table, restriction = 548) {
  
  colChecks(table, c("days_first", "marker_first", "index_first"))
  
  n_index_before_marker <- table %>% pull(index_first) %>% sum(.)
  n_marker_before_index <- table %>% pull(marker_first) %>% sum(.)
  
  numer <- 0
  denom <- 0
  
  # The case restriction is finite: 
  if (is.finite(restriction)) {
    
    table <-
      table %>%
      mutate(
        marker_cumsum_fwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = FALSE), # For each days_first, look back 548 (restriction) days and see how many marker_first are there
        marker_cumsum_bwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = TRUE), # For each days_first, look forward 548 (restriction days) and see how many marker_first are there
        numerator = index_first * marker_cumsum_fwd, 
        denominator = index_first * (marker_cumsum_bwd + marker_cumsum_fwd - marker_first), # why the minus - mistake?
      )
    
    numer <- table %>% pull(numerator) %>% sum(.)
    denom <- table %>% pull(denominator) %>% sum(.)
    
  } else {
    # The case restriction is infinite: 
    # Hallas 1996 Appendix
    numer <-
      table %>%
      mutate(
        marker_cumsum = n_marker_before_index - cumsum(marker_first),
        numerator = index_first * marker_cumsum
      ) %>%
      pull(numerator)
    
    numer <- sum(numer)
    denom <- n_marker_before_index * n_index_before_marker
    
  }
  
  if (numer < 1)
    warning("NSR numerator is 0, which results in a NSR = 0, proceed with caution")
  
  if (denom < 1){
    warning("NSR denominator is 0, suggesting no Marker Drug -> Index Drug or Index Drug -> Marker Drug events")
    nullSequenceRatio <- NA
  } else {
    a <- numer / denom
    
    nullSequenceRatio <- a / (1 - a)
  }
  
  return(nullSequenceRatio)
  
}
### For each i in 1 through length(x), it finds the minimum j such that x[i]-x[j]<=delta. 
# x needs to be non decreasing. 
# i.e., y[i] = min{j: x[i]-x[j]<=delta}
indexDeltaBackward <- function(x, delta) {

  n <- length(x)
  y <- rep(0, n) #create y which is the same size of x
  
  y[1] <- 1

  j <- 1
  i <- 2

  while (i <= n) {

    if ((x[i] - x[j]) <= delta) {
      y[i] <- j
      i <- i + 1
    } else {
      j <- j + 1
    }

  }

  return(y)

}

### For each i in 1 through length(x), it finds the max j such that x[j]-x[i]<=delta. 
# x needs to be non decreasing.
# i.e., y[i] = max{j: x[j]-x[i]<=delta}
indexDeltaForward <- function(x, delta) {
  
  n <- length(x)
  y <- rep(0, n)
  
  j <- 2
  i <- 1
  
  while (j <= n) {
    
    if ((x[j] - x[i]) > delta) {
      y[i] <- j - 1
      i <- i + 1
    } else {
      j <- j + 1
    }
    
  }
  
  y[i:n] <- n
  
  return(y)
  
}

#### Backward:
# y and t are vectors of the same length
# t is again non decreasing
# this is calculating the sum of y depending on t.
# for ith position, it looks at the ith value of t. Look back in t to find the minimum index 
# of t such that the difference is less than or equal to delta. Say this index is j
# then for the ith position, it is summing y from j to i-1. By default/design the 1st entry is 0.

#### Forward:
# y and t are vectors of the same length
# t is again non decreasing
# this is calculating the sum of y depending on t.
# for ith position, it looks at the ith value of t. Look forward in t to find the maximum index 
# of t such that the difference is less than or equal to delta. Say this index is j
# then for the ith position, it is summing y from i+1 to j. By default/design the last entry is 0.

deltaCumulativeSum <- function(y, t, delta, backwards = TRUE) {
  
  if (is.null(y)) {
    y <- t
  }
  y_cumsum <- cumsum(y)
  
  if (backwards) {
    
    n_y <- length(y)
    prior_cumsum <- c(0, y_cumsum[-n_y])
    bwd_cumsum <- rep(0, n_y)
    t_delta_bwd <- indexDeltaBackward(t, delta) - 1
    look_bwds <- t_delta_bwd > 0
    bwd_cumsum[look_bwds] <- y_cumsum[t_delta_bwd[look_bwds]]
    
    return(prior_cumsum - bwd_cumsum)
    
  } else {
    t_delta_fwd <- indexDeltaForward(t, delta)
    fwd_cumsum <- y_cumsum[t_delta_fwd]
    
    return(fwd_cumsum - y_cumsum)
    
  }
  
}
