############################# Age stratification ####################################
ageGroup = list(c(0, 20), c(21, 40), c(41, 60), c(61, 80))

ageGrDf <- data.frame(do.call(rbind, ageGroup)) %>%
  dplyr::mutate(age_group = paste0(.data$X1, ";", .data$X2))

cdm$cohort1_temp <-
cdm$cohort1 %>%
  PatientProfiles::addAge(cdm = cdm, indexDate = "cohort_start_date", ageGroup = ageGroup) %>%
  dplyr::group_by(cohort_definition_id, subject_id, age_group) %>%
  dbplyr::window_order(cohort_start_date) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::ungroup() %>%
  dbplyr::window_order() %>%
  dplyr::compute()

cdm$cohort2_temp <-
cdm$cohort2 %>%
  PatientProfiles::addAge(cdm = cdm, indexDate = "cohort_start_date", ageGroup = ageGroup) %>%
  dplyr::group_by(cohort_definition_id, subject_id, age_group) %>%
  dbplyr::window_order(cohort_start_date) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::ungroup() %>%
  dbplyr::window_order() %>%
  dplyr::compute()

group1 <- cdm$cohort1_temp %>% dplyr::select(age_group) %>% dplyr::distinct() %>% dplyr::pull()
group2 <- cdm$cohort2_temp %>% dplyr::select(age_group) %>% dplyr::distinct() %>% dplyr::pull()

groups <- intersect(group1, group2)

results <- list()
for (group in (groups)){
  cdm$cohort1_temp2 <- cdm$cohort1_temp %>%
    dplyr::filter(age_group == group)
  cdm$cohort2_temp2 <- cdm$cohort2_temp %>%
    dplyr::filter(age_group == group)
  cdm <- getCohortSequence(cdm = cdm,
                                        indexTable = "cohort1_temp2",
                                        markerTable = "cohort2_temp2")
  results[[group]] <- getSequenceRatios(cdm, "joined_cohorts") %>%
    dplyr::mutate(age_group = group)
}

results <- Reduce(dplyr::union_all, results)
#######################################################################################

############################# Sex stratification ####################################
sex  = c("Both", "Male", "Female")
cdm$cohort1_temp <-
  cdm$cohort1 %>%
  PatientProfiles::addSex(cdm = cdm) %>%
  dplyr::group_by(cohort_definition_id, subject_id, sex) %>%
  dbplyr::window_order(cohort_start_date) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::ungroup() %>%
  dbplyr::window_order() %>%
  dplyr::compute()

cdm$cohort2_temp <-
  cdm$cohort2 %>%
  PatientProfiles::addSex(cdm = cdm) %>%
  dplyr::group_by(cohort_definition_id, subject_id, sex) %>%
  dbplyr::window_order(cohort_start_date) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::ungroup() %>%
  dbplyr::window_order() %>%
  dplyr::compute()

results <- list()

if("Both" %in% sex){
  cdm <- getCohortSequence(cdm = cdm,
                           indexTable = "cohort1",
                           markerTable = "cohort2")
results[["Both"]] <- getSequenceRatios(cdm = cdm,
                                       outcomeTable = "joined_cohorts") %>%
  dplyr::mutate(sex = "Both")
}

if("Male" %in% sex){
  cdm$cohort1_temp2 <- cdm$cohort1_temp %>%
    dplyr::filter(sex == "Male")
  cdm$cohort2_temp2 <- cdm$cohort2_temp %>%
    dplyr::filter(sex == "Male")
  cdm <- getCohortSequence(cdm = cdm,
                           indexTable = "cohort1_temp2",
                           markerTable = "cohort2_temp2")
  results[["Male"]] <- getSequenceRatios(cdm, "joined_cohorts") %>%
    dplyr::mutate(sex = "Male")
}

if("Male" %in% sex){
  cdm$cohort1_temp2 <- cdm$cohort1_temp %>%
    dplyr::filter(sex == "Male")
  cdm$cohort2_temp2 <- cdm$cohort2_temp %>%
    dplyr::filter(sex == "Male")
  cdm <- getCohortSequence(cdm = cdm,
                           indexTable = "cohort1_temp2",
                           markerTable = "cohort2_temp2")
  results[["Male"]] <- getSequenceRatios(cdm, "joined_cohorts") %>%
    dplyr::mutate(sex = "Male")
}

if("Female" %in% sex){
  cdm$cohort1_temp2 <- cdm$cohort1_temp %>%
    dplyr::filter(sex == "Female")
  cdm$cohort2_temp2 <- cdm$cohort2_temp %>%
    dplyr::filter(sex == "Female")
  cdm <- getCohortSequence(cdm = cdm,
                           indexTable = "cohort1_temp2",
                           markerTable = "cohort2_temp2")
  results[["Female"]] <- getSequenceRatios(cdm, "joined_cohorts") %>%
    dplyr::mutate(sex = "Female")
}

results <- Reduce(dplyr::union_all, results)
#######################################################################################
### previous CohortSymmetry getHistogram()
getHistogram <- function (pssa_output, time_scale = "weeks"){
  # added in additional columns that calculate gap in days/weeks/months etc
  table <- pssa_output[[1]]
  prep <- table %>%
    dplyr::mutate(gap_days = as.integer(.data$dateMarkerDrug - .data$dateIndexDrug)) %>%
    dplyr::mutate(gap_weeks = round((.data$gap_days / 7),2)) %>%
    dplyr::mutate(gap_months = round((.data$gap_days / 31),2)) %>%
    dplyr::mutate(drug_initiation_order = ifelse(.data$dateMarkerDrug > .data$dateIndexDrug, "Index -> Marker", "Marker -> Index"))
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

    p <- ggplot2::ggplot(prep, ggplot2::aes(x=.data$gap_weeks, color=.data$drug_initiation_order, fill=.data$drug_initiation_order)) +
      ggplot2::geom_histogram(bins = bins) +
      ggplot2::geom_hline(yintercept = 0, colour="white", size=0.5) + # this removes the green line at the bottom
      ggplot2::geom_vline(xintercept = 0, linewidth = 1, color = "red", linetype ="dashed") +
      #labs(title = paste0("Time difference between the initiation of index and marker drugs"))+
      ggplot2::scale_y_continuous(expand = c(0, 0)) + # this removed the gap between the y axis and bottom on the bars so now they rest flush on the axis
      ggplot2::scale_x_continuous(breaks=seq(min_val, max_val, 8)) + # creates set breaks in your time axis
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                     panel.background = ggplot2::element_blank() ,
                     axis.line = ggplot2::element_line(colour = "black", size = 0.6) ,
                     panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2, linetype = "dashed"),
                     legend.key = ggplot2::element_rect(fill = "transparent", colour = "transparent")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::xlab("Weeks before and after index drug initiation") + ggplot2::ylab("Number of Patients")

    return(p)

  } else if(time_scale == "days") {

    max_val <- plyr::round_any(max(prep$gap_days), 10, f = ceiling)
    min_val <- plyr::round_any(min(prep$gap_days), 10, f = floor)

    p <- ggplot2::ggplot(prep, ggplot2::aes(x=.data$gap_days, color=.data$drug_initiation_order, fill=.data$drug_initiation_order)) +
      ggplot2::geom_histogram(bins = bins) +
      ggplot2::geom_hline(yintercept=0, colour="white", size=0.5) +
      ggplot2::geom_vline(xintercept = 0, linewidth = 1, color = "red", linetype ="dashed") +
      #labs(title = paste0("Time difference between the initiation of index and marker drugs"))+
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::scale_x_continuous(breaks=seq(min_val, max_val, 60)) + # creates set breaks in your time axis
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                     panel.background = ggplot2::element_blank() ,
                     axis.line = ggplot2::element_line(colour = "black", size = 0.6) ,
                     panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2, linetype = "dashed"),
                     legend.key = ggplot2::element_rect(fill = "transparent", colour = "transparent")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::xlab("Days before and after index drug initiation") + ggplot2::ylab("Number of Patients")

    return(p)

  }  else if(time_scale == "months") {

    max_val <- plyr::round_any(max(prep$gap_months), 10, f = ceiling)
    min_val <- plyr::round_any(min(prep$gap_months), 10, f = floor)

    p <- ggplot2::ggplot(prep, ggplot2::aes(x=.data$gap_months, color=.data$drug_initiation_order, fill=.data$drug_initiation_order)) +
      ggplot2::geom_histogram(bins = bins) +
      ggplot2::geom_hline(yintercept=0, colour="white", size=0.5) +
      ggplot2::geom_vline(xintercept = 0, linewidth = 1, color = "red", linetype ="dashed") +
      #labs(title = paste0("Time difference between the initiation of index and marker drugs"))+
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::scale_x_continuous(breaks=seq(min_val, max_val, 3)) + # creates set breaks in your time axis
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
                     panel.background = ggplot2::element_blank() ,
                     axis.line = ggplot2::element_line(colour = "black", size = 0.6) ,
                     panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2, linetype = "dashed"),
                     legend.key = ggplot2::element_rect(fill = "transparent", colour = "transparent")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::xlab("Months before and after index drug initiation") + ggplot2::ylab("Number of Patients")

    return(p)

  }
}
