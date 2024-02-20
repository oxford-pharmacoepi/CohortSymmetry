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
