### waiting time distribution
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
