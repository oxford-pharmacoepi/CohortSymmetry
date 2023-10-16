getWaitingTimeDistribution <- function(cdm,
                                       drug,
                                       table_name = "wtd",
                                       start_date,
                                       end_date,
                                       prior_obs = 365
){
  table <- generateSingleDrugCohort(cdm = cdm, drug = drug, table_name = table_name, start_date = start_date, end_date = end_date, prior_obs = prior_obs)
  table <- table %>% dplyr::mutate(gap = .data$cohort_start_date - as.Date(start_date))
  n_months <- lubridate::interval(as.Date(start_date), as.Date(end_date)) %/% months(1)

  p <- ggplot2::ggplot(table, ggplot2::aes(x=.data$gap)) +
    ggplot2::geom_histogram(bins = n_months, color="black") +
    ggplot2::labs(title = paste0("Waiting Time Distribution for the chosen drug(s)")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
          panel.background = ggplot2::element_blank() ,
          axis.line = ggplot2::element_line(colour = "black", size = 0.6) ,
          panel.grid.major = ggplot2::element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = ggplot2::element_rect(fill = "transparent", colour = "transparent")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::xlab("Days after the start date") + ggplot2::ylab("Number of Patients")
  return(p)
}
