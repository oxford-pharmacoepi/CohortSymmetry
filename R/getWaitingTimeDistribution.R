### waiting time distribution
#' Waiting Time Distribution
#'
#' @description
#' Classically, waiting time distribution is required to compute null sequence ratio.
#' Though this is not what has been done in this package,
#' Waiting Time Distribution is included for competition.
#' For more information, see Preiss AK, Roughead EE, Pratt NL. Sequence symmetry analysis graphic adjustment for prescribing trends. BMC Medical Research Methodology. 2019 Jul 9;19(1).
#'
#' @param cdm A CDM reference.
#' @param drug The drug(s) that the user wishes to compute the Waiting Time Distribution on.
#' @param table_name Table name in CDM of user's choice.
#' @param start_date Start date that the user would impose so that both index drug(s) and marker drug(s) would be initiated after this day. Set NA if this is not necessary.
#' @param end_date End date that the user would impose so that both index drug(s) and marker drug(s) would be initiated before this day. Set NA if this is not necessary.
#' @param prior_obs Prior observation that the user would like to impose on both index drug(s) and marker drug(s).
#'
#' @return
#' @export
#'
#' @examples
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
