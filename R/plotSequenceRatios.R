#' A plot for the sequence ratios.
#'
#' @description
#' It provides a ggplot of the sequence ratios of index and marker cohorts.
#'
#' @param result Table output from summariseSequenceRatios.
#' @param onlyaSR If the only SR to be plotted is the adjusted SR.
#' @param plotTitle Title of the plot, if NULL no title will be plotted.
#' @param labs Axis labels for the plot.
#' @param colours Colours for both parts of the plot, pre- and post- time 0.
#'
#' @return A plot for the sequence ratios of index and marker cohorts.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSymmetry)
#'
#' cdm <- mockCohortSymmetry()
#' cdm <- generateSequenceCohortSet(cdm = cdm,
#'                                  indexTable = "cohort_1",
#'                                  markerTable = "cohort_2",
#'                                  name = "joined_cohort")
#' sequence_ratio <- summariseSequenceRatios(cohort = cdm$joined_cohort,
#'                                           minCellCount = 0)
#' plotSequenceRatios(result = sequence_ratio)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotSequenceRatios <- function(result,
                               onlyaSR = FALSE,
                               plotTitle = NULL,
                               labs = c("SR", "Drug Pairs"),
                               colours = c("red", "blue")
                               ) {
  # checks
  checkInputPlotSequenceRatios(result = result,
                               onlyaSR = onlyaSR,
                               plotTitle = plotTitle,
                               labs = labs,
                               colours = colours)

  result <- result |>
    visOmopResults::splitGroup()

  sr_tidy <- result |>
    visOmopResults::filterSettings(.data$result_type == "sequence_ratios") |>
    dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
    visOmopResults::splitAdditional() |>
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
    dplyr::mutate(group = paste0(.data$index_cohort_name, " -> ", .data$marker_cohort_name)) |>
    dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
    dplyr::mutate(
      point_estimate = as.numeric(.data$point_estimate),
      lower_CI = as.numeric(.data$lower_CI),
      upper_CI = as.numeric(.data$upper_CI),
      variable_name = as.factor(.data$variable_name)
    ) |>
    dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|.data$group) |>
    dplyr::rename(
      !!labs[1] := "point_estimate",
      !!labs[2] := "group"
    )

  if(onlyaSR) {
    sr_tidy <- sr_tidy |>
      dplyr::filter(.data$variable_name == "adjusted")
    colours = c("adjusted" = colours[1])
  } else {
    sr_tidy <- sr_tidy |>
      dplyr::filter(.data$variable_name == "adjusted"|.data$variable_name == "crude")
    colours = c("crude" = colours[1], "adjusted" = colours[2])
  }

  facet_wrap_vars <- colnames(sr_tidy)[! colnames(sr_tidy) %in% c(labs[2], labs[1], "lower_CI", "upper_CI", "variable_name", "count", "percentage", "variable_name", "estimate_type")]
  for(i in facet_wrap_vars) {
    sr_tidy <- sr_tidy |>
      dplyr::mutate(!!i := paste0(i, " = ", .data[[i]]))
  }

  if(length(facet_wrap_vars) == 0) {
    ggplot2::ggplot(data = sr_tidy, ggplot2::aes(
      x = .data[[labs[1]]], y = .data[[labs[2]]], group = .data$variable_name)) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower_CI, xmax = .data$upper_CI, colour = .data$variable_name), height = 0.2) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$variable_name, shape = .data$variable_name), size = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), linetype = 2) +
      ggplot2::scale_shape_manual(values = rep(19, 5)) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::labs(title = plotTitle) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(),
                     legend.title = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)
      )
  } else {
    ggplot2::ggplot(data = sr_tidy, ggplot2::aes(
      x = .data[[labs[1]]], y = .data[[labs[2]]], group = .data$variable_name)) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower_CI, xmax = .data$upper_CI, colour = .data$variable_name), height = 0.2) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$variable_name, shape = .data$variable_name), size = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), linetype = 2) +
      ggplot2::scale_shape_manual(values = rep(19, 5)) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::facet_wrap(stats::as.formula(paste("~", paste(facet_wrap_vars, collapse = " + ")))) +
      ggplot2::labs(title = plotTitle) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(),
                     legend.title = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)
      )
  }
}
