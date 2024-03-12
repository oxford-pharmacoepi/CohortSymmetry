#' A plot for the sequence ratios.
#'
#' @description
#' It provides a ggplot of the sequence ratios of index and marker cohorts.
#'
#' @param cdm A cdm object.
#' @param joinedTable The name of a table in the cdm of the form of the output
#' of generateSequenceCohortSet.
#' @param sequenceRatio A table of the form of the output of summariseSequenceRatio.
#' @param onlyaSR If the only SR to be plotted is the adjusted SR.
#' @param index_ids What index ids to plot, if NULL all will be plotted.
#' @param marker_ids What marker ids to plot, if NULL all will be plotted.
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
#' cdm <- CohortSymmetry::mockCohortSymmetry()
#' cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
#'                                                  indexTable = "cohort_1",
#'                                                  markerTable = "cohort_2",
#'                                                  name = "joined_cohort")
#' sr <- CohortSymmetry::summariseSequenceRatio(cdm, "joined_cohort")
#' plotSequenceRatio(cdm, "joined_cohort", sr)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotSequenceRatio <- function(cdm,
                              joinedTable,
                              sequenceRatio,
                              onlyaSR = FALSE,
                              index_ids = NULL,
                              marker_ids = NULL,
                              plotTitle = NULL,
                              labs = c("SR", "Drug Pairs"),
                              colours = c("red", "blue")
                              ) {
  # checks
  checkInputPlotSequenceRatio(cdm = cdm,
                              joinedTable = joinedTable,
                              sequenceRatio = sequenceRatio,
                              onlyaSR = onlyaSR,
                              index_ids = index_ids,
                              marker_ids = marker_ids,
                              plotTitle = plotTitle,
                              labs = labs,
                              colours = colours)

  if(!is.null(index_ids) || !is.null(marker_ids)) {
    all_names <- attr(cdm[[joinedTable]], "cohort_set") %>%
      dplyr::select("marker_name", "index_name", "index_id", "marker_id") %>%
      dplyr::collect()
    sequenceRatio <- sequenceRatio %>%
      visOmopResults::splitGroup() %>%
      dplyr::left_join(all_names,
                       by = c("index_cohort_name" = "index_name", "marker_cohort_name" = "marker_name"))
    if(!is.null(index_ids)) {
      sequenceRatio <- sequenceRatio %>%
        dplyr::filter(.data$index_id %in% index_ids)
    }
    if(!is.null(marker_ids)) {
      sequenceRatio <- sequenceRatio %>%
        dplyr::filter(.data$marker_id %in% marker_ids)
    }
    sequenceRatio <- sequenceRatio %>%
      dplyr::select(-c("index_id", "marker_id"))
  } else {
    sequenceRatio <- sequenceRatio %>%
      visOmopResults::splitGroup()
  }

  sr_tidy <- sequenceRatio %>%
    dplyr::filter(.data$variable_name == "sequence_ratio") %>%
    dplyr::select(-c("cdm_name", "result_type", "package_name", "package_version", "strata_name", "strata_level", "variable_name")) %>%
    visOmopResults::splitAdditional() %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") %>%
    dplyr::mutate(group = paste0(.data$index_cohort_name, " -> ", .data$marker_cohort_name)) %>%
    dplyr::select(-c("index_cohort_name", "marker_cohort_name")) %>%
    dplyr::mutate(
      point_estimate = as.numeric(.data$point_estimate),
      lower_CI = as.numeric(.data$lower_CI),
      upper_CI = as.numeric(.data$upper_CI),
      variable_level = as.factor(.data$variable_level)
    ) %>%
    dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|.data$group) %>%
    dplyr::rename(
      !!labs[1] := "point_estimate",
      !!labs[2] := "group"
    )

  if(onlyaSR) {
    sr_tidy <- sr_tidy %>%
      dplyr::filter(.data$variable_level != "crude")
    colours = c("adjusted" = colours[1])
  } else {
    colours = c("crude" = colours[1], "adjusted" = colours[2])
  }

  facet_wrap_vars <- colnames(sr_tidy)[! colnames(sr_tidy) %in% c(labs[2], labs[1], "lower_CI", "upper_CI", "variable_level")]
  for(i in facet_wrap_vars) {
    sr_tidy <- sr_tidy %>%
      dplyr::mutate(!!i := paste0(i, " = ", .data[[i]]))
  }


  if(length(facet_wrap_vars) == 0) {
    ggplot2::ggplot(data = sr_tidy, ggplot2::aes(
      x = .data[[labs[1]]], y = .data[[labs[2]]], group = .data$variable_level)) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower_CI, xmax = .data$upper_CI, colour = .data$variable_level), height = 0.2) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$variable_level, shape = .data$variable_level), size = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), linetype = 2) +
      ggplot2::scale_shape_manual(values = rep(19, 5)) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(),
                     legend.title = ggplot2::element_blank()
      )
  } else {
    ggplot2::ggplot(data = sr_tidy, ggplot2::aes(
      x = .data[[labs[1]]], y = .data[[labs[2]]], group = .data$variable_level)) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower_CI, xmax = .data$upper_CI, colour = .data$variable_level), height = 0.2) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$variable_level, shape = .data$variable_level), size = 3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1), linetype = 2) +
      ggplot2::scale_shape_manual(values = rep(19, 5)) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::facet_wrap(stats::as.formula(paste("~", paste(facet_wrap_vars, collapse = " + ")))) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(),
                     legend.title = ggplot2::element_blank()
      )
  }
}
