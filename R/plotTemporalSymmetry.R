#' A plot for the temporal symmetry of cohorts.
#'
#' @description
#' It provides a ggplot of the temporal symmetry of two or more cohorts.
#'
#' @param cdm A cdm object.
#' @param joinedTable The name of a table in the cdm of the form of the output
#' of generateSequenceCohortSet.
#' @param index_ids What index ids to plot, if NULL all will be plotted.
#' @param marker_ids What marker ids to plot, if NULL all will be plotted.
#' @param plotTitle Title of the plot, if NULL no title will be plotted.
#' @param labs Axis labels for the plot.
#' @param timescale Timescale for the x axis of the plot (month, day, year).
#' @param censorRange Counts to be censored, can be NULL.
#' @param xlim Limits for the x axis of the plot.
#' @param colours Colours for both parts of the plot, pre- and post- time 0.
#'
#' @return A plot for the temporal symmetry of cohorts.
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
#' plotTemporalSymmetry(cdm, "joined_cohort", censorRange = NULL)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotTemporalSymmetry <- function(cdm,
                                 joinedTable,
                                 index_ids = NULL,
                                 marker_ids = NULL,
                                 plotTitle = NULL,
                                 labs = c("Time (months)", "Individuals (N)"),
                                 timescale = "month",
                                 censorRange = NULL,
                                 xlim = c(-12, 12),
                                 colours = c("blue", "red")) {
  # checks
  checkInputPlotTemporalSymmetry(cdm = cdm,
                                 joinedTable = joinedTable,
                                 index_ids = index_ids,
                                 marker_ids = marker_ids,
                                 plotTitle = plotTitle,
                                 labs = labs,
                                 censorRange = censorRange,
                                 xlim = xlim,
                                 colours = colours)

  index_names <- attr(cdm[[joinedTable]], "cohort_set") %>%
    dplyr::select("cohort_definition_id", "index_name", "index_id", "marker_id")
  marker_names <- attr(cdm[[joinedTable]], "cohort_set") %>%
    dplyr::select("cohort_definition_id", "marker_name")

  plot_data <- cdm[[joinedTable]] %>%
    dplyr::mutate(time = CDMConnector::datediff(
      "index_date", "marker_date", interval = timescale)) %>%
    dplyr::select("cohort_definition_id", "time") %>%
    dplyr::group_by(.data$cohort_definition_id, .data$time) %>%
    dplyr::summarise(individuals = as.integer(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      index_names,
      by = c("cohort_definition_id")
    ) %>%
    dplyr::left_join(
      marker_names,
      by = c("cohort_definition_id")
    ) %>%
    dplyr::select(-c("cohort_definition_id")) %>%
    dplyr::mutate(individuals = dplyr::if_else(.data$individuals %in% censorRange,
                                               NA, .data$individuals)) %>%
    dplyr::compute()

  if(!is.null(index_ids)) {
    plot_data <- plot_data %>%
      dplyr::filter(.data$index_id %in% .env$index_ids)
  }

  if(!is.null(marker_ids)) {
    plot_data <- plot_data %>%
      dplyr::filter(.data$marker_id %in% .env$marker_ids)
  }

  if(all(is.na(plot_data %>% dplyr::pull("individuals")))) {
    cli::cli_abort("There is nothing to plot. With that censorRange and the
                   index and marker ids provided no counts are available.")
  }

  plot_data <- plot_data %>%
    dplyr::select(-c("index_id", "marker_id")) %>%
    dplyr::filter(.data$time != 0) %>%
    dplyr::mutate(colour = dplyr::if_else(.data$time > 0, "B", "A")) %>%
    dplyr::mutate(index_name = paste0("index = ", .data$index_name),
                  marker_name = paste0("marker = ", .data$marker_name)) %>%
    dplyr::compute()

  colours = c("A" = colours[1], "B" = colours[2])

  width_range <- (xlim[2] - xlim[1])/2

  ggplot2::ggplot(data = plot_data, ggplot2::aes(
    x = .data$time, y = .data$individuals, fill = .data$colour)) +
    ggplot2::geom_col(width = 0.01*width_range) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$colour), size = 1) +
    ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2])) +
    ggplot2::labs(title = plotTitle, x = labs[1], y = labs[2]) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::facet_wrap(~ index_name + marker_name, scales = "free") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = colours)
}
