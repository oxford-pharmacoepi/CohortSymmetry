plotTemporalSymmetry <- function(cdm,
                                 joinedTable,
                                 censorRange = c(1:4),
                                 xlim = c(-12, 12),
                                 colours = c("blue", "red")) {
  # checks
  checkInputPlotTemporalSymmetry(cdm = cdm,
                                 joinedTable = joinedTable,
                                 censorRange = censorRange,
                                 xlim = xlim,
                                 colours = colours)

  index_names <- attr(cdm[[joinedTable]], "cohort_set") %>%
    dplyr::select("cohort_definition_id", "index_name")
  marker_names <- attr(cdm[[joinedTable]], "cohort_set") %>%
    dplyr::select("cohort_definition_id", "marker_name")

  plot_data <- cdm[[joinedTable]] %>%
    dplyr::mutate(time = CDMConnector::datediff(
      "index_date", "marker_date", interval = "month")) %>%
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

  plot_data <- plot_data %>%
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
    ggplot2::labs(x = "Time (months)", y = "Individuals (N)") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_wrap(~ index_name + marker_name, scales = "free") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = colours)
}
