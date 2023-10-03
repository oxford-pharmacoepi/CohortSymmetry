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
