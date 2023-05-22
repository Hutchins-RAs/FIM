# graph_mps.R
# LE: 5 May 2023
# Lorae Stojanovic
# The purpose of this code is to create a custom graph that compares actual federal
# disbursements ("disbursed" argument in the function, graphed in a translucent blue
# color) to our best guesses of how much savings resulted from those disbursements
# ("ss" argument in the function, graphed in an orange color). The function also 
# has arguments for customizing the graph title, the display start date, and the 
# display end date. This graphing function is meant for use in mps_calculations.R.
#
# SECTION 1: graph_mps function
# SECTION 2: ss_graph_wrapper function
# SECTION 3: line_mps function

########## SECTION 1: graph_mps function ##########
graph_mps <- function(disbursed = projections$federal_ui, # How much $ was actually disbursed
                      ss = ss_federal_ui_arp, # Our best guesses on savings resulting from disbursement
                      date = projections$date, # A vector of the dates used in the graph
                      start = "2019-01-01", # Graph start date
                      end = "2025-01-01", # Graph end date
                      title = "Test data type", # Graph title
                      terminal, # Terminal MPS for graph caption
                      annualized = TRUE # TRUE if values are annualized
                      ) {
  
  # Turn annualized values into quarterly values
  if(annualized == TRUE) {
    disbursed <- disbursed/4
    ss <- ss/4
  }
  
  # Get the cumulative sum for the line graph
  # NOTE: this will include all sums up to the present. I may want to consider
  # removing any sums that precede the pandemic
  c_disbursed <- cumsum(disbursed)
  
  # Assemble the data for graphing
  combined_df <- data.frame(
    # Constructing the "Date" column
    Date = as.Date(date), 
    # Constructing a column that includes disbursed and saving and cumulative sum
    Value = c(disbursed, ss, c_disbursed),
    # Constructing a column that labels observations as either a disbursement or
    # implied saving
    Dataset = rep(c("Disbursed", 
                    "Cumulative Implied Saving",
                    "Cumulative Disbursed"), 
                  c(length(disbursed), 
                    length(ss),
                    length(c_disbursed)))
  )
  
  # Initialize graph settings
  # Construct the graph title
  custom_title <- paste0(title, 
                         "\nDisbursement (using BLS data) versus Implied Saving (using MPC assumptions)")
  # Construct the graph caption
  custom_caption <- paste0("Note: Terminal MPS is ", terminal*100, "% of initial disbursement.")
  # Create custom colors
  translucent_blue <- rgb(0, 0.3, 1, alpha = 0.1)
  translucent_orange <- rgb(1, 0.7, 0, alpha = 0.1)
  # Set the date range for display
  start_date <- as.Date(start) # first bar is 2019 Q1
  end_date <- as.Date(end) # last bar is 2024 Q4
  # Add an offset to the x-axis positions. This will align the bars so that 
  # the 2021 Q1 bar, for example, will sit to the right of the 2021 tick mark, 
  # rather than be aligned with the center of the tick mark (which is the default
  # result).
  x_offset <- 45
  
  # Filter out 'Disbursed' from the Dataset column
  bar_df <- combined_df[!(combined_df$Dataset == "Cumulative Disbursed"), ]
  # Generate bar chart using ggplot2
  bar_chart <- ggplot(bar_df, aes(x = Date + x_offset, y = Value, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "identity", alpha = 0.8) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = c(start_date, end_date)) +
    # Adjusting Y-axis labels to a "$X,XXX B" format
    scale_y_continuous(label = scales::dollar_format(suffix = " B")) + 
    # Set the background to white
    theme_classic() +
    # Adjusting X-axis label positioning
    theme(axis.text.x = element_text(angle = 0, # labels are horizontal
                                     vjust = 0.5,
                                     hjust = 0.5), # labels are aligned to center of tick mark
          panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"), # add grey horizontal gridlines
          plot.caption = element_text(hjust = 0)) + # left-align the caption
    labs(title = custom_title,
         x = "", # No X-axis title
         y = "", # No Y-axis title
         caption = custom_caption) + 
    scale_fill_manual(values = c("Disbursed" = translucent_blue, "Cumulative Implied Saving" = translucent_orange)) +
    # Remove the legend title
    guides(fill = guide_legend(title = NULL)) 
  
  # Filter out 'Disbursed' from the Dataset column
  line_df <- combined_df[!(combined_df$Dataset == "Disbursed"), ]
  # Generate line chart using ggplot2
  line_chart <- ggplot(line_df, aes(x = Date + x_offset, y = Value, color = Dataset)) + 
    geom_line(aes(group = Dataset), size = 1) +
    #geom_point(aes(shape = Dataset), size = 3) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = c(start_date, end_date)) +
    scale_y_continuous(label = scales::dollar_format(suffix = " B")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 0,
                                     vjust = 0.5,
                                     hjust = 0.5),
          panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"),
          plot.caption = element_text(hjust = 0)) + 
    labs(title = custom_title,
         x = "", 
         y = "", 
         caption = custom_caption) + 
    scale_color_manual(values = c("Cumulative Disbursed" = "blue", "Cumulative Implied Saving" = "orange")) +
    #scale_shape_manual(values = c("Disbursed" = 20, "Cumulative Implied Saving" = 20)) +
    guides(color = guide_legend(title = NULL), shape = guide_legend(title = NULL))
  
  # # Display the bar chart
  # print(bar_chart)
  
  # Return both the data frame and the chart
  return(list(data = combined_df, bar = bar_chart, line = line_chart))
}

########## SECTION 2: ss_graph_wrapper function ##########

ss_graph_wrapper <- function(disbursed = projections$federal_other_vulnerable_arp, # How much $ was actually disbursed
                             mps_name, # Which row entry to use in c_mps
                             date = projections$date, # A vector of the dates used in the graph
                             start = "2019-01-01", # Graph start date
                             end = "2025-01-01", # Graph end date
                             title = "Federal Other Vulnerable ARP", # Graph title
                             annualized = TRUE # TRUE if data is annualized
)
  {
  # First, get the MPS vector using mps_name
  mps <- c_mps[which(alt_mps$variable == mps_name),] %>%
    select(-variable) %>%
    unlist()
  
  # If we make no further adjustments to MPS, then the roll_sum function used in
  # mps_lorae will automatically assume that saving beyond Q12 will be 0% of the
  # initial disbursement. We want to ensure that doesn't happen. The easiest way 
  # to do this is to make the mps vector very long by adding the terminal value many
  # times to the end.
  # A quick and easy way to do this is to append the last value to the mps vector
  # back to the mps vector N times, where N is the length of the entirety of the 
  # projection period. Using this strategy, it is impossible for us to run out of
  # mps elements before running out of projection period dates.

  terminal <- mps[length(mps)] # last value of the current mps vector
  
  # Appending terminal rate to the mps vector N times to avoid error described above
  mps <- mps %>%
    c(., rep(terminal, times = length(date)))
  
  # Next, calculate the savings stream using mps_lorae
  ss <- mps_lorae(x = disbursed, 
                  mps = mps)
  # Finally, graph the savings stream and the disbursed funds using graph_mps
  result <- graph_mps(disbursed = disbursed, # How much $ was actually disbursed
              ss = ss, # Our best guesses on savings resulting from disbursement
              date = date, # A vector of the dates used in the graph
              start = start, # Graph start date
              end = end, # Graph end date
              title = title, # Graph title
              terminal = terminal, # Terminal MPS
              annualized = annualized # TRUE if data are annualized
  )
  

  # Return the result
  return(result)
}

########## SECTION 3: line_mps function ##########

### Obsolete

line_mps <- function(disbursed = projections$federal_ui, # How much $ was actually disbursed
                      ss = ss_federal_ui_arp, # Our best guesses on savings resulting from disbursement
                      date = projections$date, # A vector of the dates used in the graph
                      start = "2019-01-01", # Graph start date
                      end = "2025-01-01", # Graph end date
                      title = "Test data type", # Graph title
                      terminal, # Terminal MPS for graph caption
                      annualized = TRUE # TRUE if values are annualized
) {
  
  # Turn annualized values into quarterly values
  if(annualized == TRUE) {
    disbursed <- disbursed/4
    ss <- ss/4
  }
  # Assemble the data for graphing
  combined_df <- data.frame(
    # Constructing the "Date" column
    Date = as.Date(date), 
    # Constructing a column that includes disbursed and saving
    Value = c(disbursed, ss),
    # Constructing a column that labels observations as either a disbursement or
    # implied saving
    Dataset = rep(c("Disbursed", "Implied Saving"), c(length(disbursed), length(ss)))
  )
  
  # Initialize graph settings
  # Construct the graph title
  custom_title <- paste0(title, 
                         "\nDisbursement (using BLS data) versus Implied Saving (using MPC assumptions)")
  # Construct the graph caption
  custom_caption <- paste0("Note: Terminal MPS is ", terminal*100, "% of initial disbursement.")
  # Create custom colors
  translucent_blue <- rgb(0, 0.3, 1, alpha = 0.1)
  translucent_orange <- rgb(1, 0.7, 0, alpha = 0.1)
  # Set the date range for display
  start_date <- as.Date(start) # first bar is 2019 Q1
  end_date <- as.Date(end) # last bar is 2024 Q4
  # Add an offset to the x-axis positions. This will align the bars so that 
  # the 2021 Q1 bar, for example, will sit to the right of the 2021 tick mark, 
  # rather than be aligned with the center of the tick mark (which is the default
  # result).
  x_offset <- 45
  
  # Generate line chart using ggplot2
    line_chart <- ggplot(combined_df, aes(x = Date + x_offset, y = Value, color = Dataset)) + 
      geom_line(aes(group = Dataset), size = 1) +
      geom_point(aes(shape = Dataset), size = 3) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = c(start_date, end_date)) +
      scale_y_continuous(label = scales::dollar_format(suffix = " B")) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 0,
                                       vjust = 0.5,
                                       hjust = 0.5),
            panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"),
            plot.caption = element_text(hjust = 0)) + 
      labs(title = custom_title,
           x = "", 
           y = "", 
           caption = custom_caption) + 
      scale_color_manual(values = c("Disbursed" = "blue", "Implied Saving" = "orange")) +
      scale_shape_manual(values = c("Disbursed" = 20, "Implied Saving" = 20)) +
      guides(color = guide_legend(title = NULL), shape = guide_legend(title = NULL))
    
    # Return both the data frame and the chart
    return(list(data = combined_df, chart = line_chart))
  }
