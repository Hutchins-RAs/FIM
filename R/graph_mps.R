# graph_mps.R
# LE: 5 May 2023
# Lorae Stojanovic
# The purpose of this code is to create a custom graph that compares actual federal
# disbursements ("disbursed" argument in the function, graphed in a translucent blue
# color) to our best guesses of how much savings resulted from those disbursements
# ("ss" argument in the function, graphed in an orange color). The function also 
# has arguments for customizing the graph title, the display start date, and the 
# display end date. This graphing function is meant for use in mps_calculations.R.

### graph function
graph_mps <- function(disbursed = projections$federal_ui, # How much $ was actually disbursed
                      ss = ss_federal_ui_arp, # Our best guesses on savings resulting from disbursement
                      date = projections$date, # A vector of the dates used in the graph
                      start = "2019-01-01", # Graph start date
                      end = "2025-01-01", # Graph end date
                      title = "Test data type" # Graph title
                      ) {
  
  # Assemble the data for graphing
  combined_df <- data.frame(
    # Constructing the "Date" column
    Date = as.Date(date), 
    # Constructing a column that includes disbursed and saving
    Value = c(disbursed, ss),
    # Constructing a column that labels observations as either a disbursement or
    # implied saving
    Dataset = rep(c("Disbursed", "Implied Saving"), c(nrow(df1), nrow(df2)))
  )
  
  # Initialize graph settings
  # Construct the string for the graph title
  custom_title <- paste0(title, 
                         "\nDisbursement (using BLS data) versus Implied Saving (using MPC assumptions)")
  # Create custom colors
  translucent_blue <- rgb(0, 0.3, 1, alpha = 1)
  translucent_orange <- rgb(0.9, 0.8, 0, alpha = 0.1)
  # Set the date range for display
  start_date <- as.Date(start) # first bar is 2019 Q1
  end_date <- as.Date(end) # last bar is 2024 Q4
  # Add an offset to the x-axis positions. This will align the bars so that 
  # the 2021 Q1 bar, for example, will sit to the right of the 2021 tick mark, 
  # rather than be aligned with the center of the tick mark (which is the default
  # result).
  x_offset <- 45
  
  # Generate bar chart using ggplot2
  bar_chart <- ggplot(combined_df, aes(x = Date + x_offset, y = Value, fill = Dataset)) + 
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
          panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) + # add grey horizontal gridlines
    labs(title = custom_title,
         x = "", # No X-axis title
         y = "") + # No Y-axis title
    scale_fill_manual(values = c("Disbursed" = translucent_blue, "Implied Saving" = translucent_orange)) +
    # Remove the legend title
    guides(fill = guide_legend(title = NULL)) 
  
  # Display the bar chart
  print(bar_chart)
}