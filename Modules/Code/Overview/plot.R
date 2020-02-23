#======================================================================#
####                    OVERVIEW 
#======================================================================#

overviewPlotData <- function(df, date_var = c("Day", "Week", "Month"), target){
  logMsg("function", "running overviewPlotData()")
  
  #Collect by YearDate:
  if(date_var == "Day"){ #Except for Day.
    df$YearDate <- df$Date
    df$DateTotal <- df$Total
    df <- df[, c("YearDate", "DateTotal")]
  } else{
    df$Month <- lubridate::month(df$Date)
    df$YearDate <- paste0(lubridate::year(df$Date), df[[date_var]])
    df <- df %>%
      dplyr::group_by(YearDate) %>%
      dplyr::summarise("DateTotal" = sum(Total))
  }
  
  #Add text:
  if(date_var == "Day") df$Text <- lubridate::wday(df$YearDate, label = TRUE)
  if(date_var == "Week") df$Text <- paste0("Week ", substr(df$YearDate, 5, nchar(df$YearDate)))
  if(date_var == "Month") df$Text <- month.abb[as.numeric(substr(df$YearDate, 5, nchar(df$YearDate)))]
  
  #Convert target:
  if(date_var == "Day") target <- target/7
  if(date_var == "Week") target <- target
  if(date_var == "Month") target <- target*4
  df$Target <- target #Only single value needed but passing here to avoid more args to overviewPlot.
  
  #Deal with duplicate date entries:
  cond_dupe <- duplicated(df$YearDate)
  dupe_dates <- df$YearDate[cond_dupe]
  for(curr_date in dupe_dates){
    cond_date <- df$YearDate == curr_date
    df$DateTotal[cond_date] <- sum(df$DateTotal[cond_date]) #Sum all study time.
  }
  df <- df[!cond_dupe, ]

  #Create colors:
  df$Color <- "rgba(204, 204, 204, 1)"
  df$Color[c(TRUE, FALSE)] <- "rgba(160, 255, 153, 1)" #Alternating colors.
  df$BorderColor <- "rgba(0, 0, 0, 0.5)"

  #Return DF:
  return(df)
  
}

overviewPlot <- function(plot_df, date_var){
  ### Plot overview of hours. ###
  logMsg("function", "running overviewPlot()")
  
  #Define bar styling:
  bar_marker <- list(color = plot_df$Color,
                     line = list(color = plot_df$BorderColor, width = 1.5))
  
  #Initialise plot:
  plt <- plotly::plot_ly(width = 1400, height = 600)
  
  #Add bars:
  plt <- plt %>%
    plotly::add_trace(x = plot_df$YearDate, y = plot_df$DateTotal,
                      name = "Hours", type = "bar",
                      text = plot_df$Text, textposition = "auto",
                      marker = bar_marker)
  
  #Add line:
  plt <- plt %>%
    add_segments(x = min(plot_df$YearDate), xend = max(plot_df$YearDate),
                 y = plot_df$Target[1], yend = plot_df$Target[1],
                 name = "Target",
                 line = list(color = "rgba(0, 0, 0, 0.5)", dash = "dash"))
  
  #Add layout:
  yaxis_layout = list(title = "Hours")
  xaxis_layout = list(title = paste0("Year", date_var))
  plt <- plt %>%
    plotly::layout(title = "Study Hours",
                   xaxis = xaxis_layout,
                   yaxis = yaxis_layout)
  
  #Return plot:
  return(plt)
  
}
