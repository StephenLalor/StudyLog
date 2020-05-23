#======================================================================#
####                    Topics Data
#======================================================================#

#========== Create Summary ==========#

topicsData <- function(df){
  #' Create Summary
  #' 
  #' Summarise data based on topics.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running topicsData()")
  
  #Summarise:
  summary_df <- df %>%
    dplyr::group_by(Topic) %>%
    dplyr::summarise(sum_total = sum(Total)) %>%
    dplyr::mutate(sum_total_pct = sum_total/sum(sum_total)) %>%
    dplyr::arrange(dplyr::desc(sum_total))
  
  #Return DF:
  return(as.data.frame(summary_df))
  
}

#========== Plot ==========#

topicsPlot <- function(plot_df){
  #' Create topics plot
  #' 
  #' Plot barchart of summarised topics daat.
  #' @param plot_df data.frame
  #' @return plotly htmlwidget
  logMsg("function", "running topicsPlot()")
  
  #Define bar styling:
  bar_marker <- list(line = list(color = "rgba(0, 0, 0, 0.5)", width = 2.5))
  
  #Initialise plot:
  plt <- plotly::plot_ly(width = 1400, height = 600)
  
  #Add bars:
  plt <- plt %>%
    plotly::add_trace(x = plot_df$Topic, y = plot_df$sum_total,
                      name = "Hours", type = "bar",
                      marker = bar_marker,
                      color = plot_df$Topic,
                      colors = "Dark2")
  
  #Add layout:
  xaxis_layout = list(title = "Topic", categoryorder = "array", categoryarray = plot_df$Topic)
  yaxis_layout = list(title = "Hours", dtick = 10)
  plt <- plt %>%
    plotly::layout(title = "Topic Breakdown",
                   xaxis = xaxis_layout,
                   yaxis = yaxis_layout,
                   showlegend = FALSE)
  
  #Return plot:
  return(plt)
  
}
