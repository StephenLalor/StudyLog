#======================================================================#
####                    Stats 
#======================================================================#

#========== All Time Plot ==========#

allTimePlot <- function(plot_df){
  #' All Time Plot
  #' 
  #' Create data used by overviewPlot().
  #' @param plot_df data.frame
  #' @return data.frame
  logMsg("function", "running allTimePlot()")
  
  #Initialise plot:
  plt <- plotly::plot_ly(width = 1400, height = 600)
  pal <- RColorBrewer::brewer.pal(n = 8, name = "Dark2") #Color scheme.
  
  #Add cumulative actual total:
  plt <- plt %>%
    plotly::add_trace(x = plot_df$YearDate, y = plot_df$cumul_total,
                      type = "scatter", mode = "lines+markers", name = "Actual",
                      line = list(color = pal[3]), marker = list(color = pal[3]))
  
  #Add cumulative target:
  plt <- plt %>%
    plotly::add_trace(x = plot_df$YearDate, y = plot_df$cumul_target,
                      type = "scatter", mode = "lines", name = "Target",
                      fill = "tonexty", fillcolor = plotly::toRGB(pal[2], alpha = 0.2),
                      line = list(color = pal[5]))
  
  #Add diff:
  plt <- plt %>%
    plotly::add_trace(x = plot_df$YearDate, y = plot_df$cumul_target - plot_df$cumul_total,
                      type = "scatter", mode = "lines+markers", name = "Debt",
                      line = list(color = pal[2]), marker = list(color = pal[2]))
  
  #Add layout:
  xaxis_layout = list(title = "Date")
  yaxis_layout = list(title = "Hours")
  plt <- plt %>%
    plotly::layout(title = "Cumulative Hours",
                   xaxis = xaxis_layout,
                   yaxis = yaxis_layout)
  
  #Return plot:
  return(plt)
  
}