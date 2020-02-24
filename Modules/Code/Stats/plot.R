#======================================================================#
####                    Stats Plots
#======================================================================#

#========== Pie Plot ==========#

zeroPiePlot <- function(df){
  #' Bests And Worsts
  #' 
  #' Calculate best and worst week records.
  #' @param df data.frame
  #' @return plotly htmlwidget
  logMsg("function", "running zeroPiePlot()")
  
  #Plot setup:
  pie_df <- data.frame(ZeroDaysProp = c(df$ZeroDaysProp, 1-df$ZeroDaysProp),
                       ZeroDays = c(df$ZeroDays, df$NumberDays - df$ZeroDays),
                       Color = c("red", "green"))
  pie_df$Text <- c("Zero Days", "Work Days")
  pie_df$Label <- pie_df$ZeroDays #Using this as label to access it in hovertemplate.
  marker_spec <- list(colors = pie_df$Color,
                      line = list(color = '#FFFFFF', width = 1))
  hover_template <- paste("<b>%{text}</b>: %{label}", #Can only use plot data objects here.
                          "<br><b>Percentage</b>: %{percent}</br>")
  
  #Initialise plot:
  plt <- plotly::plot_ly()
  
  #Add pie:
  plt <- plotly::plot_ly() %>%
    plotly::add_pie(values = pie_df$ZeroDaysProp,
                    labels = pie_df$Label,
                    text = pie_df$Text,
                    marker = marker_spec,
                    hovertemplate = hover_template)
  
  #Add layout:
  plt <- plt %>%
    plotly::layout(showlegend = FALSE,
                   title = "Zero Days")
  
  return(plt)
}