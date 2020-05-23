#======================================================================#
####                    SERVER 
#======================================================================#

server <- function(input, output){
  
  #========== Input ==========#
  observeEvent(input$add_time, {
    logMsg("input", "add_time clicked")
    valid_date <- validateDate(input$date_in) #Check input date.
    if(!valid_date) return() #Break from function.
    archiveLog(paths) #More current version to archive.
    topic <- getTopic(input$new_topic_in, input$topic_in) #Use selected topic.
    main_df <- updateLog(main_df, input$time_in, input$date_in, topic) #Add new line to log.
    saveLog(main_df, paths) #Save now updated log.
  })
  
  observeEvent(input$proc_old_log, {
    logMsg("input", "proc_old_log clicked")
    prepTxtLog(input$old_log_path, paths)
  })
  
  #========== Overview ==========#
  output$overview_plot <- plotly::renderPlotly({
    logMsg("plot", "rendering overview plot")
    plot_df <- overviewPlotData(main_df, input$sel_time, 20)
    return(overviewPlot(plot_df, input$sel_time))
  })
  
  #========== Stats ==========#
  output$alltime_plot <- plotly::renderPlotly({
    logMsg("plot", "rendering alltime plot")
    plot_df <- overviewPlotData(main_df, input$sel_time, 20)
    return(allTimePlot(plot_df))
  })
  
  #========== Topics ==========#
  output$topics_bar_plot <- plotly::renderPlotly({
    logMsg("topics", "rendering topics barchart")
    summary_df <- topicsData(main_df)
    return(topicsPlot(summary_df))
  })
    
}