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
  
  #========== Plot ==========#
  output$overview_plot <- plotly::renderPlotly({
    logMsg("plot", "rendering overview plot")
    plot_df <- overviewPlotData(main_df, input$sel_time, 20)
    return(overviewPlot(plot_df, input$sel_time))
  })
  
  #========== Stats ==========#
  output$good_bad <- DT::renderDataTable({
    logMsg("stats", "rendering good_bad table")
    window_df <- windowSubset(main_df, input$date_range) #Subset data based on slider.
    summary_df <- switch(input$sel_time,
                         "Day" = bestWorstDays(window_df),
                         "Week" = bestWorstWeeks(window_df),
                         "Month" = bestWorstMonths(window_df))
    summary_df <- makeTall(summary_df) #Convert to tall format.
    options = list(info = FALSE, paging = FALSE, searching = FALSE)
    return(DT::datatable(summary_df, options, rownames = FALSE))
  })
  
  output$sliding_avg <- DT::renderDataTable({
    logMsg("stats", "rendering sliding_avg table")
    window_df <- windowSubset(main_df, input$date_range) #Subset data based on slider.
    summary_df <- slidingAvgs(window_df)
    summary_df <- makeTall(summary_df) #Convert to tall format.
    options = list(info = FALSE, paging = FALSE, searching = FALSE)
    return(DT::datatable(summary_df, options, rownames = FALSE))
  })
  
  output$zero_pie_plot <- plotly::renderPlotly({
    logMsg("stats", "rendering zero pie plot")
    window_df <- windowSubset(main_df, input$date_range)
    summary_df <- bestWorstDays(window_df)
    return(zeroPiePlot(summary_df))
  })
    
}