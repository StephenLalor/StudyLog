#======================================================================#
####                    Data Prep 
#======================================================================#

#========== Prep Pipeline ==========#

readTxtLog <- function(txt_log_path){
  #' Prep Pipeline
  #' 
  #' Read text study log.
  #' @param txt_log_path character vector
  #' @return data.frame
  logMsg("function", "running readTxtLog()")
  return(utils::read.csv(txt_log_path, header = FALSE))
}

prepTxtLog <- function(txt_log_path, paths){
  #' Prep Pipeline
  #' 
  #' Full text log prep pipeline. Reads, prepares and then saves in Data folder.
  #' @param txt_log_path character vector
  #' @param paths list
  #' @return void
  logMsg("function", "running prepTxtLog()")
  txt_log_path <-  gsub("\\\\", "/", txt_log_path)
  main_df <- readTxtLog(txt_log_path) %>%
    renameCols() %>%
    parseTime() %>%
    fillDays() %>%
    cleanNA() %>%
    addWeekNum() %>%
    addDayName() %>%
    addTotalDur() %>%
    prepCols() %>%
    formatTopic() %>%
    saveLog(paths)
}
