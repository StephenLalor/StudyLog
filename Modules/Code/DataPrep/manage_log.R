#======================================================================#
####                    Manage Log 
#======================================================================#

readLog <- function(paths){
  logMsg("function", "running readLog()")
  df <- readRDS(paste0(paths$Data, "StudyLog.rds"))
  return(df)
}

validateDate <- function(date){
  ### Check given date is valid. ###
  logMsg("function", "running validateDate()")
  nchar_chk <- nchar(date) == 10
  slash_chk <- (substr(date, 3, 3) == "/") & (substr(date, 6, 6) == "/")
  digits_chk <- grepl("^[[:digit:]]+$", gsub("/", "", date)) #All numeric, except slashes.
  if(!all(nchar_chk, slash_chk, digits_chk)){
    warning("Invalid Date")
    return(FALSE)
  }
  return(TRUE)
}

updateLog <- function(df, dur, date, topic){
  logMsg("function", "running updateLog()")
  
  #Set up new line:
  new_df <- data.frame("Duration" = dur, "Date" = date, "Topic" = topic)
  new_df$Date <- lubridate::dmy(new_df$Date)
  new_df <- new_df %>%
    parseTime() %>%
    cleanNA() %>%
    addWeekNum() %>%
    addDayName() %>%
    addTotalDur() %>%
    prepCols()

  #Add to existing data:
  return(rbind(df, new_df))
}
  
delDate <- function(df, date){
  logMsg("function", "running delDate()")
  return(df[df$Date != date, ])
}

archiveLog <- function(paths){
  logMsg("function", "running archiveLog()")
  log_path <- paste0(paths$Data, "StudyLog.rds") #Current log.
  if(file.exists(log_path)){
    date_form <- gsub("-", "", Sys.Date()) #Add today's date.
    archive_path <- paste0(paths$Data, "Archive/", "StudyLog_", date_form, ".rds")
    file.copy(log_path, archive_path)
  }
}

saveLog <- function(df, paths){
  logMsg("function", "running saveLog()")
  saveRDS(df, paste0(paths$Data, "StudyLog.rds"))
  return(df)
}