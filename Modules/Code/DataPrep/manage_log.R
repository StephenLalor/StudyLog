#======================================================================#
####                    Manage Log 
#======================================================================#

#========== Log Manipulation ==========#

readLog <- function(paths){
  #' Log Manipulation
  #' 
  #' Read RDS study log saved in Data folder.
  #' @param paths list
  #' @return data.frame
  logMsg("function", "running readLog()")
  df <- readRDS(paste0(paths$Data, "StudyLog.rds"))
  return(df)
}

updateLog <- function(df, dur, date, topic){
  #' Log Manipulation
  #' 
  #' Add a new line to the log DF.
  #' @param df data.frame
  #' @param dur character vec - duration for new entry
  #' @param date character vec
  #' @param topic character vec
  #' @return data.frame - with new row appended
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

archiveLog <- function(paths){
  #' Log Manipulation
  #' 
  #' Move current study log RDS from Data folder to Archive folder and stamp name with date.
  #' @param paths list
  #' @return void
  logMsg("function", "running archiveLog()")
  log_path <- paste0(paths$Data, "StudyLog.rds") #Current log.
  if(file.exists(log_path)){
    date_form <- gsub("-", "", Sys.Date()) #Add today's date.
    archive_path <- paste0(paths$Data, "Archive/", "StudyLog_", date_form, ".rds")
    file.copy(log_path, archive_path)
  }
}

saveLog <- function(df, paths){
  #' Log Manipulation
  #' 
  #' Save study log dataframe to Data folder as RDS.
  #' @param df data.frame
  #' @param paths list
  #' @return data.frame
  logMsg("function", "running saveLog()")
  saveRDS(df, paste0(paths$Data, "StudyLog.rds"))
  return(df)
}

delDate <- function(df, date){
  #' Log Manipulation
  #' 
  #' Delete rows of DF corresponding to given date.
  #' @param df data.frame
  #' @param date character vec
  #' @return data.frame
  logMsg("function", "running delDate()")
  return(df[df$Date != date, ])
}

#========== Other ==========#

validateDate <- function(date){
  ### Check given date is valid. ###
  #' Log Management Other
  #' 
  #' Check user has given a valid date.
  #' @param date character vec
  #' @return bool
  logMsg("function", "running validateDate()")
  nchar_chk <- nchar(date) == 10 #Check number of characters.
  slash_chk <- (substr(date, 3, 3) == "/") & (substr(date, 6, 6) == "/") #Check position of slashes.
  digits_chk <- grepl("^[[:digit:]]+$", gsub("/", "", date)) #All numeric, except slashes.
  if(!all(nchar_chk, slash_chk, digits_chk)){ #If any fail.
    warning("Invalid Date")
    return(FALSE)
  }
  return(TRUE)
}
