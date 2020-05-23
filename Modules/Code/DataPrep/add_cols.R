#======================================================================#
####                    Data Prep 
#======================================================================#

#========== Add Cols ==========#

addWeekNum <- function(df){
  #' Add Cols
  #' 
  #' Add week number (1-52) to DF.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running addWeekNum()")
  df$Week <- strftime(df$Date, format = "%V")
  return(df)
}

addDayName <- function(df){
  #' Add Cols
  #' 
  #' Add day name (e.g. Tuesday) to DF.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running addDayName()")
  df$Day <- lubridate::wday(df$Date, label = TRUE)
  return(df)
}

addTotalDur <- function(df){
  #' Add Cols
  #' 
  #' Add total study duration in hours to DF.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running addTotalDur()")
  df$Total <- round(df$Hours + (1/60)*df$Mins + (1/(60^2))*df$Seconds, 2)
  return(df)
}

#========== Format Cols ==========#

prepCols <- function(df){
  #' Format Cols
  #' 
  #' Select reorder and set class of each col.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running prepCols()")
  df <- df[, c("Date", "Week", "Day", "Total", "Topic")]
  df$Date <- as.character(df$Date)
  df$Week <- as.numeric(df$Week)
  df$Day <- as.character(df$Day)
  df$Total <- as.numeric(df$Total)
  df$Topic <- as.character(df$Topic)
  return(df)
}

formatTopic <- function(df){
  #' Format Cols
  #' 
  #' Replace Topic col with "None" for days with 0 Total.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running formatTopic()")
  df$Topic[df$Total == 0] <- "None"
  return(df)
}