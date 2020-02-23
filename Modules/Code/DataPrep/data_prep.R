#======================================================================#
####                    Data Prep 
#======================================================================#

renameCols <- function(df){
  logMsg("function", "running renameCols()")
  names(df) <- c("Duration", "Date", "Topic")
  return(df)
}

parseTime <- function(df){
  logMsg("function", "running parseTime()")
  df$Duration <- as.character(df$Duration)
  df$Duration <- gsub("(s).*", "\\1", df$Duration) #Remove all after seconds.
  cond_dur <- !grepl("h", df$Duration)
  df$Duration[cond_dur] <- paste0("00h", df$Duration[cond_dur]) #Pad hours.
  df$Duration <- lubridate::hms(df$Duration)
  df$Hours <- lubridate::hour(df$Duration)
  df$Mins <- lubridate::minute(df$Duration)
  df$Seconds <- lubridate::second(df$Duration)
  return(df)
}

fillDays <- function(df){
  logMsg("function", "running fillDays()")
  df$Date <- lubridate::dmy(df$Date)
  num_days <- lubridate::interval(min(df$Date), max(df$Date))/days(1)
  date_seq <- min(df$Date) + days(0:num_days)
  full_df <- data.frame("Date" = date_seq)
  full_df <- full_df %>% dplyr::left_join(df, by = "Date")
  return(full_df)
}

cleanNA <- function(df){
  logMsg("function", "running cleanNA()")
  df$Duration[is.na(df$Duration)] <- 0
  df$Hours[is.na(df$Hours)] <- 0
  df$Mins[is.na(df$Mins)] <- 0
  df$Seconds[is.na(df$Seconds)] <- 0
  df$Topic[is.na(df$Topic)] <- 0
  return(df)
}

addWeekNum <- function(df){
  logMsg("function", "running addWeekNum()")
  df$Week <- strftime(df$Date, format = "%V")
  return(df)
}

addDayName <- function(df){
  logMsg("function", "running addDayName()")
  df$Day <- lubridate::wday(df$Date, label = TRUE)
  return(df)
}

addTotalDur <- function(df){
  logMsg("function", "running addTotalDur()")
  df$Total <- round(df$Hours + (1/60)*df$Mins + (1/(60^2))*df$Seconds, 2)
  return(df)
}

prepCols <- function(df){
  logMsg("function", "running prepCols()")
  df <- df[, c("Date", "Week", "Day", "Total", "Topic")]
  df$Date <- as.character(df$Date)
  df$Week <- as.numeric(df$Week)
  df$Day <- as.character(df$Day)
  df$Total <- as.numeric(df$Total)
  df$Topic <- as.character(df$Topic)
  return(df)
}

genChoices <- function(df){
  logMsg("function", "running genChoices()")
  choices_vec <- c("Data Structures and Algorithms", #Premade choices.
                   "Machine Learning",
                   "Statistics & Probability")
  choices_vec <- c(choices_vec, unique(df$Topic)) #All existing choices.
  choices_vec <- choices_vec[!is.na(choices_vec)] #Remove all NAs.
  return(choices_vec)
}

readTxtLog <- function(txt_log_path){
  logMsg("function", "running readTxtLog()")
  return(read.csv(txt_log_path, header = FALSE))
}

prepTxtLog <- function(txt_log_path, paths){
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
    saveLog(paths)
}
