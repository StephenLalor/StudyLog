#======================================================================#
####                    Data Prep 
#======================================================================#

#========== Basic Formatting ==========#

renameCols <- function(df){
  #' Basic Formatting
  #' 
  #' Rename columns names to meaningful ones.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running renameCols()")
  names(df) <- c("Duration", "Date", "Topic")
  return(df)
}

parseTime <- function(df){
  #' Basic Formatting
  #' 
  #' Parse 00h00m00s format time and create more useful individual columns.
  #' @param df data.frame
  #' @return data.frame
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
  #' Basic Formatting
  #' 
  #' Add all missing days between days already logged.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running fillDays()")
  df$Date <- lubridate::dmy(df$Date)
  num_days <- lubridate::interval(min(df$Date), max(df$Date))/lubridate::days(1)
  date_seq <- min(df$Date) + lubridate::days(0:num_days)
  full_df <- data.frame("Date" = date_seq)
  full_df <- full_df %>% dplyr::left_join(df, by = "Date")
  return(full_df)
}

cleanNA <- function(df){
  #' Basic Formatting
  #' 
  #' Replace NA instances of variables with substitute.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running cleanNA()")
  df$Duration[is.na(df$Duration)] <- 0
  df$Hours[is.na(df$Hours)] <- 0
  df$Mins[is.na(df$Mins)] <- 0
  df$Seconds[is.na(df$Seconds)] <- 0
  df$Topic[is.na(df$Topic) | df$Topic == ""] <- "Unknown"
  return(df)
}