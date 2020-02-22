#======================================================================#
####                    Stats Data
#======================================================================#

allTimeTotal <- function(df){
  ### Calculate total time. ###
  logMsg("function", "running allTimeTotal()")
  return(sum(df$Total))
}

windowSubset <- function(df, slider_range){
  ### Subset df for given range. ###
  logMsg("function", "running windowSubset()")
  time_range <- c(0, 0)
  time_range[1] <- which(df$Date == slider_range[1])
  time_range[2] <- which(df$Date == slider_range[2])
  return(df[time_range[1]:time_range[2], ])
}

slidingAvgs <- function(window_df){
  ### Calculate averages over sliding window. ###
  logMsg("function", "running slidingAvgs()")
  cond_weekend <- window_df$Day %in% c("Sat", "Sun")
  df <- data.frame(WindowAvg = mean(window_df$Total) %>% round(2),
                   WeekdayAvg = mean(window_df$Total[!cond_weekend]) %>% round(2),
                   WeekendDateAvg = mean(window_df$Total[cond_weekend]) %>% round(2))
  
  return(df)
}

bestWorstDays <- function(window_df){
  ### Calculate best and worst day totals. ###
  logMsg("function", "running bestWorstDays()")
  
  #Define conditions:
  cond_weekend <- window_df$Day %in% c("Sat", "Sun")
  cond_best_weekday <- which.max(window_df$Total[!cond_weekend])
  cond_best_weekend_day <- which.max(window_df$Total[cond_weekend])
  
  #Create summary DF:
  df <- data.frame(NumberDays = nrow(window_df),
                   ZeroDays = sum(window_df$Total == 0),
                   ZeroDaysProp = (sum(window_df$Total == 0)/nrow(window_df)) %>% round(2),
                   BestWeekDay = window_df$Total[!cond_weekend][cond_best_weekday],
                   BestWeekDayDate = window_df$Date[!cond_weekend][cond_best_weekday],
                   BestWeekendDay = window_df$Total[cond_weekend][cond_best_weekend_day],
                   BestWeekendDayDate = window_df$Date[cond_weekend][cond_best_weekend_day])

  return(df)
}

bestWorstWeeks <- function(window_df){
  ### Calculate best and worst week totals. ###
  logMsg("function", "running bestWorstWeeks()")
  
  #Group DF:
  window_df <- window_df %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::mutate(YearWeek = paste0(Year, Week)) %>%
    dplyr::group_by(YearWeek) %>%
    dplyr::summarise(WeekTotal = round(sum(Total), 2))
  
  #Define conditions:
  cond_best_week <- which.max(window_df$WeekTotal)
  cond_worst_week <- which.min(window_df$WeekTotal)
  
  #Create summary DF:
  df <- data.frame(BestWeek = window_df$WeekTotal[cond_best_week],
                   BestWeekDate = window_df$YearWeek[cond_best_week],
                   WorstWeek = window_df$WeekTotal[cond_worst_week],
                   WorstWeekDate = window_df$YearWeek[cond_worst_week])

  return(df)
}

bestWorstMonths <- function(window_df){
  ### Calculate best and worst week totals. ###
  logMsg("function", "running bestWorstMonths()")
  
  #Group DF:
  window_df <- window_df %>%
    dplyr::mutate(Year = lubridate::year(Date)) %>%
    dplyr::mutate(Month = lubridate::month(Date)) %>%
    dplyr::mutate(YearMonth = paste0(Year, Month)) %>%
    dplyr::group_by(YearMonth) %>%
    dplyr::summarise(MonthTotal = round(sum(Total), 2))
  
  #Add formatted month name:
  year_name <- substr(window_df$YearMonth, 1, 4)
  month_name <- month.abb[as.numeric(substr(window_df$YearMonth, 5, nchar(window_df$YearMonth)))]
  window_df$MonthName <- paste0(month_name, " ", year_name)
  
  #Define conditions:
  cond_best_month <- which.max(window_df$MonthTotal)
  cond_worst_month <- which.min(window_df$MonthTotal)

  #Create summary DF:
  df <- data.frame(BestMonth = window_df$MonthTotal[cond_best_month],
                   BestMonthName = window_df$MonthName[cond_best_month],
                   WorstMonth = window_df$MonthTotal[cond_worst_month],
                   WorstMonthName = window_df$MonthName[cond_worst_month])
  
  return(df)
}