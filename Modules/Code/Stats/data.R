#======================================================================#
####                    Stats Data
#======================================================================#

#========== Divide Data ==========#

windowSubset <- function(df, slider_range){
  #' Divide Data
  #' 
  #' Create overview bar plot.
  #' @param df data.frame
  #' @param slider_range character vec
  #' @return plotly htmlwidget
  logMsg("function", "running windowSubset()")
  
  #Deal with duplicate date entries:
  cond_dupe <- duplicated(df$Date)
  dupe_dates <- df$Date[cond_dupe]
  for(curr_date in dupe_dates){
    cond_date <- df$Date == curr_date
    df$Total[cond_date] <- sum(df$Total[cond_date]) #Sum all study time.
  }
  df <- df[!cond_dupe, ]
  
  #Subset:
  time_range <- c(0, 0)
  time_range[1] <- which(df$Date == slider_range[1])
  time_range[2] <- which(df$Date == slider_range[2])
  return(df[time_range[1]:time_range[2], ])
}

#========== Basic Stats ==========#

allTimeTotal <- function(df){
  #' Basic Stats
  #' 
  #' Calculate all time total study time.
  #' @param df data.frame
  #' @return numeric
  logMsg("function", "running allTimeTotal()")
  return(sum(df$Total))
}

slidingAvgs <- function(window_df){
  #' Basic Stats
  #' 
  #' Calculate sliding averages.
  #' @param window_df data.frame
  #' @return data.frame
  logMsg("function", "running slidingAvgs()")
  cond_weekend <- window_df$Day %in% c("Sat", "Sun")
  df <- data.frame(WindowAvg = mean(window_df$Total) %>% round(2),
                   WeekdayAvg = mean(window_df$Total[!cond_weekend]) %>% round(2),
                   WeekendDateAvg = mean(window_df$Total[cond_weekend]) %>% round(2))
  
  return(df)
}

#========== Bests And Worsts ==========#

bestWorstDays <- function(window_df){
  #' Bests And Worsts
  #' 
  #' Calculate best and worst day records.
  #' @param window_df data.frame
  #' @return data.frame
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
  #' Bests And Worsts
  #' 
  #' Calculate best and worst week records.
  #' @param window_df data.frame
  #' @return data.frame
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
  #' Bests And Worsts
  #' 
  #' Calculate best and worst month records.
  #' @param window_df data.frame
  #' @return data.frame
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