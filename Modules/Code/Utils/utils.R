#======================================================================#
####                    Utils 
#======================================================================#

logMsg <- function(loc, msg, timestamp){
  if(missing(timestamp)) timestamp <- TRUE
  timestamp <- crayon::yellow(paste0("@ ",Sys.time()))
  loc_form <- crayon::blue(paste0("LOG - ", toupper(loc), ":"))
  cat(paste(loc_form, msg, timestamp), "\n")
}

loadScripts <- function(dir_path){
  ### Load all scripts in folder. ###
  logMsg("function", "running loadScripts()")

  #Loop through and source all:
  script_list <- list.files(dir_path, ".R", recursive = TRUE, full.names = TRUE)
  cat(underline("From:", dir_path, "\n"))
  for(i in 1:length(script_list)){
    curr_script <- script_list[i]
    cat("\t", yellow("Sourcing script:"), bold(basename(curr_script)), "\n")
    source(curr_script)
  }
}

defPaths <- function(){
  logMsg("function", "running defPaths()")
  lst <- list()
  lst$Base <- paste0(getwd(), "/")
  lst$Data <- paste0(lst$Base, "Data/")
  lst$Code <- paste0(lst$Base, "Modules/Code/")
  lst$AppPath <- paste0(lst$Base, "Modules/Shiny/")
  return(lst)
}

checkDuplicateEntry <- function(df){
  logMsg("function", "running checkDuplicateEntry()")
  dupe_bool <- duplicated(df$Date)
  if(sum(dupe_bool) > 0){
    dupe_dates <- df$Date[dupe_bool]
    dupe_df <- df[df$Date %in% dupe_dates, ]
    num_dupes <- sum(dupe_bool)
    cat(crayon::blue("Duplicate Entries Found:"), crayon::bold(num_dupes), "\n")
    print(dupe_df)
  }
}

makeTall <- function(df){
  ### Trandform DF to tall version. ###
  logMsg("function", "running makeTall()")
  df <- t(df) %>% as.data.frame() #Transform to tall.
  df$Variable <- rownames(df)
  rownames(df) <- NULL
  names(df) <- c("Value", "Variable")
  df <- df[, c("Variable", "Value")] #Reorder.
  return(df)
}

getDate <- function(){
  date <- gsub("-", "/", Sys.Date())
  year <- substr(date, 1, 4)
  month <- substr(date, 6, 7)
  day <- substr(date, 9, 10)
  return(paste0(day, "/", month, "/", year))
}

getTopic <- function(new_topic_in, topic_in){
  if(new_topic_in != "") return(new_topic_in) else return(topic_in)
}