#======================================================================#
####                    Utils 
#======================================================================#

#========== General Project Utilities ==========#

logMsg <- function(loc, msg, timestamp){
  #' General Project Utilities
  #' 
  #' Concatenate a line to console with colouring and timestamp.
  #' @param loc character vec
  #' @param msg character vec
  #' @param timestamp bool - defaults to TRUE
  #' @return void
  if(missing(timestamp)) timestamp <- TRUE
  timestamp <- crayon::yellow(paste0("@ ", Sys.time()))
  loc_form <- crayon::blue(paste0("LOG - ", toupper(loc), ":"))
  cat(paste(loc_form, msg, timestamp), "\n")
}

loadScripts <- function(dir_path){
  #' General Project Utilities
  #' 
  #' Load all R scripts in specified folder.
  #' @param dir_path character vec
  #' @return void
  logMsg("function", "running loadScripts()")

  #Loop through and source all:
  script_list <- list.files(dir_path, ".R", recursive = TRUE, full.names = TRUE)
  cat(crayon::underline("From:", dir_path, "\n"))
  for(i in 1:length(script_list)){
    curr_script <- script_list[i]
    cat("\t", crayon::yellow("Sourcing script:"), crayon::bold(basename(curr_script)), "\n")
    source(curr_script)
  }
}

defPaths <- function(){
  #' General Project Utilities
  #' 
  #' Storage list of useful paths for app.
  #' @return list
  logMsg("function", "running defPaths()")
  lst <- list()
  lst$Base <- paste0(getwd(), "/")
  lst$Data <- paste0(lst$Base, "Data/")
  lst$Code <- paste0(lst$Base, "Modules/Code/")
  lst$AppPath <- paste0(lst$Base, "Modules/Shiny/")
  return(lst)
}

getDate <- function(){
  #' General Project Utilities
  #' 
  #' Return date properly formatted for use in app.
  #' @return character vec
  date <- gsub("-", "/", Sys.Date())
  year <- substr(date, 1, 4)
  month <- substr(date, 6, 7)
  day <- substr(date, 9, 10)
  return(paste0(day, "/", month, "/", year))
}

#========== Manipulation ==========#

checkDuplicateEntry <- function(df){
  #' Manipulation
  #' 
  #' Check if DF has duplicate date entries, print details of them if so.
  #' @param df data.frame
  #' @return void
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
  #' Manipulation
  #' 
  #' Transform DF to tall format, as opposed to wide.
  #' @param df data.frame
  #' @return data.frame
  logMsg("function", "running makeTall()")
  df <- t(df) %>% as.data.frame() #Transform to tall.
  df$Variable <- rownames(df)
  rownames(df) <- NULL
  names(df) <- c("Value", "Variable")
  df <- df[, c("Variable", "Value")] #Reorder.
  return(df)
}

#========== Shiny ==========#

getTopic <- function(new_topic_in, topic_in){
  #' Shiny Utils
  #' 
  #' Return whichever object user chose to enter in topic selection.
  #' @param new_topic_in character vec
  #' @param topic_in character vec
  #' @return character vec
  if(new_topic_in != "") return(new_topic_in) else return(topic_in)
}

genChoices <- function(df){
  #' Shiny Utils
  #' 
  #' Create vector of topic choices to fill UI element.
  #' @param df data.frame
  #' @return character vec
  logMsg("function", "running genChoices()")
  choices_vec <- c("DSA", #Premade choices.
                   "ML",
                   "Stats & Prob")
  choices_vec <- c(choices_vec, unique(df$Topic)) #All existing choices.
  choices_vec <- choices_vec[!is.na(choices_vec)] #Remove all NAs.
  return(choices_vec)
}