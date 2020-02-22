#======================================================================#
####                    STUDY LOG  
#======================================================================#

#========== SetUp ==========#

#Library:
logMsg("global", "loading packages")
library(dplyr)
library(crayon)
library(shinyWidgets)
library(stringr)
library(lubridate)
library(plotly)
library(data.table)
library(DT)

#Options:
logMsg("global", "setting options")
options(stringsAsFactors = FALSE)

#Source project modules:
logMsg("global", "loading scripts")
loadScripts(paths$Code)

#Prepare data:
logMsg("global", "loading and checking log data")
main_df <- readLog(paths)
checkDuplicateEntry(main_df)
topic_choices <- genChoices(main_df)
sys_date <- revString(gsub("-", "/", Sys.Date()))
