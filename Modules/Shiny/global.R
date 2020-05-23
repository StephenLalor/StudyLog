#======================================================================#
####                    STUDY LOG  
#======================================================================#

#========== SetUp ==========#

#Options:
logMsg("global", "setting options")
options(stringsAsFactors = FALSE)

#Source project modules:
logMsg("global", "loading scripts")
loadScripts(paths$Code)

#========== Data ==========#

#Prepare data:
logMsg("global", "loading and checking log data")
main_df <- readLog(paths)
checkDuplicateEntry(main_df)
topic_choices <- genChoices(main_df)
sys_date <- getDate()
target <- 20