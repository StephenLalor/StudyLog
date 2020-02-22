#========== Run App ==========#
library(shiny)
source(paste0(getwd(), "/Modules/Code/Utils/utils.R"))
paths <- defPaths()
runApp(paste0(getwd(), "/Modules/Shiny/"), launch.browser = TRUE)