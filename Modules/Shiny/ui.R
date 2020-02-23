# #======================================================================#
# ####                    UI 
# #======================================================================#

ui <- fluidPage(
  
  #========== Title ==========#
  titlePanel("Study Log"),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 radioButtons("sel_time", "Time Period:",
                              choices = c("Day", "Week", "Month")
                 )
    ),
    
    mainPanel(width = 9,
              tabsetPanel(
                
                #---------- Input ----------#
                tabPanel(type = "tabs",
                         "Input",
                         h2("Time Input"),
                         br(),
                         
                         #New row input:
                         fluidRow(
                           h4("Add New Data"),
                           textInput("time_in", "Time:", placeholder = "00h00m00s"),
                           textInput("date_in", "Date:", sys_date),
                           checkboxInput("new_topic_toggle", label = "Add new topic"),
                           conditionalPanel("!input.new_topic_toggle", selectInput("topic_in", "Topic:", choices = topic_choices)),
                           conditionalPanel("input.new_topic_toggle", textInput("new_topic_in", "New Topic:")),
                           actionButton("add_time", "Add Time"),
                           hr()
                         ),
                         
                         #Process old log:
                         fluidRow(
                           h4("Process Old Text File"),
                           textInput("old_log_path", "Path:", placeholder = "Text log path"),
                           actionButton("proc_old_log", "Process")
                         )
                ),
                
                #---------- Overview ----------#
                tabPanel(type = "tabs",
                         "Overview",
                         h2("Overview"),
                         br(),
                         
                         #Overview plot row:
                         fluidRow(
                           h4("Study Time"),
                           plotlyOutput("overview_plot"),
                           br(),
                           hr()
                         ),
                         
                         #Overview tab row:
                         fluidRow(
                           h4("Dataset"),
                           dataTableOutput("overview_tab")
                         )
                ),
                
                #---------- Stats ----------#
                tabPanel(type = "stats",
                         "Stats",
                         h2("Stats"),
                         br(),
                         
                         #Slider row:
                         fluidRow(
                           h4("Select Range"),
                           sliderTextInput("date_range", "Date Range:",
                                           choices = main_df$Date,
                                           selected = c(min(main_df$Date), max(main_df$Date)),
                                           width = "90%"),
                           hr()),
                         
                         #Good and bad section:
                         fluidRow(
                           column(3, style = "padding:20px;", h4("The Good and Bad"), dataTableOutput("good_bad")),
                           column(3, style = "padding:20px;", h4("Averages"), dataTableOutput("sliding_avg")),
                           column(3, style = "padding:20px;", plotlyOutput("zero_pie_plot"))
                         )

                )
      )
    )
  )
)
