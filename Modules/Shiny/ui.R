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
                              choices = c("Day", "Week", "Month"),
                              selected = "Week"
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
                           plotly::plotlyOutput("overview_plot"),
                           br(),
                           hr()
                         )
                ),
                
                #---------- Stats ----------#
                tabPanel(type = "stats",
                         "Stats",
                         h2("Stats"),
                         br(),
                         
                         #Alltime plot row:
                         fluidRow(
                           h4("All Time"),
                           plotly::plotlyOutput("alltime_plot"),
                           br(),
                           hr()
                         )
                ),
                
                #---------- Topics ----------#
                tabPanel(type = "topics",
                         "Topics",
                         h2("Topics"),
                         br(),
                         
                         #Overview plot row:
                         fluidRow(
                           h4("Topics"),
                           plotly::plotlyOutput("topics_bar_plot"),
                           br(),
                           hr()
                         )
                )
      )
    )
  )
)
