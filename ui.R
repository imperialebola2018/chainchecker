library(shiny)
# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Calculate onset date"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateInput("death",
                "Date of death:",
                value = Sys.Date()),
      dateInput("report_onset",
                "Reported onset date:",
                value = (Sys.Date()-7)),
      numericInput("symptomatic",
                   "Duration of symptomatic period:",
                   value = 7),
      numericInput("incubation",
                   "Duration of incubation period:",
                   value = 21),
      textInput("id",
                "Identifier:",
                value = "ID1")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Timeline", 
                           plotOutput("onset_plot"),
                           textOutput("earliest_onset")),
                  tabPanel("Probability", 
                           textOutput("prob_intro"),
                           plotOutput("dist_plot"),
                           textOutput("prob_onset")),
                  tabPanel("Upload",
                           downloadButton("download_ctemplate", "Download contacts template"),
                           downloadButton("download_ltemplate", "Download linelist template"),
                           fileInput("file_line", h3("Upload linelist")),
                           fileInput("file_contact", h3("Upload contacts"))),
                  tabPanel("Analysis",
                           plotOutput("death_onset_plot")
                  )
      )
      
    )
  )
)