library(shiny)
# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Calculating Ebola exposure dates"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #standard inputs
      numericInput("min_incubation",
                   "Duration of incubation period min:",
                   value = 21),
      numericInput("max_incubation",
                   "Duration of incubation period max:",
                   value = 21),
      numericInput("symptomatic",
                   "Duration of period from onset to death:",
                   value = 7),
      
      textInput("id",
                "Identifier:",
                value = "ID1"),
      
      #conditions
      checkboxInput("death_avail", "Check box if the date of death is available", value = TRUE), 
      conditionalPanel(
        condition = "input.death_avail == true",
        dateInput("death",
                  "Date of death:",
                  value = Sys.Date())
      ), 
      conditionalPanel(
        condition = "input.death_avail == false",
        dateInput("report_onset",
                  "Reported onset date:",
                  value = (Sys.Date()-7)),
        checkboxInput("bleeding", "Check box if the individual was bleeding*.", value = TRUE),
        conditionalPanel(
          condition = "input.bleeding == true",
          numericInput("bleeding_correction",
                       "Estimate of time from onset to bleeding:",
                       value = 4)
        ),
        conditionalPanel(
          condition = "input.bleeding == false",
          checkboxInput("diarrhea", "Check box if the individual had diarrhea."),
          conditionalPanel(
            condition = "input.diarrhea == true",
            numericInput("diarrhea_correction",
                         "Estimate of time from onset to diarrhea:",
                         value = 4)
          )
          
        )
        
        
      )
      
      
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