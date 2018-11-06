library(shiny)
library(plotly)
# Define UI for application that draws a histogram
navbarPage("Calculating Ebola exposure dates", 
           selected = "Timeline",
           
           
           #making sure the figue is not huge
           tags$head(tags$style(
             type="text/css",
             "#Diagram img {max-height: 100%; height: 100%; width: auto}"
           )),
           

           
           # Sidebar with a slider input for number of bins 
           tabPanel("Timeline",
                    sidebarPanel(
                      
                      #standard inputs
                      numericInput("min_incubation",
                                   "Duration of incubation period min:",
                                   value = 14),
                      numericInput("max_incubation",
                                   "Duration of incubation period max:",
                                   value = 21),
                      numericInput("symptomatic",
                                   "Duration of period from onset to death:",
                                   value = 9),
                      
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
                                  value = Sys.Date()-7),
                        checkboxInput("bleeding", "Check box if the individual was bleeding*.", value = TRUE),
                        conditionalPanel(
                          condition = "input.bleeding == true",
                          numericInput("bleeding_correction",
                                       "Estimate of time from onset to bleeding:",
                                       value = 6)
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
                    mainPanel(plotOutput("exposure_plot"),
                              textOutput("estimated_onset"),
                              textOutput("exposure_window"))
           ),
           tabPanel("Upload",
                    downloadButton("download_ctemplate", "Download contacts template"),
                    downloadButton("download_ltemplate", "Download linelist template"),
                    fileInput("file_line", h3("Upload linelist")),
                    fileInput("file_contact", h3("Upload contacts"))
           ),
           tabPanel("Exposure windows for uploaded linelist",
                    
                    sidebarPanel(
                      
                      #standard inputs
                      numericInput("min_incubation_all",
                                   "Duration of incubation period min:",
                                   value = 14),
                      numericInput("max_incubation_all",
                                   "Duration of incubation period max:",
                                   value = 21),
                      numericInput("symptomatic_all",
                                   "Duration of period from onset to death:",
                                   value = 9),
                      numericInput("bleeding_correction_all",
                                   "Estimate of time from onset to bleeding:",
                                   value = 6),
                      numericInput("diarrhea_correction_all",
                                   "Estimate of time from onset to diarrhea:",
                                   value = 4),
                      
                      downloadButton("download_window", "Download")

                    ),
                    mainPanel(plotlyOutput("onset_plot"))
           ),
           tabPanel("Transmission tree for uploaded linelist and contacts",
                    sidebarPanel(
                      checkboxInput("adjust_tree", "Show adjusted tree: ", value = TRUE),
                      textInput("group", "Enter a characteristic to show on the plot: ", 
                                value = "classification"),
                      textInput("groupcontact", "Enter a transmission type to show on the plot: ", 
                                value = "NA")
                    ),
                    mainPanel(plotlyOutput("tree"))),
           tabPanel("Method and definitions",
                    "We make certain assumptions about how disease prgresses from one stage to the next. 
                           We assume individuals are exposed, become infectious with some symptoms and then, 
                           in some cases, die. The different stages and definitions are shown below.",
                    br(),
                    imageOutput("Diagram"),
                    br(),
                    "*Bleeding is defined as ..."
           )
           
)
