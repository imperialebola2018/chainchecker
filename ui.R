library(shiny)
library(shinythemes)
library(plotly)


navbarPage("chainchecker", 
           selected = "Timeline",
           
           
           #making sure the figue is not huge
           tags$head(tags$style(
             type="text/css",
             "#Diagram img {max-height: 100%; height: 100%; width: auto}"
           )),
           
           theme = shinytheme("cerulean"),
           
           # Sidebar with a slider input for number of bins 
           tabPanel("Timeline",
                    sidebarPanel(
                      
                      #standard inputs
                      numericInput("min_incubation",
                                   "Minimum incubation period (days):",
                                   value = 4, min = 0, max = 365),
                      numericInput("max_incubation",
                                   "Maximum incubation period (days):",
                                   value = 21, min = 0, max = 365),
                      numericInput("days_onset_to_death",
                                   "Mean period from onset to death (days):",
                                   value = 9, min = 0, max = 365),
                      
                      textInput("id",
                                "Identifier:",
                                value = "EG1"),
                      
                      #conditions
                      checkboxInput("death_avail", 
                                    "Check box if the date of death is available", 
                                    value = TRUE), 
                      conditionalPanel(
                        condition = "input.death_avail == true",
                        dateInput("death_date",
                                  "Date of death:",
                                  value = Sys.Date())
                      ), 
                      conditionalPanel(
                        condition = "input.death_avail == false",
                        dateInput("reported_onset_date",
                                  "Reported date of symptom onset:",
                                  value = Sys.Date()-7),
                        checkboxInput("bleeding_at_reported_onset", 
                                      "Check box if the individual was bleeding when onset was reported*.", 
                                      value = TRUE),
                        conditionalPanel(
                          condition = "input.bleeding_at_reported_onset == true",
                          numericInput("days_onset_to_bleeding",
                                       "Mean time from symptom onset to bleeding (days):",
                                       value = 6, min = 0, max = 365)
                        ),
                        conditionalPanel(
                          condition = "input.bleeding_at_reported_onset == false",
                          checkboxInput("diarrhea_at_reported_onset", 
                                        "Check box if the individual had diarrhea when onset was reported."),
                          conditionalPanel(
                            condition = "input.diarrhea_at_reported_onset == true",
                            numericInput("days_onset_to_diarrhea",
                                         "Mean time from symptom onset to diarrhea (days):",
                                         value = 4, min = 0, max = 365)
                          )
                        )
                      ),
                      
                      span("Hover over the plot for more information on each point.", style="color:blue")
                      
                    ),
                    mainPanel(plotlyOutput("exposure_plot"),
                              textOutput("estimated_onset"),
                              textOutput("exposure_window"))
           ),
           tabPanel("Upload",
                    sidebarPanel(
                    downloadButton("download_ctemplate", "Download contacts template"),
                    downloadButton("download_ltemplate", "Download linelist template"),
                    fileInput("file_line", h3("Upload linelist")),
                    fileInput("file_contact", h3("Upload contacts"))),
                    mainPanel(includeMarkdown("Documentation/Upload_Guidelines.md"))
           ),
           tabPanel("Exposure windows for uploaded linelist",
                    
                    sidebarPanel(
                      
                      #standard inputs
                      numericInput("min_incubation_all",
                                   "Minimum incubation period (days):",
                                   value = 4, min = 0, max = 365),
                      numericInput("max_incubation_all",
                                   "Maximum incubation period (days):",
                                   value = 21, min = 0, max = 365),
                      numericInput("days_onset_to_death_all",
                                   "Mean period from onset to death (days):",
                                   value = 9, min = 0, max = 365),
                      numericInput("days_onset_to_bleeding_all",
                                   "Mean time from symptom onset to bleeding (days):",
                                   value = 6, min = 0, max = 365),
                      numericInput("days_onset_to_diarrhea_all",
                                   "Mean time from symptom onset to diarrhea (days):",
                                   value = 4, min = 0, max = 365),
                      textInput("ID1_onset_window",
                                "Enter a pair of identifiers to compare",
                                placeholder = "EG1"),
                      textInput("ID2_onset_window",
                                "", placeholder = "EG2"),
                      
                      span("Hover over the plot for more information on each point.", 
                           style="color:blue"),
                      
                      br(),br(),
                      
                      downloadButton("download_window", "Download results as .csv"),
                      
                      br(),br(),
                      span("Dates of death that are inconsistent with reported onset date
                           are denoted with a square.", 
                           style="color:red")

                    ),
                    mainPanel(plotlyOutput("onset_plot"))
           ),
           tabPanel("Transmission tree for uploaded linelist and contacts",
                    sidebarPanel(
                      checkboxInput("adjust_tree", 
                                    "Show tree with estimated onset dates."),
                      uiOutput("linelist_group"),
                      uiOutput("contact_group"),
                      span("Hover over the plot for more information on each point.", 
                           style="color:blue"),
                      
                      br(),br(),
                      downloadButton("tree_download", "Download Tree as HTML"),
                      
                      downloadButton("contact_download", 
                                     "Download contact inconsistencies as .csv")
                    ),
                    mainPanel(plotlyOutput("tree"))),
           tabPanel("Method and definitions",
                    includeMarkdown("Documentation/Methods.md")
           )
           
)
