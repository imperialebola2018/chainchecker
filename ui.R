library(shiny)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(DT)

navbarPage(title = "chainchecker",
           selected = "Timeline",
           
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
                      
                      textInput("timeline_id",
                                "Identifier:",
                                value = "EG1"),
                      
                      #conditions
                      checkboxInput("death_avail", 
                                    "Check box if the date of death is available", 
                                    value = TRUE), 
                      conditionalPanel(
                        condition = "input.death_avail == true",
                        dateInput("timeline_onset_date",
                                  "Date of death:",
                                  value = Sys.Date(),
                                  format="dd/mm/yyyy")
                      ), 
                      conditionalPanel(
                        condition = "input.death_avail == false",
                        dateInput("timeline_death_date",
                                  "Reported date of symptom onset:",
                                  value = Sys.Date()-7,
                                  format="dd/mm/yyyy"),
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
                      downloadButton("download_vhf_template", "Download VHF template"),
                      fileInput("file_vhf", h3("Upload VHF")))
           ),

           tabPanel("Data Entry",
                    sidebarPanel(
                      actionButton("vhf_submit","Submit (or Create)"),
                      actionButton("vhf_create","New Record"),
                      actionButton("vhf_delete","Delete"),
                      downloadButton("vhf_export", "Export VHF Data as .csv"),
                      uiOutput("sidebar")
                      
                    ),
                    
                    mainPanel(
                      dataTableOutput("vhfTable", width=500)
                    )
           ),
           tabPanel("Exposure windows for uploaded linelist",
                    
                    sidebarPanel(
                      selectInput("caseId", "Case IDs", choices = c("None"), selected = NA, selectize=TRUE, multiple=TRUE),
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
                      
                      span("Hover over the plot for more information on each point.", 
                           style="color:blue"),
                      
                      br(),br(),
                      
                      downloadButton("download_window", "Download results as .csv"),
                      
                      br(),br(),
                      span("Dates of death that are inconsistent with reported onset date
                           are denoted with a square.", 
                           style="color:red")
                      
                    ),
                    mainPanel(plotlyOutput("onset_plot") %>% withSpinner(type = 5, color = "orange"))
           ),
           tabPanel("Transmission tree for uploaded linelist and contacts",
                    sidebarPanel(
                      checkboxInput("adjust_tree", 
                                    "Show tree with estimated onset dates."),
                      uiOutput("linelist_group"),
                      uiOutput("contact_group"),
                      uiOutput("tooltip_options"),
                      span("Hover over the plot for more information on each point.", 
                           style="color:blue"),
                      
                      br(),br(),
                      downloadButton("tree_download", "Download Tree as HTML"),
                      br(),br(),
                      downloadButton("contact_download", 
                                     "Download contact inconsistencies as .csv",
                                     style="white-space: normal;
                                            text-align:left;"),
                      br(),br(),
                      plotOutput("link_legend", height = "100px")
                    ),
                    mainPanel(  plotlyOutput("tree") %>% withSpinner(type = 5, color = "orange") )
           ),
           tabPanel("Nosocomial Transmission Graphs by Case ID",
                    sidebarPanel(
                      checkboxInput("noso_death_avail", 
                                  "Check box if the date of death is available", 
                                  value = TRUE), 
                      numericInput("noso_min_incubation_all",
                                   "Minimum incubation period (days):",
                                   value = 4, min = 0, max = 365),
                      numericInput("noso_max_incubation_all",
                                   "Maximum incubation period (days):",
                                   value = 21, min = 0, max = 365),
                      numericInput("noso_days_onset_to_death_all",
                                   "Mean period from onset to death (days):",
                                   value = 9, min = 0, max = 365),
                      numericInput("noso_days_onset_to_bleeding_all",
                                   "Mean time from symptom onset to bleeding (days):",
                                   value = 6, min = 0, max = 365),
                      numericInput("noso_days_onset_to_diarrhea_all",
                                   "Mean time from symptom onset to diarrhea (days):",
                                   value = 4, min = 0, max = 365),
                      selectInput("noso_case_id",
                                "Enter Case ID to Examine",
                                choices = c("None"))
                    ),
                    mainPanel(  plotlyOutput("noso_case_id_plot") )
           ),
           tabPanel("Nosocomial Transmission Graphs by Hospital",
                    sidebarPanel(
                      checkboxInput("noso_death_avail", 
                                  "Check box if the date of death is available", 
                                  value = TRUE), 
                      numericInput("noso_min_incubation_all",
                                   "Minimum incubation period (days):",
                                   value = 4, min = 0, max = 365),
                      numericInput("noso_max_incubation_all",
                                   "Maximum incubation period (days):",
                                   value = 21, min = 0, max = 365),
                      numericInput("noso_days_onset_to_death_all",
                                   "Mean period from onset to death (days):",
                                   value = 9, min = 0, max = 365),
                      numericInput("noso_days_onset_to_bleeding_all",
                                   "Mean time from symptom onset to bleeding (days):",
                                   value = 6, min = 0, max = 365),
                      numericInput("noso_days_onset_to_diarrhea_all",
                                   "Mean time from symptom onset to diarrhea (days):",
                                   value = 4, min = 0, max = 365),
                      selectInput("noso_hospital_id",
                                "Enter Hospital to Examine", choices=c("None"))
                    ),
                    mainPanel(  plotlyOutput("noso_hospital_plot") )
           ),
           tabPanel("Cluster plots",
                    
                    sidebarPanel(
                      span("Hover over the plot for more information on each point.", 
                           style="color:blue")
                      
                    ),
                    mainPanel(plotlyOutput("network",width="1200px",height="800px") %>% withSpinner(type = 5, color = "orange"))),
           
           tabPanel("Cluster Information",
                    DT::dataTableOutput("networkTable")
           ),
           tabPanel("Method and definitions",
                    includeMarkdown('Documentation/Methods.md')
           )
           
)
