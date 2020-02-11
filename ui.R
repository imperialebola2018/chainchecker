library(shiny)
library(shinythemes)
library(plotly)
library(shinycssloaders)

  
navbarPage(title = "chainchecker",
           selected = "Home",
           
           theme = shinytheme("cerulean"),
          
           
           tabPanel("Home",
                    icon = icon("home"),
                    radioButtons(inputId = "language", 
                                 label = "",
                                 choiceValues = c("en", "fr"),
                                 choiceNames = c("English", "Français"),
                                 selected = "en",
                                 inline = TRUE),
                    
                    uiOutput("aboutUI")
           ),
           
           # Sidebar with a slider input for number of bins 
           tabPanel("Timeline",
                    
                    icon = icon("stream"),
                    
                    sidebarPanel(
                      
                      uiOutput("min_incubUI"),
                      
                      uiOutput("max_incubUI"),
                      
                      uiOutput("onset_deathUI"),
                      
                      uiOutput("idUI"),
                      
                      #conditions
                      uiOutput("dod_avail_checkUI"),
                      
                      conditionalPanel(
                        condition = "input.death_avail == true",
                        uiOutput("dodUI")
                      ),
                      
                      conditionalPanel(
                        condition = "input.death_avail == false",
                        
                        uiOutput("dosoUI"),
                        
                        uiOutput("bleeding_checkUI"),
                        
                        conditionalPanel(
                          condition = "input.bleeding_at_reported_onset == true",
                          
                          uiOutput("onset_bleedingUI")),
                        
                        conditionalPanel(
                          condition = "input.bleeding_at_reported_onset == false",
                          
                          uiOutput("diarrhea_checkUI"),
                          
                          conditionalPanel(
                            condition = "input.diarrhea_at_reported_onset == true",
                            
                            uiOutput("onset_diarrheaUI"))
                        )
                      ),
                      
                      uiOutput("hoverUI")
                      
                    ),
                    mainPanel(plotlyOutput("exposure_plot"),
                              textOutput("estimated_onset"),
                              textOutput("exposure_window"))
           ),
           tabPanel("Upload",
                    
                    icon = icon("upload"),
                    
                    sidebarPanel(
                      uiOutput("download_lUI"),
                      br(),br(),
                      uiOutput("download_cUI"),
                      
                      uiOutput("upload_lUI"),
                      uiOutput("upload_cUI")),
                    
                    mainPanel(
                      uiOutput("upload_guideUI")
                    )
           ),
           tabPanel("Exposure windows",
                    
                    icon = icon("poll-h"),
                    
                    sidebarPanel(
                      uiOutput("check_dates_reportedUI"),
                      
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
                      
                      
                      br(),br(),
                      
                      downloadButton("download_window", "Download results as .csv"),
                      
                      br(),br(),
                      span("Dates of death that are inconsistent with reported onset date
                           are denoted with a square.", 
                           style="color:red")
                      
                    ),
                    mainPanel(plotlyOutput("onset_plot") %>% withSpinner(type = 5, color = "orange"))
           ),
           
           tabPanel("Transmission tree",
                    
                    icon = icon("link"),
                    
                    sidebarPanel(
                      checkboxInput("adjust_tree", 
                                    "Show tree with estimated onset dates."),
                      uiOutput("linelist_group"),
                      uiOutput("contact_group"),
                      uiOutput("tooltip_options"),
                      
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
           
           tabPanel("Cluster plots",
                    
                    icon = icon("project-diagram"),
                    
                    sidebarPanel(
                      span("Hover over the plot for more information on each point.", 
                           style="color:blue"),
                      br(),br(),
                      downloadButton("cluster_download", "Download Clusters as HTML")
                      
                    ),
                    mainPanel(plotlyOutput("network",width="1200px",height="800px") %>% 
                                withSpinner(type = 5, color = "orange"))),
           
           tabPanel("Cluster Information",
                    
                    icon = icon("table"),
                    
                    DT::dataTableOutput("networkTable")
           ),
           tabPanel("Method and definitions",
                    
                    icon = icon("book"),
                    
                    uiOutput("methodUI")
           )
           
)
