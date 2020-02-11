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
                      uiOutput("min_incub_allUI"),
                      
                      uiOutput("max_incub_allUI"),
                      
                      uiOutput("onset_death_allUI"),
                      
                      uiOutput("onset_bleeding_allUI"),
                      
                      uiOutput("onset_diarrhea_allUI"),
                      
                      uiOutput("enter_id1UI"),
                      
                      textInput("ID2_onset_window",
                                "", placeholder = "EG2"),
                      
                      
                      br(),br(),
                      
                      uiOutput("download_windowUI"),
                      
                      br(),br(),
                      
                      uiOutput("dates_of_deathUI")
                      
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
