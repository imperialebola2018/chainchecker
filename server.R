#
# dependencies
library(shiny)
library(ggplot2)
library(epicontacts)
library(tibble)
library(dplyr)
source('vis_epicontacts_ggplot.R')
source('calculator_functions.R')
source('internals.R')

### SERVER ###
function(input, output) {
  
  ### TIMELINE ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$exposure_plot <- renderPlotly({
    
    df = fun_get_onset(input)
    
    #plot
    g = ggplot(df) 
    g = g + geom_rect(aes(xmin = exposure_date_min,
                          xmax = exposure_date_max,
                          ymin = id, 
                          ymax = id,
                          color = "Exposure"),
                      size = 1.1) +
      geom_point( aes( x = death_date,
                       y = id,
                       color = "Death"),
                  size = 5) +
      geom_point( aes( x = exposure_date_min,
                       y = id,
                       color = "Exposure"), size = 0.1) +
      geom_point( aes( x = exposure_date_max,
                       y = id,
                       color = "Exposure"), size = 0.1) +
      geom_point(aes(x = onset_date,
                     y = id,
                     color = "Estimated onset"),
                 size = 5) +
      geom_point(aes(x = reported_onset_date,
                     y = id,
                     color = "Reported onset"),
                 size = 5, shape = 4, stroke = 2) +
      ylab("Identifier") +
      labs(colour = "Key")+
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("Date")
    
    plotly::ggplotly(g, height = 400) 
  })
  
  output$estimated_onset = renderText({
    paste0("Estimated onset of symptoms for these parameters is ", 
           format(fun_get_onset(input)$onset_date, format = "%d %B"), 
           ".")
  })
  
  output$exposure_window = renderText({
    paste0("The exposure window for these parameters is ", 
           format(fun_get_onset(input)$exposure_date_min, format = "%d %B"), 
           " to ",
           format(fun_get_onset(input)$exposure_date_max, format = "%d %B"),
           ".")
  })
  
  
  
  ### UPLOAD ###-----------------------------------------------------------------------------------
  
  # DOWNLOAD LINELIST #
  output$download_ctemplate = downloadHandler(
    filename = function(){
      "contact_template.csv"
    },
    content = function(file){
      write.csv(data.frame("from" = c("EG1","EG1", "EG2", "EG3"), 
                           "to" = c("EG2", "EG4", "EG5", "EG6") ,
                           "contact_of_type1" = c("TRUE", "FALSE", "FALSE", 
                                                  "FALSE"),
                           "contact_of_type2" = c("FALSE", "FALSE", "TRUE", 
                                                  "FALSE") ), file, row.names = FALSE )
    }
  )
  
  # DOWNLOAD CONTACT #
  output$download_ltemplate = downloadHandler(
    filename = function(){
      "linelist_template.csv"
    },
    content = function(file){
      write.csv(data.frame("id" = c("EG1", "EG2", "EG3", "EG4", "EG5", "EG6"), 
                           "reported_onset_date" = format(c(Sys.Date()-7, Sys.Date()-4, Sys.Date()-3, 
                                                            Sys.Date()-2, Sys.Date()-1, Sys.Date()), 
                                                          format = "%d/%m/%Y"),
                           "death_date" = format(c(Sys.Date(), NA, Sys.Date()-2, 
                                                   Sys.Date()-1, Sys.Date(), NA), 
                                                 format = "%d/%m/%Y"),
                           "bleeding_at_reported_onset" = c("TRUE", "FALSE", "FALSE", 
                                                            "FALSE", "TRUE", "FALSE"),
                           "diarrhea_at_reported_onset" = c("FALSE", "FALSE", "TRUE", 
                                                            "FALSE","TRUE", "FALSE")), file, row.names = FALSE )
    }
  )
  
  ### ANALYSIS - WINDOWS ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$onset_plot = renderPlotly({
    
    df_out = fun_import_adjust(input)
    
    df_out = df_out %>% arrange(exposure_date_min)
    
    g = ggplot(df_out) 
    g = g + geom_rect(aes(xmin = exposure_date_min,
                          xmax = exposure_date_max,
                          ymin = id, 
                          ymax = id,
                          color = "Exposure")) +
      geom_point( aes( x = death_date,
                       y = id,
                       color = "Death")) +
      geom_point( aes( x = exposure_date_min,
                       y = id,
                       color = "Exposure"), size = 0.1) +
      geom_point( aes( x = exposure_date_max,
                       y = id,
                       color = "Exposure"), size = 0.1) +
      geom_point(aes(x = onset_date,
                     y = id,
                     color = "Estimated onset")) +
      geom_point(aes(x = reported_onset_date,
                     y = id,
                     color = "Reported onset"),
                 shape = 4, stroke = 2) +
      ylab("Identifier") +
      labs(colour = "Key")+
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("Date")
    
    plotly::ggplotly(g, height = 800) 
    
  })
  
  # DOWNLOAD #
  output$download_window = downloadHandler(
    filename = function(){
      paste0("linelist_with_estimated_exposure_", Sys.Date(), ".csv")
      },
    content = function(file){
      
      df_out = fun_import_adjust(input)
      
      write.csv(df_out, file, row.names = FALSE)
      
      
    }
  )
  
  ### ANALYSIS - TREE ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$tree = renderPlotly({
    
    fun_make_tree(input)
    
  })
  
  # DROP DOWN MENU LINELIST #
  output$linelist_group = renderUI({
    
    #adjust or not?
    if(input$adjust_tree){
      linelist = fun_import_adjust(input)
    } else {
      linelist = check_line_upload(input$file_line)
    }
    
    selectInput("group", "Enter a characteristic to show on the plot: ", names(linelist))
  })
  
  # DROP DOWN MENU CONTACT #
  output$contact_group = renderUI({
    
    contacts = check_contacts_upload(input$file_contact)
    
    selectInput("groupcontact", "Enter a transmission type to show on the plot: ", c(names(contacts),NA), selected = NA )
  })
  
  # DOWNLOAD #
  output$tree_download = downloadHandler(
    filename = function(){ 
      paste0("Transmission_Tree_", Sys.Date(), ".html")
      },
    content = function(file){
      
      p = fun_make_tree(input)
      
      htmlwidgets::saveWidget(as.widget(p), file)
      
    }
  )
  
  
}


