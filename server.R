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
    
    df = check_date_order(df)
    
    p = fun_plot_exposure_windows(df, height=400)
    
    p
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
    
    df_out = check_date_order(df_out)
    
    p = fun_plot_exposure_windows(df_out, height=800)
    
    p
    
  })
  
  # DOWNLOAD #
  output$download_window = downloadHandler(
    filename = function(){
      paste0("linelist_with_estimated_exposure_", Sys.Date(), ".csv")
      },
    content = function(file){
      
      df_out = fun_import_adjust(input)
      
      df_out = check_date_order(df_out)
      
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


