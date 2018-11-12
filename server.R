#
# dependencies
library(shiny)
library(ggplot2)
library(epicontacts)
library(tibble)
source('vis_epicontacts_ggplot.R')
source('calculator_functions.R')

### SERVER ###
function(input, output) {
  
  ### TIMELINE ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$exposure_plot <- renderPlotly({
    
    df = fun_get_onset(input)
    
    #plot
    g = ggplot(df) 
    g = g + geom_rect(aes(xmin = Exposure_min,
                          xmax = Exposure_max,
                          ymin = ID, 
                          ymax = ID,
                          color = "Exposure period"),
                      size = 1.1) +
      geom_point( aes( x = Death,
                       y = ID,
                       color = "Death"),
                  size = 5) +
      geom_point(aes(x = Onset,
                     y = ID,
                     color = "Estimated onset"),
                 size = 5) +
      geom_point(aes(x = Reported_Onset,
                     y = ID,
                     color = "Reported onset"),
                 size = 5, shape = 4, stroke = 2) +
      ylab("Identifier") +
      labs(colour = "Key")+
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("Date")
    
    ggplotly(g)
  })
  
  output$estimated_onset = renderText({
    paste0("Estimated onset of symptoms for these parameters is ", 
           format(fun_get_onset(input)$Onset, format = "%d %B"), 
           ".")
  })
  
  output$exposure_window = renderText({
    paste0("The exposure window for these parameters is ", 
           format(fun_get_onset(input)$Exposure_min, format = "%d %B"), 
           " to ",
           format(fun_get_onset(input)$Exposure_max, format = "%d %B"),
           ".")
  })
  
  
  
  ### UPLOAD ###-----------------------------------------------------------------------------------
  
  # DOWNLOAD LINELIST #
  output$download_ctemplate = downloadHandler(
    filename = function(){
      paste0("contact_template", ".csv")
    },
    content = function(file){
      write.csv(data.frame("from" = "EG1", 
                           "to" = "EG2", 
                           "type" = "TRUE"), file, row.names = FALSE )
    }
  )
  
  # DOWNLOAD CONTACT #
  output$download_ltemplate = downloadHandler(
    filename = function(){
      paste0("linelist_template", ".csv")
    },
    content = function(file){
      write.csv(data.frame("id" = "EG1", 
                           "report_onset" = Sys.Date(),
                           "death" = Sys.Date(),
                           "bleeding" = "eg. TRUE",
                           "diarrhea" = "eg. FALSE"), file, row.names = FALSE )
    }
  )
  
  ### ANALYSIS - WINDOWS ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$onset_plot = renderPlotly({
    
    file_upload = input$file_line
    
    if(!is.null(file_upload)){
      
      df_out = fun_import_adjust(input)
      
      g = ggplot(df_out) 
      g = g + geom_rect(aes(xmin = exposure_min,
                            xmax = exposure_max,
                            ymin = id, 
                            ymax = id,
                            color = "Exposure period")) +
        geom_point( aes( x = death,
                         y = id,
                         color = "Death")) +
        geom_point(aes(x = onset,
                       y = id,
                       color = "Estimated onset")) +
        geom_point(aes(x = report_onset,
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
    }
  })
  
  # DOWNLOAD #
  output$download_window = downloadHandler(
    filename = function(){paste0("linelist_with_estimated_exposure_", Sys.Date(), ".csv")},
    content = function(file){
      
      file_upload = input$file_line
      
      if(!is.null(file_upload)){
        
        df_out = fun_import_adjust(input)
        
        write.csv(df_out, file, row.names = FALSE)
      }
      
    }
  )
  
  ### ANALYSIS - TREE ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$tree = renderPlotly({
    
    fun_make_tree(input)
    
  })
  
  # DROP DOWN MENU LINELIST #
  output$linelist_group = renderUI({
    
    file_uploadl = input$file_line
    
    if(!is.null(file_uploadl)){
      
      #adjust or not?
      if(input$adjust_tree){
        
        linelist = fun_import_adjust(input)
        
      } else {
        linelist = read.csv(file_uploadl$datapath, stringsAsFactors = FALSE, na.strings = "")
        
        linelist = linelist %>% mutate(report_onset = as.Date(report_onset, format = "%d/%m/%Y"),
                                       death = as.Date(death, format = "%d/%m/%Y"))
      }
      
    }
    
    selectInput("group", "Enter a characteristic to show on the plot: ", names(linelist))
  })
  
  # DROP DOWN MENU CONTACT #
  output$contact_group = renderUI({
    
    file_uploadc = input$file_contact
    
    if(!is.null(file_uploadc)){
      contacts = read.csv(file_uploadc$datapath, stringsAsFactors = FALSE, na.strings = "")
    }
    
    selectInput("groupcontact", "Enter a transmission type to show on the plot: ", c(names(contacts),NA), selected = NA )
  })
  
  # DOWNLOAD #
  output$tree_download = downloadHandler(
    filename = function(){ paste0("Transmission_Tree_", Sys.Date(), ".html")},
    content = function(file){
      
      p = fun_make_tree(input)
      
      htmlwidgets::saveWidget(as.widget(p), file)
      
    }
  )
  
  
}


