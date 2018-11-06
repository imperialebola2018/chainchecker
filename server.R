#
# dependencies
library(shiny)
library(ggplot2)
library(gridExtra)
library(epicontacts)
source('~/Eb_general/TransmissionTree/vis_epicontacts_ggplot.R')

#function for workflow
fun_get_onset = function(input){
  df = data.frame("ID" = input$id)
  
  
  #then add other dates
  if(input$death_avail){
    df$Death = input$death
    df$Onset = as.Date(input$death - input$symptomatic)
    df$Reported_Onset = as.Date(NA)
  } else {
    df$Death = as.Date(NA)
    df$Reported_Onset = input$report_onset
    
    if(input$bleeding){
      df$Onset = as.Date(df$Reported_Onset - input$bleeding_correction)
    } else if(input$diarrhea){
      df$Onset = as.Date(df$Reported_Onset - input$diarrhea_correction)
    } else {
      df$Onset = input$report_onset
    }
  }
  #calculate exposure period
  df$Exposure_min = as.Date(df$Onset - input$max_incubation)
  df$Exposure_max = as.Date(df$Onset - input$min_incubation)
  
  
  return(df)
}

#function to get file in right format
fun_format_file = function(df, input){
  
  #input parameters
  df$bleeding_correction = input$bleeding_correction
  df$diarrhea_correction = input$diarrhea_correction
  df$max_incubation = input$max_incubation
  df$min_incubation = input$min_incubation
  df$symptomatic = input$symptomatic
  
  #true false
  df$death_avail = !is.na(df$death)
  
  return(df)
}

### SERVER ###

# Define server logic required to draw a histogram
function(input, output) {
  
  ### TIMELINE ###
  output$exposure_plot <- renderPlot({
    
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
      #scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
      xlab("Date")
    
    g
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
  
  
  
  ### UPLOAD ###
  
  output$download_ctemplate = downloadHandler(
    filename = function(){
      paste0("contact_template", ".csv")
    },
    content = function(file){
      write.csv(data.frame("from" = "EG1", "to" = "EG2", "type" = "TRUE"), file, row.names = FALSE )
    }
  )
  
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
  
  ### ANALYSIS - WINDOWS ###
  
  output$onset_plot = renderPlotly({
    
    file_upload = input$file_line
    if(is.null(file_upload)){
      
    } else {
      df = read.csv(file_upload$datapath, stringsAsFactors = FALSE)
      df$report_onset = as.Date(df$report_onset, format = "%d/%m/%Y")
      df$death = as.Date(df$death, format = "%d/%m/%Y")
      
      #format
      input_all = data.frame("bleeding_correction" = input$bleeding_correction_all,
                             "diarrhea_correction" = input$diarrhea_correction_all,
                             "symptomatic" = input$symptomatic_all,
                             "min_incubation" = input$min_incubation_all,
                             "max_incubation" = input$max_incubation_all)
      df = fun_format_file(df, input_all)
      
      #get onset
      df_out = NULL
      for(i in 1:nrow(df)){
        tmp = fun_get_onset(df[i,])
        df_out = rbind(df_out, tmp)
      }
      
      g = ggplot(df_out) 
      g = g + geom_rect(aes(xmin = Exposure_min,
                            xmax = Exposure_max,
                            ymin = ID, 
                            ymax = ID,
                            color = "Exposure period")) +
        geom_point( aes( x = Death,
                         y = ID,
                         color = "Death")) +
        geom_point(aes(x = Onset,
                       y = ID,
                       color = "Estimated onset")) +
        geom_point(aes(x = Reported_Onset,
                       y = ID,
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
  
  output$download_window = downloadHandler(
    filename = function(){
      "linelist_with_estimated_exposure.csv"
    },
    content = function(file){
      file_upload = input$file_line
      if(is.null(file_upload)){
        
      } else {
        df = read.csv(file_upload$datapath, stringsAsFactors = FALSE)
        df$report_onset = as.Date(df$report_onset, format = "%d/%m/%Y")
        df$death = as.Date(df$death, format = "%d/%m/%Y")
        
        #format
        input_all = data.frame("bleeding_correction" = input$bleeding_correction_all,
                               "diarrhea_correction" = input$diarrhea_correction_all,
                               "symptomatic" = input$symptomatic_all,
                               "min_incubation" = input$min_incubation_all,
                               "max_incubation" = input$max_incubation_all)
        df = fun_format_file(df, input_all)
        #get onset
        df_out = NULL
        for(i in 1:nrow(df)){
          tmp = fun_get_onset(df[i,])
          df_out = rbind(df_out, tmp)
        }
        
        df_out2 = tibble::add_column(df, onset = df_out$Onset, 
                                     exposure_min = df_out$Exposure_min, 
                                     exposure_max = df_out$Exposure_max,
                                     .after = "report_onset")
        
        
        write.csv(df_out2, file, row.names = FALSE)
      }
      
    }
  )
  ### ANALYSIS - TREE ###
  output$tree = renderPlotly({
    
    file_uploadl = input$file_line
    file_uploadc = input$file_contact
    
    if(is.null(file_uploadl) | is.null(file_uploadc)){
      
    } else {
      
      #adjust or not?
      if(input$adjust_tree){
        df = read.csv(file_uploadl$datapath, stringsAsFactors = FALSE, na.strings = "")
        df$report_onset = as.Date(df$report_onset, format = "%d/%m/%Y")
        df$death = as.Date(df$death, format = "%d/%m/%Y")
        
        #format
        input_all = data.frame("bleeding_correction" = input$bleeding_correction_all,
                               "diarrhea_correction" = input$diarrhea_correction_all,
                               "symptomatic" = input$symptomatic_all,
                               "min_incubation" = input$min_incubation_all,
                               "max_incubation" = input$max_incubation_all)
        df = fun_format_file(df, input_all)
        
        #get onset
        df_out = NULL
        for(i in 1:nrow(df)){
          tmp = fun_get_onset(df[i,])
          df_out = rbind(df_out, tmp)
        }
        
        df_out2 = tibble::add_column(df, onset = df_out$Onset, 
                                     exposure_min = df_out$Exposure_min, 
                                     exposure_max = df_out$Exposure_max,
                                     .after = "report_onset")
        
        df = df_out2
        df$onset = as.Date(df$onset, format = "%d/%m/%Y")
        
      } else {
        df = read.csv(file_uploadl$datapath, stringsAsFactors = FALSE, na.strings = "")
        df$onset = as.Date(df$report_onset, format = "%d/%m/%Y")
        df$death = as.Date(df$death, format = "%d/%m/%Y")
      }
      
      linelist = df
      contacts = read.csv(file_uploadc$datapath, stringsAsFactors = FALSE, na.strings = "")
      
      #covering extras
      linelist$name = linelist$code = linelist$id
      contacts[is.na(contacts)] = FALSE
      
      #make epicontacts
      x = epicontacts::make_epicontacts(linelist, contacts)
      
      #visualise
      vis_epicontacts_ggplot(x,
                             group = input$group, 
                             contactsgroup = input$groupcontact,
                             anon = TRUE,
                             incubation = input$min_incubation_tree) %>% 
        layout(height = 800)
    }
  })
  
  
  output$linelist_group = renderUI({
    file_uploadl = input$file_line
    file_uploadc = input$file_contact
    
    if(is.null(file_uploadl) | is.null(file_uploadc)){
      
    } else {
      
      #adjust or not?
      if(input$adjust_tree){
        df = read.csv(file_uploadl$datapath, stringsAsFactors = FALSE, na.strings = "")
        df$report_onset = as.Date(df$report_onset, format = "%d/%m/%Y")
        df$death = as.Date(df$death, format = "%d/%m/%Y")
        
        #format
        input_all = data.frame("bleeding_correction" = input$bleeding_correction_all,
                               "diarrhea_correction" = input$diarrhea_correction_all,
                               "symptomatic" = input$symptomatic_all,
                               "min_incubation" = input$min_incubation_all,
                               "max_incubation" = input$max_incubation_all)
        df = fun_format_file(df, input_all)
        
        #get onset
        df_out = NULL
        for(i in 1:nrow(df)){
          tmp = fun_get_onset(df[i,])
          df_out = rbind(df_out, tmp)
        }
        
        df_out2 = tibble::add_column(df, onset = df_out$Onset, 
                                     exposure_min = df_out$Exposure_min, 
                                     exposure_max = df_out$Exposure_max,
                                     .after = "report_onset")
        
        df = df_out2
        df$onset = as.Date(df$onset, format = "%d/%m/%Y")
        
      } else {
        df = read.csv(file_uploadl$datapath, stringsAsFactors = FALSE, na.strings = "")
        df$onset = as.Date(df$report_onset, format = "%d/%m/%Y")
        df$death = as.Date(df$death, format = "%d/%m/%Y")
      }
      
    }
    
    
    
    selectInput("group", "Enter a characteristic to show on the plot: ", names(df))
  })
  
  
  output$contact_group = renderUI({
    file_uploadl = input$file_line
    file_uploadc = input$file_contact
    
    if(is.null(file_uploadl) | is.null(file_uploadc)){
      
    } else {
      
      contacts = read.csv(file_uploadc$datapath, stringsAsFactors = FALSE, na.strings = "")
    }
    
    selectInput("groupcontact", "Enter a transmission type to show on the plot: ", names(contacts))
  })
  

  ### METHOD ###
  output$Diagram = renderImage({
    list(src = "./images/Diagram.png",
         alt = "Diagram of relative dates.")
  }, deleteFile = FALSE)
  
}


