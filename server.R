#
# dependencies
library(shiny)
library(ggplot2)
library(gridExtra)

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


### SERVER ###

# Define server logic required to draw a histogram
function(input, output) {
  
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
  
  # output$prob_intro = renderText({
  #   "We may be interested in how likely the reported onset date is, 
  #   given the date of death and the average incubation and symptomatic 
  #   periods as shown. To calculate this, we assign probability 
  #   distributions to both periods and use these to calculate the 
  #   probability of the reported onset given the observed date of death."
  # })
  # 
  # output$dist_plot = renderPlot({
  #   
  #   df_dist = data.frame("Days" = 0:50,
  #                        "Density_Symptomatic" = dlnorm(0:50,
  #                                                       meanlog = log(input$symptomatic),
  #                                                       sdlog = log(4)),
  #                        "Density_Incubation" = dlnorm(0:50,
  #                                                      meanlog = log(input$incubation),
  #                                                      sdlog = log(4)))
  #   
  #   g1 = ggplot(df_dist) + 
  #     geom_col( aes(x = Days, y = Density_Symptomatic, 
  #                   fill = "Symptomatic")) +
  #     geom_vline(xintercept = as.numeric(input$death - input$report_onset)) + 
  #     ylab("Density") +
  #     labs(fill = "Key")+
  #     theme(panel.background = element_rect(fill = "white", colour = "grey50"),
  #           text = element_text(size = 14),
  #           axis.text.x = element_text(angle = 45, hjust = 1)) +
  #     xlab("Duration") +
  #     ylim(0,0.1)
  #   
  #   g1
  # })
  
  output$download_ctemplate = downloadHandler(
    filename = function(){
      paste0("contact_template", ".csv")
    },
    content = function(file){
      write.csv(data.frame("from" = "EG1", "to" = "EG2"), file, row.names = FALSE )
    }
  )
  
  output$download_ltemplate = downloadHandler(
    filename = function(){
      paste0("linelist_template", ".csv")
    },
    content = function(file){
      write.csv(data.frame("id" = "EG1", 
                           "onset" = Sys.Date(),
                           "death" = Sys.Date(),
                           "characteristic1" = "eg. age",
                           "characteristic2" = "eg. location"), file, row.names = FALSE )
    }
  )
  
  output$death_onset_plot = renderPlot({
    
    file_upload = input$file_line
    if(is.null(file_upload)){
      print("No linelist uploaded")
    } else {
      df = read.csv(file_upload$datapath, stringsAsFactors = FALSE)
      
      df$onset = as.Date(df$onset)
      df$death = as.Date(df$death)
      
      g = ggplot(df) +
        geom_rect( aes( xmin = death-input$symptomatic,
                        xmax = death,
                        ymin = id,
                        ymax = id,
                        color = "Symptomatic period"), 
                   size = 1.1) +
        geom_rect( aes( xmin = death-input$symptomatic-input$incubation,
                        xmax = death-input$symptomatic,
                        ymin = id,
                        ymax = id,
                        color = "Incubation period"), 
                   size = 1.1) +
        
        geom_point( aes( x = onset,
                         y = id,
                         color = "Reported onset"),
                    size = 5) +
        
        geom_point( aes( x = death,
                         y = id,
                         color = "Death"),
                    size = 5) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
        xlab("Date")
      
      g
    }
  })
  
}


