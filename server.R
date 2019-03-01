#
# dependencies
library(plotly)
library(ggplot2)
library(epicontacts)
library(tibble)
library(dplyr)
library(lubridate)
library(data.table)
source('Functions/vis_epicontacts_ggplot.R')
source('Functions/calculator_functions.R')
source('Functions/internals.R')

### SERVER ###
function(input, output) {
  
  ### TIMELINE ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$exposure_plot <- renderPlotly({
    
    df = fun_get_onset(input, default_to_death_date = TRUE)
    
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
  
  # DOWNLOAD CONTACT TEMPLATE #
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
  
  # DOWNLOAD LINELIST TEMPLATE #
  output$download_ltemplate = downloadHandler(
    filename = function(){
      "linelist_template.csv"
    },
    content = function(file){
      write.csv(data.frame("id" = c("EG1", "EG2", "EG3", "EG4", "EG5", "EG6"), 
                           "reported_onset_date" = format(c(Sys.Date()-7, Sys.Date()-4, Sys.Date()-3, 
                                                            Sys.Date()-2, Sys.Date()-1, Sys.Date()), 
                                                          format = "%d/%m/%Y"),
                           "death_date" = format(c(as.Date(NA), as.Date(NA), Sys.Date()-2, 
                                                   Sys.Date()-1, Sys.Date(), as.Date(NA)), 
                                                 format = "%d/%m/%Y"),
                           "bleeding_at_reported_onset" = c("TRUE", "FALSE", "FALSE", 
                                                            "FALSE", "TRUE", "FALSE"),
                           "diarrhea_at_reported_onset" = c("FALSE", "FALSE", "TRUE", 
                                                            "FALSE","TRUE", "TRUE")), file, row.names = FALSE )
    }
  )
  
  ### ANALYSIS - WINDOWS ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$onset_plot = renderPlotly({
    
    df_out = fun_import_adjust(input,
                               default_to_death_date = TRUE)
    
    df_out = check_date_order(df_out)
    
    if(input$ID1_onset_window %in% df_out$id | input$ID2_onset_window %in% df_out$id ){
      df_out = df_out %>% filter(id %in% c(input$ID1_onset_window, input$ID2_onset_window))
    }
    
    p = fun_plot_exposure_windows(df_out, height=700)
    
    p
    
  })
  
  # DOWNLOAD #
  output$download_window = downloadHandler(
    filename = function(){
      paste0("linelist_with_estimated_exposure_", Sys.Date(), ".csv")
    },
    content = function(file){
      
      df_out = fun_import_adjust(input,
                                 default_to_death_date = TRUE)
      
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
    
    linelist = fun_import_adjust(input,
                                 default_to_death_date = input$adjust_tree)
    
    #adjust for epicontacts
    names(linelist)[names(linelist) == 'onset_date'] = 'onset'
    
    #remove added columns for inputs
    vec = names(linelist)
    added_cols = c("days_onset_to_bleeding", 
                   "days_onset_to_diarrhea", 
                   "max_incubation",
                   "min_incubation", 
                   "days_onset_to_death", 
                   "death_avail")
    vec = vec[!vec %in% added_cols]
    
    selectInput("group", 
                "Enter a characteristic to show on the plot: ", 
                vec)
  })
  
  # DROP DOWN MENU CONTACT #
  output$contact_group = renderUI({
    
    linelist = fun_import_adjust(input,
                                 default_to_death_date = input$adjust_tree)
    
    
    contacts = check_contacts_upload(input$file_contact)
    
    #check links are feasible
    contacts = check_exposure_timeline(linelist, contacts, input)
    
    selectInput("groupcontact", 
                "Enter a transmission type to show on the plot: ", 
                names(contacts), selected = "INCONSISTENT" )
  })
  
  output$tooltip_options = renderUI({
    
    linelist = fun_import_adjust(input,
                                 default_to_death_date = input$adjust_tree)

    
    #adjust for epicontacts
    names(linelist)[names(linelist) == 'onset_date'] = 'onset'
    
    #remove added columns for inputs
    vec = names(linelist)
    added_cols = c("days_onset_to_bleeding", 
                   "days_onset_to_diarrhea", 
                   "max_incubation",
                   "min_incubation", 
                   "days_onset_to_death", 
                   "death_avail")
    vec = vec[!vec %in% added_cols]
    
    selectizeInput("tooltip", 
                   "Enter (up to 5) characteristics to show on hover: ", 
                   vec,
                   multiple = TRUE,
                   options = list(maxItems = 5))
  })
  
  #LEGEND FOR LINKS #
  output$link_legend = renderPlot({
    
    par(mar = c(0,0,1,0))
    
    plot(0:1, c(0.5,0.5), col = alpha(rgb(255,165,0, maxColorValue=255), 0.8), 
         ylim = c(0.4,0.6),
         lwd = 10, las = 1,
         type = "l",xaxt='n', yaxt = "n", ylab = "",
         main = input$groupcontact, 
         xlab = "", axes = FALSE) 
    
    lines(0:1, c(0.5,0.5), lwd = 2)
    
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
  
  # CONTACT DOWNLOAD #
  output$contact_download = downloadHandler(
    filename = function(){
      paste0("contacts_", Sys.Date(), ".csv")
    },
    content = function(file){
      
      linelist = fun_import_adjust(input,
                                   default_to_death_date = input$adjust_tree)
      
      
      contacts = check_contacts_upload(input$file_contact)
      
      #covering extras
      if(is.null(linelist$name)){ linelist = linelist %>% mutate(name = id)}
      if(is.null(linelist$code)){ linelist = linelist %>% mutate(code = id)}
      
      contacts[is.na(contacts)] = FALSE
      
      #check links are feasible
      contacts = check_exposure_timeline(linelist, contacts, input)
      
      #add source and sink onset dates
      contacts = contacts %>% add_column(source_onset = linelist$onset[match(contacts$from, linelist$id)], .after = "reason_inconsistent")
      contacts = contacts %>% add_column(infectee_onset = linelist$onset[match(contacts$to, linelist$id)], .after = "source_onset")
      
      write.csv(contacts, file, row.names = FALSE)
    }
  )
  
}


