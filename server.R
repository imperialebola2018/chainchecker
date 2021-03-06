#

### SERVER ###
function(input, output, session) {
  

  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  # LANGUAGE SWITCHING #
  
  # HOME #
  output$aboutUI <- renderUI(
    includeMarkdown(paste0("Documentation/About_", input$language,".md"))
  )
  
  # TIMELINE #
  output$min_incubUI <- renderUI( #standard inputs
    numericInput01("min_incubation", "min_incub", 4, input)
  )
  
  output$max_incubUI <- renderUI(
    numericInput01("max_incubation", "max_incub", 21, input)
  )
  
  output$onset_deathUI <- renderUI(
    numericInput01("days_onset_to_death", "onset_death", 9, input)
  )
  
  output$idUI <- renderUI(
    textInput("id",
              translation[["id"]][[input$language]],
              value = "EG1")
  )
  
  output$dod_avail_checkUI <- renderUI(
    checkboxInput01("death_avail", "dod_avail_check", input)
  )
  
  output$dodUI <- renderUI(
    dateInput("death_date",
              label = translation[["dod"]][[input$language]],
              value = Sys.Date())
  )
  
  output$dosoUI <- renderUI(
    dateInput("reported_onset_date",
              label = translation[["doso"]][[input$language]],
              value = Sys.Date()-7)
  )
  
  output$bleeding_checkUI <- renderUI(
    checkboxInput01("bleeding_at_reported_onset", "bleeding_check", input)
  )
  
  output$onset_bleedingUI <- renderUI(
    numericInput01("days_onset_to_bleeding", "onset_bleeding", 6, input)
  )
  
  output$diarrhea_checkUI <- renderUI(
    checkboxInput01("diarrhea_at_reported_onset", "diarrhea_check", input)
  )
  
  output$onset_diarrheaUI <- renderUI(
    numericInput01("days_onset_to_diarrhea", "onset_diarrhea", 4, input)
  )
  
  output$hoverUI <- renderUI(
    span(translation[["hover"]][[input$language]], style="color:blue")
  )
  
  # UPLOAD #
  output$download_lUI <- renderUI(
    downloadButton("download_ltemplate", translation[["down_l"]][[input$language]])
  )
  
  output$download_cUI <- renderUI(
    downloadButton("download_ctemplate", translation[["down_c"]][[input$language]])
  )
  
  output$upload_lUI <- renderUI(
    fileInput01("file_line", "upload_l", input)
  )
  
  output$upload_cUI <- renderUI(
    fileInput01("file_contact", "upload_c", input)
  )
  
  output$upload_guideUI <- renderUI(
    includeMarkdown(paste0("Documentation/Upload_Guidelines_", input$language,".md"))
  )
  
  # EXPOSURE WINDOWS #
  output$check_dates_reportedUI <- renderUI(
    checkboxInput01("dates_as_reported", "calc_exp_check", input)
  )
  
  output$min_incub_allUI <- renderUI( #standard inputs
    numericInput01("min_incubation_all", "min_incub", 4, input)
  )
  
  output$max_incub_allUI <- renderUI(
    numericInput01("max_incubation_all", "max_incub", 21, input)
  )
  
  output$onset_death_allUI <- renderUI(
    numericInput01("days_onset_to_death_all", "onset_death", 9, input)
  )
  
  output$onset_bleeding_allUI <- renderUI(
    numericInput01("days_onset_to_bleeding_all", "onset_bleeding", 6, input)
  )
  
  output$onset_diarrhea_allUI <- renderUI(
    numericInput01("days_onset_to_diarrhea_all", "onset_diarrhea", 4, input)
  )
  
  output$enter_id1UI <- renderUI(
    textInput("ID1_onset_window",
              translation[["id_compare"]][[input$language]],
              placeholder = "EG1")
  )
  
  output$download_windowUI <- renderUI(
    downloadButton("download_window", translation[["down_results"]][[input$language]])
  )
  
  output$dates_of_deathUI <- renderUI(
    span(translation[["dod_incon"]][[input$language]], style="color:red")
  )
  
  # TRANSMISSION TREE #
  output$adjust_treeUI <- renderUI(
    checkboxInput01("adjust_tree", "show_tree", input)
  )
  
  output$tree_downloadUI <- renderUI(
    downloadButton("tree_download", translation[["down_results"]][[input$language]])
  )
  
  output$contact_downloadUI <- renderUI(
    downloadButton("contact_download", 
                   translation[["down_c_incon"]][[input$language]],
                   style="white-space: normal;
                                            text-align:left;")
  )
  
  # CLUSTER PLOTS #
  output$hover2UI <- renderUI(
    span(translation[["hover"]][[input$language]], style="color:blue")
  )
  
  output$download_clUI <- renderUI(
    downloadButton("cluster_download", translation[["down_cl"]][[input$language]])
  )
  
  # CLUSTER INFORMATION
  
  # METHOD #
  output$methodUI <- renderUI(
    includeMarkdown(paste0("Documentation/Methods_", input$language,".md"))
  )
  
  sapply(names(outputOptions(output)),
         FUN = function(x){outputOptions(output, x, suspendWhenHidden = FALSE)})

  
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------

  ### TIMELINE ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$exposure_plot <- renderPlotly({

    df = fun_get_onset(input, default_to_death_date = TRUE)
    
    df = check_date_order(df)
    
    p = fun_plot_exposure_windows(df, height=400)
    
    p
  })
  
  output$estimated_onset = renderText({
    paste0(translation[["estimated_onset"]][[input$language]], 
           format(fun_get_onset(input)$onset_date, format = "%d %B"), 
           ".")
  })
  
  output$exposure_window = renderText({
    paste0(translation[["estimated_window"]][[input$language]], 
           format(fun_get_onset(input)$exposure_date_min, format = "%d %B"), 
           " - ",
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
      write.csv(data.frame("from" = c("EG0", "EG1","EG1", "EG2", "EG3"), 
                           "to" = c("EG1", "EG2", "EG4", "EG5", "EG6") ,
                           "contact_of_type1" = c("FALSE", "TRUE", "FALSE", "FALSE", 
                                                  "FALSE"),
                           "contact_of_type2" = c("FALSE", "FALSE", "FALSE", "TRUE", 
                                                  "FALSE") ), file, row.names = FALSE )
    }
  )
  
  # DOWNLOAD LINELIST TEMPLATE #
  output$download_ltemplate = downloadHandler(
    filename = function(){
      "linelist_template.csv"
    },
    content = function(file){
      write.csv(data.frame("id" = c("EG0", "EG1", "EG2", "EG3", "EG4", "EG5", "EG6", "EG7", "EG8", "EG9"), 
                           "reported_onset_date" = format(c(Sys.Date() - 30,
                                                            Sys.Date()-7, Sys.Date()-4, Sys.Date()-3, 
                                                            Sys.Date()-2, Sys.Date()+1, Sys.Date(), 
                                                            Sys.Date() +1, Sys.Date() +1, Sys.Date()+2),
                                                          format = "%d/%m/%Y"),
                           "death_date" = format(c(Sys.Date() - 30,
                                                   as.Date(NA), as.Date(NA), Sys.Date()-2, 
                                                   Sys.Date()-1, Sys.Date()+4, as.Date(NA),
                                                   as.Date(NA), as.Date(NA), as.Date(NA)),
                                                 format = "%d/%m/%Y"),
                           "bleeding_at_reported_onset" = c("FALSE",
                                                            "TRUE", "FALSE", "FALSE", 
                                                            "FALSE", "TRUE", "FALSE", 
                                                            "FALSE", "TRUE", "TRUE"),
                           "diarrhea_at_reported_onset" = c("FALSE",
                                                            "FALSE", "FALSE", "TRUE", 
                                                            "FALSE","TRUE", "TRUE", 
                                                            "FALSE", "FALSE", "FALSE")), file, row.names = FALSE )
    }
  )
  
  ### ANALYSIS - WINDOWS ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$onset_plot = renderPlotly({
    
    req(input$days_onset_to_bleeding_all, input$days_onset_to_diarrhea_all)
    
    df_out = fun_import_adjust(input,
                               default_to_death_date = ifelse(input$dates_as_reported,
                                                              FALSE,
                                                              TRUE))
    
    df_out = check_date_order(df_out)
    
    if(input$ID1_onset_window %in% df_out$id | input$ID2_onset_window %in% df_out$id ){
      df_out %<>% filter(id %in% c(input$ID1_onset_window, input$ID2_onset_window))
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
                                 default_to_death_date = ifelse(input$dates_as_reported,
                                                                FALSE,
                                                                TRUE))
      
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
    
    contacts = check_contacts_upload(input$file_contact)
    
    #add the clusters
    linelist = cluster_add_func(linelist, contacts, input)
    
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
    

    
    selectInput(inputId = "group", 
                label = translation[["enter_plot_char"]][[input$language]], 
                choices = vec)
  })
  
  
  # DROP DOWN MENU CONTACT #
  output$contact_group = renderUI({
    
    linelist = fun_import_adjust(input,
                                 default_to_death_date = input$adjust_tree)
    
    
    contacts = check_contacts_upload(input$file_contact)
    
    #add the clusters
    linelist = cluster_add_func(linelist, contacts, input)
    
    #check links are feasible
    contacts = check_exposure_timeline(linelist, contacts, input)
    
    selectInput("groupcontact", 
                translation[["enter_trans"]][[input$language]], 
                names(contacts), selected = translation[["incon"]][[input$language]] )
  })
  
  output$tooltip_options = renderUI({
    
    linelist = fun_import_adjust(input,
                                 default_to_death_date = input$adjust_tree)
    
    contacts = check_contacts_upload(input$file_contact)
    
    #add the clusters
    linelist = cluster_add_func(linelist, contacts, input)
    
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
                   translation[["enter_char"]][[input$language]], 
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
      contacts %<>% add_column(source_onset = linelist$onset[match(contacts$from, linelist$id)], .after = "reason_inconsistent")
      contacts %<>% add_column(infectee_onset = linelist$onset[match(contacts$to, linelist$id)], .after = "source_onset")
      
      write.csv(contacts, file, row.names = FALSE)
    }
  )
  
  #------------------------------------------------------#
  # CLUSTER ANALYSIS #
  output$network = renderPlotly({
    
    fun_make_tree(input,type = "network")
    
  })
  
  output$networkTable = DT::renderDataTable({fun_make_tree(input,type = "table")})
  
  # CLUSTER DOWNLOAD #
  output$cluster_download = downloadHandler(
    filename = function(){ 
      paste0("Clusters_", Sys.Date(), ".html")
    },
    content = function(file){
      
      p = fun_make_tree(input, type = "network")
      
      htmlwidgets::saveWidget(as.widget(p), file)
      
    }
  )
}


