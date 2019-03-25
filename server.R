#
# dependencies
library(plotly)
library(ggplot2)
library(epicontacts)
library(tibble)
library(plyr)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(gtools)
source('Functions/globals.R')
source('Functions/vis_epicontacts_ggplot.R')
source('Functions/calculator_functions.R')
source('Functions/internals.R')
source('Functions/form.R')
source('Functions/RR_nosocomial_transmission_plots.R')



### SERVER ###
function(input, output, session) {

  vhf_data_reactive <<- reactive({
    infile <- input$file_vhf
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }

    fields.name <- c()

    for (row in 1:nrow(csv_field_info)){
        column_name = toString(csv_field_info[row, "column_name"])
        
        fields.name <- c(fields.name, column_name)
    }


    new_df = data.frame(matrix(ncol = nrow(csv_field_info), nrow = 0), stringsAsFactors=FALSE)
    colnames(new_df) <- fields.name

    vhf_data = read.csv(infile$datapath, header=TRUE)
    
    vhf_data <- vhf_data %>% add_column(id = vhf_data$ID, .after = "ID")
    
    matching_fields = fields.name[fields.name %in% names(vhf_data)]

    cc_fields = setdiff(names(new_df), names(vhf_data))

    df <- rbind(new_df, vhf_data[,matching_fields])
    df[,cc_fields] <- NA

    df <- df[fields.name]

    vhf_data <<- df

    output$sidebar <- renderUI({generate_sidebar()})
    df
  })



  output$sidebar <- renderUI({
    infile <- input$file_vhf
    if (is.null(infile)) {
      # User has not uploaded a file yet
      vhf_data <<- create_vhf_data(create_default_vhf_record())
    }
    generate_sidebar()
  })

  vhfFormData <- reactive({
    sapply(names(get_vhf_table_metadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$vhf_submit, {
    if (input$id != 0) {
      update_vhf_data(vhfFormData())
    } else {
      create_vhf_data(vhfFormData())
    }

    output$vhfTable <- renderDT({
      #update after submit is clicked
      input$submit
      #update after delete is clicked
      input$delete
      vhf_data
      }, server = FALSE, selection = "single", rownames = FALSE, 
      colnames = unname(get_vhf_table_metadata()$fields)[-1],

      )    

  }, priority = 1)

  observeEvent(input$vhf_create, {
    update_vhf_inputs(create_default_vhf_record(), session)
  })
  
  # Press "New" button -> display empty record
  observeEvent(vhf_data_reactive(), {
      vhf_data <<- vhf_data_reactive()
  })
  
  # Press "Delete" button -> delete from data
  output$vhf_export <- downloadHandler(
    filename = function(){
      paste0("vhf_", Sys.Date(), ".csv")
    },
    content = function(file){
      
      write.csv(vhf_data, file, row.names = FALSE)
    }
  )

  # Press "Delete" button -> delete from data
  observeEvent(input$vhf_delete, {
    delete_vhf_data(vhf_data[input$vhfTable_rows_selected, ])
    update_vhf_inputs(create_default_vhf_record(), session)

    output$vhfTable <- renderDT({
      input$submit
      input$delete
      vhf_data
      }, server = FALSE, selection = "single", rownames = FALSE, 
      colnames = unname(get_vhf_table_metadata()$fields)[-1],

      )    
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$vhfTable_rows_selected, {
    if (length(input$vhfTable_rows_selected) > 0) {
      data <- vhf_data[input$vhfTable_rows_selected, ]
      update_vhf_inputs(data, session, input$vhfTable_rows_selected)
      
    }
    
  })

  output$vhfTable <- renderDT({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    vhf_data_reactive()
  }, server = FALSE, selection = "single", rownames = FALSE, 
  colnames = unname(get_vhf_table_metadata()$fields)[-1],

  )    

  ### TIMELINE ###-----------------------------------------------------------------------------------
  
  # PLOT #
  output$exposure_plot <- renderPlotly({
    df <- tibble(
      id = input$timeline_id, 
      reported_onset_date = input$timeline_onset_date, 
      death_date = input$timeline_death_date,
      bleeding_at_reported_onset = input$bleeding_at_reported_onset,
      days_onset_to_bleeding = input$days_onset_to_bleeding,
      diarrhea_at_reported_onset = input$diarrhea_at_reported_onset,
      days_onset_to_diarrhea = input$days_onset_to_diarrhea,
      days_onset_to_death = input$days_onset_to_death,
      death_avail = input$death_avail,
      min_incubation = input$min_incubation,
      max_incubation = input$max_incubation
      )
  
    df = fun_get_onset(df[1,], default_to_death_date = TRUE)
  
    df = check_date_order(df)
    

    output$estimated_onset = renderText({
      paste0("Estimated onset of symptoms for these parameters is ", 
            format(df$onset_date, format = "%d %B"), 
            ".")
    })
    
    output$exposure_window = renderText({
      paste0("The exposure window for these parameters is ", 
            format(df$exposure_date_min, format = "%d %B"), 
            " to ",
            format(df$exposure_date_max, format = "%d %B"),
            ".")
    })

    p = fun_plot_exposure_windows(df, height=400)
    
    p
  })
  
output$noso_case_id_plot <- renderPlotly({
    df = fun_import_adjust(input,
                               default_to_death_date = TRUE)

    p = get_nosocomial_plots_by_case_id(input, df)
    
    p
  })

  output$noso_hospital_plot <- renderPlotly({
    df = fun_import_adjust(input,
                               default_to_death_date = TRUE)

    p = get_nosocomial_plots_by_hospital_patients(input, df)
    
    p
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
    df = fun_import_adjust(input,
                               default_to_death_date = TRUE)
    df_out = NULL
    for(i in 1:nrow(df)){
      df_out = rbind(df_out, fun_get_onset(df[i,]))
    }

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
    
    tree <- fun_make_tree(input)
    
    tree
    
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
    
    
    contacts = check_contacts_upload(input$file_vhf)
    
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
      
      
      contacts = check_contacts_upload(input$file_vhf)
      
      #covering extras
      if(is.null(linelist$name)){ linelist = linelist %>% mutate(name = id)}
      if(is.null(linelist$code)){ linelist = linelist %>% mutate(code = id)}
      
      contacts[is.na(contacts)] = FALSE
      
      #check links are feasible
      contacts = check_exposure_timeline(linelist, contacts, input)
      
      write.csv(contacts, file, row.names = FALSE)
    }
  )
  
}


