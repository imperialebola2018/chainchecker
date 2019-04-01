##################################
### ONSET CALCULATOR FUNCTIONS ###
##################################

### calculator logic steps ###
fun_get_onset = function(input,
                         default_to_death_date = TRUE){
  df = tibble("id" = input$id)

  if(default_to_death_date){
    #then add other dates
    #if(input$death_avail){ #if there is a death date

    if(input$death_avail){
      df = df %>% add_column(death_date = as.Date(input$death_date, "%d/%m/%Y"),
                             onset_date = as.Date(input$death_date, "%d/%m/%Y") - input$days_onset_to_death,
                             reported_onset_date = as.Date(input$reported_onset_date, "%d/%m/%Y"))
      
    } else { #no death date
      df = df %>% add_column(death_date = as.Date(input$death_date, "%d/%m/%Y"),
                             reported_onset_date = as.Date(input$reported_onset_date, "%d/%m/%Y"))
      
      if(input$bleeding_at_reported_onset){ #with bleeding
        df = df %>% add_column( onset_date = as.Date(df$reported_onset_date, "%d/%m/%Y") - 
                                                       input$days_onset_to_bleeding)
        
      } else if(input$diarrhea_at_reported_onset){ #with diarrhea
        df = df %>% add_column( onset_date = as.Date(df$reported_onset_date, "%d/%m/%Y") - 
                                                       input$days_onset_to_diarrhea)
        
      } else { #no wet symptoms
        df = df %>% add_column( onset_date = as.Date(df$reported_onset_date, "%d/%m/%Y"))
      }
    }
  } else {
    df = df %>% add_column( onset_date = as.Date(input$reported_onset_date, "%d/%m/%Y"),
                            death_date = as.Date(input$death_date, "%d/%m/%Y"),
                            reported_onset_date = as.Date(input$reported_onset_date, "%d/%m/%Y"))
  }
  
  #calculate exposure period
  df = df %>% add_column( exposure_date_min = as.Date(df$onset_date, "%d/%m/%Y") - input$max_incubation,
                          exposure_date_max = as.Date(df$onset_date, "%d/%m/%Y") - input$min_incubation)
  
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
### import and adjust ###
fun_import_adjust = function(input, data, 
                             default_to_death_date = TRUE){
  
  #import and check
  df = check_line_upload(data, "id")
  
  #format
  df$DateDeath[df$DateDeath == ""] <- NA

  df = df %>% add_column(death_date = df$DateDeath,
                        reported_onset_date = df$DateOnset)
  
  df = df %>% add_column( days_onset_to_bleeding = input$days_onset_to_bleeding_all,
                          days_onset_to_diarrhea = input$days_onset_to_diarrhea_all,
                          max_incubation = input$max_incubation_all,
                          min_incubation = input$min_incubation_all,
                          days_onset_to_death = input$days_onset_to_death_all,
                          death_avail = !is.na(df$death_date))

  if(is.null(df$bleeding_at_reported_onset)){
    df = df %>% add_column(bleeding_at_reported_onset = FALSE)
  } else if(is.na(all(df$bleeding_at_reported_onset))){
    df$bleeding_at_reported_onset = FALSE
  }
  if(is.null(df$diarrhea_at_reported_onset)){
    df = df %>% add_column(diarrhea_at_reported_onset = FALSE)
  } else if(is.na(all(df$diarrhea_at_reported_onset))){
    df$diarrhea_at_reported_onset = FALSE
  }
  
  #get onset
  df_out = NULL
  for(i in 1:nrow(df)){
    df_out = rbind(df_out, fun_get_onset(df[i,], default_to_death_date))
  }
  
  df = df %>% 
    add_column( onset_date = as.Date(df_out$onset_date, "%d/%m/%Y"), 
                exposure_date_min = as.Date(df_out$exposure_date_min, "%d/%m/%Y"), 
                exposure_date_max = as.Date(df_out$exposure_date_max, "%d/%m/%Y"),
                .after = "DateOnset") 
  
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
### function to make tree if data is uploaded ###
fun_make_tree = function(input, data){
  linelist = fun_import_adjust(input, data,
                               default_to_death_date = ifelse(input$adjust_tree,
                                                              FALSE, TRUE))
  contacts = check_contacts_upload(data)
  
  #covering extras for vis_epicontacts_ggplot
  if(is.null(linelist$name)){ linelist = linelist %>% mutate(name = id)}
  if(is.null(linelist$code)){ linelist = linelist %>% mutate(code = id)}
  
  #check links are feasible
  contacts = check_exposure_timeline(linelist, contacts, input)

  contacts[is.na(contacts)] = FALSE
  
  #adjust for epicontacts
  names(linelist)[names(linelist) == "DateOnset"] = "onset"
  
  
  contacts = contacts %>% add_column(to = contacts$id, .after="caseId_source")

  #make epicontacts
  x = epicontacts::make_epicontacts(linelist, contacts)
  
  #visualise
  p = vis_epicontacts_ggplot(x,
                             group = input$group, 
                             contactsgroup = input$groupcontact,
                             tooltip = unlist(strsplit(paste(input$tooltip), ","))) %>% 
    layout(height = 700)

  return(p)
}

#### ----------------------------------------------------------------------------------- ####
### function to plot exposure windows ###
fun_plot_exposure_windows = function(df, height){
  g = ggplot(df, aes(text = paste0("id: ",id))) 

  g = g + geom_rect(aes(xmin = as.Date(exposure_date_min,  "%d/%m/%Y"),
                        xmax = as.Date(exposure_date_max,  "%d/%m/%Y"),
                        ymin = reorder(id, exposure_date_min), 
                        ymax = reorder(id, exposure_date_min),
                        color = "Exposure"),
                    size = 1.1) +
    geom_point( aes( x = death_date,
                     y = reorder(id, exposure_date_min),
                     color = "Death"),
                shape = ifelse(df$dates_in_correct_order != TRUE,
                               "square",
                               "circle"),
                size = 5) +
    geom_point( aes( x = exposure_date_min,
                     y = reorder(id, exposure_date_min),
                     color = "Exposure"), size = 0.1) +
    geom_point( aes( x = exposure_date_max,
                     y = reorder(id, exposure_date_min),
                     color = "Exposure"), size = 0.1) +
    geom_point(aes(x = onset_date,
                   y = reorder(id, exposure_date_min),
                   color = "Estimated onset"),
               size = 5) +
    geom_point(aes(x = reported_onset_date,
                   y = reorder(id, exposure_date_min),
                   color = "Reported onset"),
               size = 5, shape = 4, stroke = 2) +
    ylab("Identifier") +
    labs(colour = "Key", shape = NULL)+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Date") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d/%m/%Y")

  p = plotly::ggplotly(g, height = height, tooltip = c("x", "text" )) 
  
  return(p)
}
