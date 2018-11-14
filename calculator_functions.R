##################################
### ONSET CALCULATOR FUNCTIONS ###
##################################

### calculator logic steps ###
fun_get_onset = function(input){
  
  df = tibble("id" = input$id)
  
  #then add other dates
  if(input$death_avail){ #if there is a death date
    
    df = df %>% add_column(death_date = input$death_date,
                           onset_date = as.Date(input$death_date - input$days_onset_to_death),
                           reported_onset_date = as.Date(NA))
    
  } else { #no death date
    df = df %>% add_column(death_date = as.Date(NA),
                           reported_onset_date = input$reported_onset_date)
    if(input$bleeding){ #with bleeding
      df = df %>% add_column( onset_date = as.Date(df$reported_onset_date - input$days_onset_to_bleeding))
    } else if(input$diarrhea){ #with diarrhea
      df = df %>% add_column( onset_date = as.Date(df$reported_onset_date - input$days_onset_to_diarrhea))
    } else { #no wet symptoms
      df = df %>% add_column( onset_date = input$reported_onset_date)
    }
  }
  
  #calculate exposure period
  df = df %>% add_column( exposure_date_min = as.Date(df$onset_date - input$max_incubation),
                          exposure_date_max = as.Date(df$onset_date - input$min_incubation))
  
  
  return(df)
}


### import and adjust ###
fun_import_adjust = function(input){
  
  #import
  df = read.csv(input$file_line$datapath, stringsAsFactors = FALSE, na.strings = "")
  
  #make sure as dates
  df = df %>% mutate(reported_onset_date = assert_date(reported_onset_date),
                     death_date = assert_date(death_date))
  
  #format
  df = df %>% add_column( days_onset_to_bleeding = input$days_onset_to_bleeding_all,
                          days_onset_to_diarrhea = input$days_onset_to_diarrhea_all,
                          max_incubation = input$max_incubation_all,
                          min_incubation = input$min_incubation_all,
                          days_onset_to_death = input$days_onset_to_death_all,
                          death_avail = !is.na(df$death_date))
  
  if(is.null(df$bleeding_at_reported_onset)){
    df = df %>% add_column(bleeding_at_reported_onset = FALSE)
  }
  if(is.null(df$diarrhea_at_reported_onset)){
    df = df %>% add_column(diarrhea_at_reported_onset = FALSE)
  }
  
  #get onset
  df_out = NULL
  for(i in 1:nrow(df)){
    df_out = rbind(df_out, fun_get_onset(df[i,]))
  }
  
  df = df %>% 
    add_column( onset_date = df_out$onset_date, 
                exposure_date_min = df_out$exposure_date_min, 
                exposure_date_max = df_out$exposure_date_max,
                .after = "reported_onset_date") %>% 
    mutate(onset_date = assert_date(onset_date))
  
  return(df)
}

### function to make tree if data is uploaded ###
fun_make_tree = function(input){
  
  file_uploadl = input$file_line
  file_uploadc = input$file_contact
  
  if(!is.null(file_uploadl) & !is.null(file_uploadc)){
    
    if(input$adjust_tree){ #adjusted tree?
      
      linelist = fun_import_adjust(input)
      
    } else {
      linelist = read.csv(file_uploadl$datapath, stringsAsFactors = FALSE, na.strings = "")
      
      linelist = linelist %>% mutate(onset_date = assert_date(reported_onset_date),
                         death_date = assert_date(death_date))
    }
    

    contacts = read.csv(file_uploadc$datapath, stringsAsFactors = FALSE, na.strings = "")
    
    #covering extras
    if(is.null(linelist$name)){ linelist = linelist %>% mutate(name = id)}
    if(is.null(linelist$code)){ linelist = linelist %>% mutate(code = id)}
    contacts[is.na(contacts)] = FALSE
    
    #adjust for epicontacts
    names(linelist)[names(linelist) == 'onset_date'] <- 'onset'
    
    #make epicontacts
    x = epicontacts::make_epicontacts(linelist, contacts)
    
    #visualise
    p = vis_epicontacts_ggplot(x,
                               group = input$group, 
                               contactsgroup = input$groupcontact,
                               anon = FALSE,
                               serial = input$min_serial_tree) %>% 
      layout(height = 800)
    
    return(p)
  }
  
}
