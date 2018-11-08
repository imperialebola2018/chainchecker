##################################
### ONSET CALCULATOR FUNCTIONS ###
##################################

### calculator logic steps ###
fun_get_onset = function(input){
  
  df = tibble("ID" = input$id)
  
  #then add other dates
  if(input$death_avail){ #if there is a death date
    
    df = df %>% add_column(Death = input$death,
                           Onset = as.Date(input$death - input$symptomatic),
                           Reported_Onset = as.Date(NA))
    
  } else { #no death date
    df = df %>% add_column(Death = as.Date(NA),
                           Reported_Onset = input$report_onset)
    if(input$bleeding){ #with bleading
      df = df %>% add_column( Onset = as.Date(df$Reported_Onset - input$bleeding_correction))
    } else if(input$diarrhea){ #with diarrhea
      df = df %>% add_column( Onset = as.Date(df$Reported_Onset - input$diarrhea_correction))
    } else { #no wet symptoms
      df = df %>% add_column( Onset = input$report_onset)
    }
  }
  
  #calculate exposure period
  df = df %>% add_column( Exposure_min = as.Date(df$Onset - input$max_incubation),
                          Exposure_max = as.Date(df$Onset - input$min_incubation))
  
  
  return(df)
}

### formatting the file with settings for download ###
fun_format_file = function(df, input){
  
  df = df %>% add_column( bleeding_correction = input$bleeding_correction,
                          diarrhea_correction = input$diarrhea_correction,
                          max_incubation = input$max_incubation,
                          min_incubation = input$min_incubation,
                          symptomatic = input$symptomatic,
                          death_avail = !is.na(df$death))
  
  
  return(df)
}

### import and adjust ###
fun_import_adjust = function(input){
  
  #import
  df = read.csv(input$file_line$datapath, stringsAsFactors = FALSE, na.strings = "")
  
  #make sure as dates
  df = df %>% mutate(report_onset = as.Date(report_onset, format = "%d/%m/%Y"),
                     death = as.Date(death, format = "%d/%m/%Y"))
  
  #format
  input_all = tibble("bleeding_correction" = input$bleeding_correction_all,
                     "diarrhea_correction" = input$diarrhea_correction_all,
                     "symptomatic" = input$symptomatic_all,
                     "min_incubation" = input$min_incubation_all,
                     "max_incubation" = input$max_incubation_all)
  
  df = fun_format_file(df, input_all)
  
  #get onset
  df_out = NULL
  for(i in 1:nrow(df)){
    df_out = rbind(df_out, fun_get_onset(df[i,]))
  }
  
  df = df %>% 
    add_column( onset = df_out$Onset, 
                exposure_min = df_out$Exposure_min, 
                exposure_max = df_out$Exposure_max,
                .after = "report_onset") %>% 
    mutate(onset = as.Date(onset, format = "%d/%m/%Y"))
  
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
      
      linelist = linelist %>% mutate(onset = as.Date(report_onset, format = "%d/%m/%Y"),
                         death = as.Date(death, format = "%d/%m/%Y"))
    }
    

    contacts = read.csv(file_uploadc$datapath, stringsAsFactors = FALSE, na.strings = "")
    
    #covering extras
    if(is.null(linelist$name)){ linelist = linelist %>% mutate(name = id)}
    if(is.null(linelist$code)){ linelist = linelist %>% mutate(code = id)}
    contacts[is.na(contacts)] = FALSE
    
    #make epicontacts
    x = epicontacts::make_epicontacts(linelist, contacts)
    
    #visualise
    p = vis_epicontacts_ggplot(x,
                               group = input$group, 
                               contactsgroup = input$groupcontact,
                               anon = TRUE,
                               serial = input$min_incubation_tree) %>% 
      layout(height = 800)
    
    return(p)
  }
  
}
