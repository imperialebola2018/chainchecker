### checking functions ###

#### ----------------------------------------------------------------------------------- ####
assert_date = function(vec){
  
  #check formatting
  vec_sub = vec[!is.na(vec) & vec!="NA"]
  if(anyNA(as.Date(vec_sub, format = "%d/%m/%Y")) & anyNA(as.Date(vec_sub, format = "%m/%d/%Y")) ){ 
    
    stop(safeError("The dates are not in the correct format. The correct format is dd/mm/yy"))
    
  } else {
    
    if(anyNA(as.Date(vec_sub, format = "%d/%m/%Y"))){
      
      vec_out = mdy(vec)
      
    } else {
      
      vec_out = dmy(vec)
      
    }
  }
  
  
  return(vec_out)
}

#### ----------------------------------------------------------------------------------- ####
assert_TF = function(vec){
  
  if(sum(!vec %in% c(TRUE, FALSE, NA))>0){
    
    stop(safeError("A column that should be TRUE or FALSE (or NA) contains other values."))
  }
  
  return(vec)
}

#### ----------------------------------------------------------------------------------- ####
check_line_upload = function(file_upload){
  
  if(is.null(file_upload)){
    stop(safeError("No linelist file uploaded yet."))
  }
  
  
  #check right file type
  if(sum(grep(".csv", file_upload$datapath))==0){
    stop(safeError("Wrong file type uploaded for linelist, file should be a .csv ."))
  }
  
  #import
  df = read.csv(file_upload$datapath, stringsAsFactors = FALSE, na.strings = "")
  
  #check names
  check_line_names(df)
  
  #id
  if(length(unique(df$id))<nrow(df)){
    stop(safeError("There are duplicate id's in the linelist."))
  }
  
  #dates
  date_ind = grep("date", names(df))
  for(i in 1:length(date_ind)){
    df[,date_ind[i]] = assert_date(df[,date_ind[i]])
  }
  
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
check_contacts_upload = function(file_upload){
  
  if(is.null(file_upload)){
    stop(safeError("No contacts file uploaded yet."))
  }
  
  #check right file type
  if(sum(grep(".csv", file_upload$datapath))==0){
    stop(safeError("Wrong file type uploaded for contacts, file should be a .csv ."))
  }
  
  contacts = read.csv(file_upload$datapath, stringsAsFactors = FALSE, na.strings = "")
  
  #check names
  check_contact_names(contacts)
  
  #check for true/false
  if(length(names(contacts))>2){
    logic_ind = which(!names(contacts) %in% c("to", "from"))
    for(i in 1:length(logic_ind)){
      contacts[ ,logic_ind[i]] = assert_TF(contacts[ ,logic_ind[i]])
    }
  }
  
  #check for unique contact links
  contacts = check_unique_contact_links(contacts)
  
  #make sure in the right order
  contacts_out <- contacts %>% select(from, everything())
  
  #remove missing sources
  contacts_out = contacts_out[!is.na(contacts_out$from),]
  
  return(contacts_out)
}

#### ----------------------------------------------------------------------------------- ####
### check the names are correct ###
check_line_names = function(linelist){
  # must include `id`, `reported_onset_date` and `death_date`
  if(!"id" %in% names(linelist)){
    stop(safeError("Column `id` is missing from linelist."))
  }
  
  if(!"reported_onset_date" %in% names(linelist)){
    stop(safeError("Column `reported_onset_date` is missing from linelist."))
  }
  
  if(!"death_date" %in% names(linelist)){
    stop(safeError("Column `death_date` is missing from linelist."))
  }
}
#### ----------------------------------------------------------------------------------- ####
### check the names are correct ###
check_contact_names = function(contacts){
  #make sure it contains `from` and `to`
  if(!"from" %in% names(contacts)){
    stop(safeError("Column `from` is missing from contacts."))
  }
  
  if(!"to" %in% names(contacts)){
    stop(safeError("Column `to` is missing from contacts."))
  }
}
#### ----------------------------------------------------------------------------------- ####
### check contact links are unique and offer a warning ###
check_unique_contact_links = function(df){
  df_out = df[!duplicated(t(apply(df[c("from", "to")], 1, sort))),]
  
  if(nrow(df_out)<nrow(df)){
    
    
    stop(safeError(paste0("There were contact links defined twice (A->B and B->A).")))
  }
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
### check the dates are in the right order ###
check_date_order = function(linelist){
  #check whether onset reported before death
  linelist = linelist %>% 
    add_column(dates_in_correct_order = linelist$reported_onset_date < 
                 linelist$death_date)
  
  return(linelist)
}

#### ----------------------------------------------------------------------------------- ####
### check contact links are in exposure windows ###
check_exposure_timeline = function(linelist, contacts, input){
  
  #add extra column to contacts to check if the link is feasible wrt exposure windows
  contacts = contacts %>% add_column(INCONSISTENT = NA, .after = "to")
  
  #check each contact
  for(i in 1:nrow(contacts)){
    
    #if the linelist id is in the contacts (ie. that the contacts are specified correctly)
    if(sum(linelist$id %in% contacts$from[i])>0
       & sum(linelist$id %in% contacts$to[i])>0){
      
      #get the indices of each in the linelist
      linelist_index_from = which(linelist$id %in% contacts$from[i])
      linelist_index_to = which(linelist$id %in% contacts$to[i])
      
      #check if the exposure occurred before onset
      if(!is.na(linelist$onset_date[linelist_index_from]) & 
         !is.na(linelist$exposure_date_max[linelist_index_to])){
        
        if(as.numeric(linelist$onset_date[linelist_index_from] - 
                      linelist$exposure_date_max[linelist_index_to]) >= 0 ){
          
          contacts$INCONSISTENT[i] = TRUE
          
        } 
      }
      
      #check if the exposure happened after death
      if(!is.na(linelist$death_date[linelist_index_from]) &
         !is.na(linelist$exposure_date_min[linelist_index_to])){
        
        if(as.numeric(linelist$death_date[linelist_index_from] -
                      linelist$exposure_date_min[linelist_index_to]) <= 0){
          
          contacts$INCONSISTENT[i] = TRUE
          
        }
      }
      
      # #check if exposure happened after when death might have occurred (as an upper bound)
      # if(!is.na(linelist$onset_date[linelist_index_from]) &
      #    !is.na(linelist$exposure_date_min[linelist_index_to])){
      #   
      #   if(as.numeric(linelist$onset_date[linelist_index_from] + 7 +
      #                 input$days_onset_to_death_all -
      #                 linelist$exposure_date_min[linelist_index_to]) <= 0){
      #     
      #     contacts$INCONSISTENT[i] = TRUE
      #     
      #   }
      # }
    } 
  }
  
  return(contacts)
}
