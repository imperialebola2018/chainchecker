### checking functions ###

#### ----------------------------------------------------------------------------------- ####
assert_date = function(vec){
  
  #check formatting
  if(all(is.na(as.Date(vec, format = "%d/%m/%Y")))){
    
    stop("The dates are not in the correct format. The correct format is dd/mm/yy")
    
  } else {
    
    vec_out = as.Date(vec, format = "%d/%m/%Y")
    
  }
  
  return(vec_out)
}

#### ----------------------------------------------------------------------------------- ####
assert_TF = function(vec){
  
  if(sum(!vec %in% c(TRUE, FALSE, NA))>0){
    
    stop("A column that should be TRUE or FALSE (or NA) contains other values.")
  }
  
  return(vec)
}

#### ----------------------------------------------------------------------------------- ####
check_line_upload = function(file_upload){
  
  if(is.null(file_upload)){
    stop("No linelist file uploaded yet.")
  }
  
  
  #check right file type
  if(sum(grep(".csv", file_upload$datapath))==0){
    stop("Wrong file type uploaded for linelist, file should be a .csv .")
  }
  
  #import
  df = read.csv(file_upload$datapath, stringsAsFactors = FALSE, na.strings = "")
  
  #id
  if(length(unique(df$id))<nrow(df)){
    stop("There are duplicate id's in the linelist.")
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
    stop("No contacts file uploaded yet.")
  }
  
  #check right file type
  if(sum(grep(".csv", file_upload$datapath))==0){
    stop("Wrong file type uploaded for contacts, file should be a .csv .")
  }
  
  contacts = read.csv(file_upload$datapath, stringsAsFactors = FALSE, na.strings = "")
  
  #check for true/false
  logic_ind = which(!names(contacts) %in% c("to", "from"))
  for(i in 1:length(logic_ind)){
    contacts[ ,logic_ind[i]] = assert_TF(contacts[ ,logic_ind[i]])
  }
  
  return(contacts)
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
check_exposure_timeline = function(linelist, contacts){
  
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
                      linelist$exposure_date_max[linelist_index_to]) > 0 ){
          
          contacts$INCONSISTENT[i] = TRUE
          
        } 
      }
      
      #check if the exposure happened after death
      if(!is.na(linelist$death_date[linelist_index_from]) &
         !is.na(linelist$exposure_date_min[linelist_index_to])){
        
        if(as.numeric(linelist$death_date[linelist_index_from] -
                      linelist$exposure_date_min[linelist_index_to]) < 0){
          
          contacts$INCONSISTENT[i] = TRUE
          
        }
      }
    } 
  }
  
  return(contacts)
}
