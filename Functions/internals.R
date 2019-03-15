### checking functions ###

#### ----------------------------------------------------------------------------------- ####
assert_date = function(vec){
  
  #check formatting is in either dd/mm/YY or mm/dd/YY
  vec_sub = vec[!is.na(vec) & vec!="NA"]

  if(anyNA(as.Date(vec_sub, format = "%d/%m/%Y")) & 
     anyNA(as.Date(vec_sub, format = "%m/%d/%Y")) ){ 
    
    stop(safeError("The dates are not in the correct format. 
                   The correct format is either dd/mm/yy or mm/dd/yy and all dates should be consistent."))
    
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
  
  #checking columns that should be true/ false are true/ false
  if(sum(!vec %in% c(TRUE, FALSE, NA))>0){
    
    stop(safeError("A column that should be TRUE or FALSE (or NA) contains other values."))
  }
  return(vec)
}

#### ----------------------------------------------------------------------------------- ####
check_line_upload = function(file_upload,id_col){
  
  #if empty upload
  if(is.null(file_upload)){
    stop(safeError("No linelist file uploaded yet."))
  }
  
  #check correct file type. This works for both comma separated and semicolon separated Csv
  if(sum(grep(".csv", file_upload$datapath))==0){
    stop(safeError("Wrong file type uploaded for linelist, file should be a .csv ."))
  }
  
  #import
  #df = as.data.frame(fread(file_upload$datapath, stringsAsFactors = FALSE, na.strings = ""))
  df <- vhf_data
  #check names
  check_line_names(df)
  
  #check id's are unique
  if(length(unique(df$id))<nrow(df)){
    stop(safeError("There are duplicate id's in the linelist."))
  }
  
  #make sure id is first column

  df = df %>% select(id, everything())
  
  #dates


  
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
check_contacts_upload = function(file_upload){
  
  #if empty upload
  if(is.null(file_upload)){
    stop(safeError("No contacts file uploaded yet."))
  }
  
  #check correct file type. This works for both comma separated and semicolon separated Csv
  #if(sum(grep(".csv", file_upload$datapath))==0){
  #  stop(safeError("Wrong file type uploaded for contacts, file should be a .csv ."))
  #}

  fields.name <- c()

  for (row in 1:nrow(csv_field_info)){
      column_name = toString(csv_field_info[row, "column_name"])
      to_col_tf <- as.logical(csv_field_info[row, "toCol"])
      from_col_tf <- as.logical(csv_field_info[row, "fromCol"])

      if(!is.na(to_col_tf) && to_col_tf == TRUE){
        to_col <- column_name
      }
      if(!is.na(from_col_tf) && from_col_tf == TRUE){
        from_col <- column_name
      }
  }

  #import
  contacts <- vhf_data
  
  #check names
  check_contact_names(contacts, to_col, from_col)
  
  #check for unique contact links
  contacts = check_unique_contact_links(contacts, from_col, to_col)
  
  #make sure in the right order
  contacts_out = contacts %>% select(from_col, everything())
  
  #remove missing sources
  contacts_out = contacts_out[!is.na(contacts_out[from_col]),]
  
  return(contacts_out)
}

#### ----------------------------------------------------------------------------------- ####
### check the names are correct ###
check_line_names = function(linelist){

  # must include `id`, `reported_onset_date` and `death_date`
  if(!"id" %in% names(linelist)){
    stop(safeError("Column `id` is missing from linelist."))
  }
  
  if(!"DateOnset" %in% names(linelist)){
    stop(safeError("Column `reported_onset_date` is missing from linelist."))
  }
  
  if(!"DateDeath" %in% names(linelist)){
    stop(safeError("Column `death_date` is missing from linelist."))
  }
  
}
#### ----------------------------------------------------------------------------------- ####
### check the names are correct ###
check_contact_names = function(contacts, to_col, from_col){

  #make sure it contains `from` and `to`
  if(!from_col %in% names(contacts)){
    stop(safeError("Column `from` is missing from contacts."))
  }

  if(!to_col %in% names(contacts)){
    stop(safeError("Column ", to_col, " is missing from contacts."))
  }
}
#### ----------------------------------------------------------------------------------- ####
### check contact links are unique and offer a warning ###
check_unique_contact_links = function(df, to_col, from_col){
  #remove duplicates
  df_out = df[!duplicated(t(apply(df[c(from_col, to_col)], 1, sort))),]
  
  if(nrow(df_out)<nrow(df)){
    stop(safeError(paste0("There were contact links defined twice.")))
  }
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
### check the dates are in the right order ###
check_date_order = function(linelist){
  #check whether onset reported before death

  linelist = linelist %>% 
    add_column(dates_in_correct_order = as.Date(linelist$onset_date, "%d/%m/%Y") < 
                 as.Date(linelist$death_date, "%d/%m/%Y"))
  
  return(linelist)
}

#### ----------------------------------------------------------------------------------- ####
### check contact links are in exposure windows ###
check_exposure_timeline = function(linelist, contacts, input){
  
  #add extra column to contacts to check if the link is feasible wrt exposure windows
  contacts = contacts %>% add_column(INCONSISTENT = NA, .after = "caseId_source")
  contacts = contacts %>% add_column(reason_inconsistent = NA, .after = "INCONSISTENT")
  
  #check each contact
  for(i in 1:nrow(contacts)){
    
    #if the linelist id is in the contacts (ie. that the contacts are specified correctly)
    if(sum(linelist$id %in% contacts$caseId_source[i])>0
       & sum(linelist$id %in% contacts$id[i])>0){
      #get the indices of each in the linelist
      linelist_index_from = which(linelist$id %in% contacts$caseId_source[i])
      linelist_index_to = which(linelist$id %in% contacts$id[i])
      
      #check if the exposure occurred before onset
      if(!is.na(linelist$DateOnset[linelist_index_from]) & 
         !is.na(linelist$exposure_date_max[linelist_index_to])){
        
        if(as.Date(linelist$DateOnset[linelist_index_from], "%d/%m/%Y") >=
                      as.Date(linelist$exposure_date_max[linelist_index_to], "%d/%m/%Y") ){
          
          contacts$INCONSISTENT[i] = TRUE
          contacts$reason_inconsistent[i] = "Exposure occurred before source onset date."
          
        } 
      }
      
      #check if the exposure happened after death
      if(!is.na(as.Date(linelist$DateDeath[linelist_index_from], "%d/%m/%Y")) &
         !is.na(as.Date(linelist$exposure_date_min[linelist_index_to], "%d/%m/%Y"))){
        
        if(as.Date(linelist$DateDeath[linelist_index_from], "%d/%m/%Y") <=
                      as.Date(linelist$exposure_date_min[linelist_index_to], "%d/%m/%Y")){
          
          contacts$INCONSISTENT[i] = TRUE
          contacts$reason_inconsistent[i] = "Exposure occurred after source death date."
        }
      }
      
    } 
  }
  
  return(contacts)
}