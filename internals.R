### checking functions ###

library(lubridate)
#### ----------------------------------------------------------------------------------- ####
assert_date = function(vec){
  
  #check formatting
  if(is.na(as.Date(vec, format = "%d/%m/%Y"))){
    
    stop("The dates are not in the correct format. The correct format is dd/mm/yy")
    
  } else {
    vec_out = dmy(vec)
  }
  
  return(vec_out)
}


assert_TF = function(vec){

  if(!typeof(vec) == "logical"){
    
    stop("A column that should be TRUE/ FALSE contains other values.")
  }
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
  
  return(contacts)
}
