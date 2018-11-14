### checking functions ###

library(lubridate)

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
