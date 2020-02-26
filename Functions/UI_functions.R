# UI_functions

checkboxInput01 <- function(id, language_label, input){
  checkboxInput(id, 
                translation[[language_label]][[input$language]],
                value = TRUE)
}
#----------------------------------------------------------------
numericInput01 <- function(id, 
                           language_label, 
                           value,
                           input){
  numericInput(id,
               translation[[language_label]][[input$language]],
               value = value, 
               min = 0, max = 365)
}
#----------------------------------------------------------------
dateInput01 <- function(id, 
                        language_label, 
                        value,
                        input){
  dateInput(id,
            translation[[language_label]][[input$language]],
            value = value)
}
#----------------------------------------------------------------
fileInput01 <- function(id, 
                        language_label, 
                        input){
  fileInput(id, 
            h3(translation[[language_label]][[input$language]]), 
            accept = ".csv",
            placeholder = translation[["no_data"]][[input$language]])
}
