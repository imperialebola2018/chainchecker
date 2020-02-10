# UI_functions

checkboxInput01 <- function(label, language_label, input){
  checkboxInput(label, 
                translation[[language_label]][[input$language]],
                value = TRUE)
}

numericInput01 <- function(label, 
                           language_label, 
                           value,
                           input){
  numericInput(label,
               translation[[language_label]][[input$language]],
               value = value, min = 0, max = 365)
}

dateInput01 <- function(label, 
                        language_label, 
                        value,
                        input){
  dateInput(label,
            translation[[language_label]][[input$language]],
            value = value)
}