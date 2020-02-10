# update the processed translation file translation.bin
# run this every time dictionary.csv is updated 
# it reads the look-up table in dictionary.csv and turns it into a 2D list

# https://github.com/chrislad/multilingualShinyApp/blob/master/updateTranslation.R

library(plyr)
translationContent <- read.delim("dictionary.csv", header = TRUE, sep = ",", as.is = TRUE) 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))

save(translation, file = "translation.bin")
