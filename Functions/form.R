get_vhf_table_metadata <- function() {
    fields.name <- c()
    fields.display <- c()
    fields.name <- c(fields.name, "row")
    fields.display <- c(fields.display, "Row")

    for (row in 1:nrow(csv_field_info)){
        display_name = toString(csv_field_info[row, "display_name"])
        fields.display <- c(fields.display, display_name)
    }

    for (row in 1:nrow(csv_field_info)){
        column_name = toString(csv_field_info[row, "column_name"])
        fields.name <- c(fields.name, column_name)
    }
    names(fields.display) <- fields.name

    result <- list(fields = fields.display)

    return (result)
}

get_next_id <- function(){
  if(exists("vhf_data") && nrow(vhf_data) > 0){
    max(as.integer(rownames(vhf_data))) + 1
  }
  else{
    return (1)
  }
}

#C
create_vhf_data <- function(data) {
  
  data <- cast_vhf_data(data)
  rownames(data) <- get_next_id()
  if (exists("vhf_data")) {
    vhf_data <<- rbind(vhf_data, data)
  } else {
    vhf_data <<- data
  }
}

create_default_vhf_record <- function(){
  fields.display <- c()
  fields.value <- c()
  default_record = tibble()
  for (row in 1:nrow(csv_field_info)){
    column_name <- toString(csv_field_info[row, "column_name"])
    column_type <- toString(csv_field_info[row, "input_type"])
    fields.display <- c(fields.display, column_name)
    fields.value <- c(fields.value, NA)

    default_value <- switch(column_type,
        "text" = "",
        "numeric" = 0,
        "checkbox" = FALSE,
        "date" = format(Sys.Date(), "%d/%m/%Y"),
        "select" = "None",
        "multiSelect" = "None",
        "source" = "None",
        NA
      )

    fields.display <- c(fields.display, column_name)
    fields.value <- c(fields.value, default_value)

  }
  
  names(fields.value) <- fields.display
  default_record <- data.frame(matrix(unlist(fields.value), nrow=1, byrow=T), stringsAsFactors=FALSE)    
  names(default_record) <- fields.display
  rownames(default_record) <- 0
  
  return (default_record)
}

#U
update_vhf_data <- function(data, session, selectedRow) {
    data <- cast_vhf_data(data)
    vhfData_rownames = row.names(vhf_data)
    vhf_data <<- data.frame(vhf_data, stringsAsFactors=FALSE)
    indx <- sapply(vhf_data, is.factor)
    vhf_data[indx] <<- lapply(vhf_data[indx], function(x) as.character(x))
    
    data_rownames = row.names(data)
    
    for (col in 1:length(data)){
        
        col_name  = names(data[col])

        data_val = data[selectedRow, col]

        vhf_data[vhfData_rownames == data_rownames, col_name[selectedRow]] <<- data[col]
    }

    vhf_data
    
}

#D
delete_vhf_data <- function(data) {
  vhf_data <<- vhf_data[row.names(vhf_data) != row.names(data), ]
}

# Cast from Inputs to a one-row data.frame
cast_vhf_data <- function(data) {
    #data_fields <- c()
    data_fields <- list()
    fields.display <- c()
    names_check <- c()

    for (row in 1:nrow(csv_field_info)){
      column_name <- toString(csv_field_info[row, "column_name"])
      input_type <- toString(csv_field_info[row, "input_type"])
      date_format <- toString(csv_field_info[row, "formats"])
      fields.display <- c(fields.display, column_name)

      column_value = switch(input_type,
        "text" = unname(data[column_name]),
        "numeric" = {
          val = data[column_name]
            if(is.null(val) || val == ""){
              val <- 0
            }
            else{
              val <- as.integer(val)
            }
          val
        },
        "checkbox" = {
            val = unname(data[column_name])
            if(is.null(val) || val == ""){
              val <- FALSE
            }
            else{
              val <- as.logical(val)
            }
          val
          
        },
        "date" = {
            date <- data[column_name][[1]]
            if(length(date) == 0 || is.null(date)){
              date <- NA
            }
            else{

              format1 <- as.Date(date, "%y-%m-%d")
              format2 <- as.Date(date, date_format)
              format1[is.na(format1)] <- format2[!is.na(format2)]

              date <- format(format1, date_format)
            }

            date
        },
        "select" = {
          val = unname(data[column_name])
            if(is.null(val) || val == ""){
              val <- row.names
            }
            else{
              val <- as.character(val)
            }
          val
        },
        "source" = {
          val = unname(data[column_name])
            if(is.null(val) || val == ""){
              val <- NA
            }
            else{
              val <- as.character(val)
            }
          val
        },
        "multiSelect" = {
            val = unname(data[column_name])
            if(is.null(val) || val == ""){
              val <- NA
            }
            else{
              val <- as.character(val)
            }
          val
        },
        NA
      )
    
      data_fields <- c(data_fields, column_value)
    }
    
    names(data_fields) <- fields.display

    df <- data.frame(matrix(unlist(data_fields), nrow=1, byrow=T), stringsAsFactors=FALSE)    
    rownames(df) <- data["row"]

    names(df) <- fields.display

  return (df)
}

# Fill the input fields with the values of the selected record in the table
update_vhf_inputs <- function(data, session, rowsSelected) {
  updateTextInput(session, "row", value = unname(rownames(data)))

  for (row in 1:nrow(csv_field_info)){
    column_name <- toString(csv_field_info[row, "column_name"])
    input_type <- toString(csv_field_info[row, "input_type"])
    date_format <- toString(csv_field_info[row, "formats"])
    

    if(column_name %in% colnames(data)){
      switch(input_type,
          "text" = updateTextInput(session, column_name, value = {              
            unname(data[column_name])
          }),
          "numeric" = updateNumericInput(session, column_name, value = as.integer(data[column_name])),
          "checkbox" = { 
              updateCheckboxInput(session, column_name, value = as.logical(unname(data[column_name][1])))
          },
          "date" = {
              #date = as.Date(data[column_name][[1]], date_format)
              date = as.Date(data[column_name][[1]], date_format)
              updateDateInput(session, column_name, value = date)
          },
          "select" = updateSelectInput(session, column_name, selected = unname(data[column_name])),
          "source" = updateSelectInput(session, column_name, selected = unname(data[column_name])),
          "multiSelect" = updateSelectInput(session, column_name, selected = unname(data[column_name]))
      )
    }
  }
}

generate_sidebar <- function(){
    formFields = tagList()
    formFields <- tagAppendChild(formFields, textInput("row", "Row"))
    for (row in 1:nrow(csv_field_info)){
      column_name <- toString(csv_field_info[row, "column_name"])
      display_name <- toString(csv_field_info[row, "display_name"])
      input_type <- toString(csv_field_info[row, "input_type"])
      required <- toString(csv_field_info[row, "required"])
      values <- strsplit(toString(csv_field_info[row, "values"]), ',')[[1]]
      
      field <- switch(input_type,
        "text" = textInput(column_name, display_name),
        "numeric" = numericInput(column_name, display_name, 0),
        "checkbox" = checkboxInput(column_name, display_name),
        "date" = dateInput(column_name, display_name, format="dd/mm/yyyy"),
        "select" = selectInput(column_name, display_name, c(values), selected=NA, selectize=TRUE, multiple=FALSE),
        "multiSelect" = selectInput(column_name, display_name, c(values), selected = NA, selectize=TRUE, multiple=TRUE),
        "source" = selectInput(column_name, display_name, c("None", select(vhf_data, "id")), selected = NA, selectize=TRUE, multiple=FALSE)

      )
      formFields <- tagAppendChild(formFields, field)

    }

    formFields
}