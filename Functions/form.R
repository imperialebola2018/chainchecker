# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically

outputDir <- "."

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

#C
create_vhf_data <- function(data) {
  
  data <- cast_vhf_data(data)

  if (exists("vhf_data")) {

    vhf_data <<- rbind(vhf_data, data)
  } else {
    vhf_data <<- data
  }
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
  vhf_data <<- vhf_data[row.names(vhf_data()) != unname(data["row"]), ]
}

# Cast from Inputs to a one-row data.frame
cast_vhf_data <- function(data) {
    data_fields <- c()
    fields.display <- c()

    for (row in 1:nrow(csv_field_info)){
        display_name = as.character(csv_field_info[row, "column_name"])
        #display_name = toString(csv_field_info[row, "display_name"])
        fields.display <- c(fields.display, display_name)
    }

    for (row in 1:nrow(csv_field_info)){
      column_name <- toString(csv_field_info[row, "column_name"])
      input_type <- toString(csv_field_info[row, "input_type"])
      date_format <- toString(csv_field_info[row, "formats"])

      column_value = switch(input_type,
        "text" = data[column_name],
        "numeric" = as.integer(unname(data[column_name])),
        "checkbox" = as.logical(unname(data[column_name])),
        "date" = {

            date <- format(as.Date(data[column_name][[1]]), date_format)
            if(length(date) == 0 || is.null(date) || date == ""){
                date <- ""
            }
            date
        },
        "select" = as.character(unname(data[column_name])),
        "source" = as.character(unname(data[column_name])),
        "multiSelect" = {
            as.character(data[column_name])
        }
      )
      
      
      data_fields <- c(data_fields, column_value)
    }

    names(data_fields) <- fields.display

    df <- data.frame(matrix(unlist(data_fields), nrow=1, byrow=T), stringsAsFactors=FALSE)
    
    names(df) <- fields.display

    rownames(df) <- data["row"]

  return (df)
}

# Fill the input fields with the values of the selected record in the table
update_vhf_inputs <- function(data, session, rowsSelected) {
    
    for (row in 1:nrow(csv_field_info)){
      column_name <- toString(csv_field_info[row, "column_name"])
      input_type <- toString(csv_field_info[row, "input_type"])
      date_format <- toString(csv_field_info[row, "formats"])

    updateTextInput(session, "row", value = unname(rownames(data)))

    if(column_name %in% colnames(data)){
        switch(input_type,
            "text" = updateTextInput(session, column_name, value = {
              unname(data[column_name])
            }),
            "numeric" = updateNumericInput(session, column_name, value = as.integer(data[column_name])),
            #"checkbox" = {
            #    print(data[column_name])
            #    print(unname(data[column_name]))
            ##    print(as.logical(unname(data[column_name])[1]))
            #    print(data[column_name][[1]][1])
            #    updateCheckboxInput(column_name, value = as.logical(unname(data[column_name])[[1]]))
            #},
            "date" = {
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