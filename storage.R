#Data Storage
#Handles reading and writing ETAP entries to local CSV file

data_file <- file.path(getwd(), "etap_entry.csv")

#save_entry: appends one row to the CSV

save_entry <- function(input, all_input_ids, total) {
  
  #Collect all item counts into a named vector
  item_vals <- sapply(all_input_ids, function(id) {
    v <- input[[id]]
    if (is.null(v) || is.na(v)) 0L else as.integer(v)
  })
  
  #Build one row dataframe
  new_row <- data.frame(
    timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    site_name    = input$site_name,
    cleanup_date = as.character(input$cleanup_date),
    volunteer_id = input$volunteer_id,
    site_type    = input$site_type,
    condition    = input$condition,
    total_items  = total,
    t(item_vals),
    stringsAsFactors = FALSE
  )
  
  # Append to CSV if it exists, create it if it doesn't
  if (file.exists(data_file)) {
    write.table(new_row, data_file,
                sep       = ",",
                col.names = FALSE,
                row.names = FALSE,
                append    = TRUE)
  } else {
    write.csv(new_row, data_file, row.names = FALSE)
  }
}

#load_entries: reads all saved entries back as a dataframe

load_entries <- function() {
  if (file.exists(data_file)) {
    read.csv(data_file, stringsAsFactors = FALSE)
  } else {
    NULL
  }
}

