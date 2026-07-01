#storage.R
#Handles reading and writing ETAP entries to Google Drive CSV.
#This file is sourced by app.R at startup.

library(googledrive)
googledrive::drive_deauth()

#Configuration

drive_folder_id <- "1AKb1lDhQn93f6IvCWExFZW1svz1jjFdr"
local_temp_file <- tempfile(fileext = ".csv")

#get_drive_file: finds the CSV in Drive, returns its ID or NULL

get_drive_file <- function(data_file_name) {
  results <- drive_ls(
    path = as_id(drive_folder_id),
    pattern = data_file_name
  )
  if (nrow(results) > 0) results$id[1] else NULL
}

#save_entry: appends one row to the Drive CSV

save_entry <- function(input, all_input_ids, total) {
  event_code     <- ifelse(is.null(input$event_code) || input$event_code == "",
                           "no-event", input$event_code)
  data_file_name <- paste0("etap_", event_code, ".csv")
  message("Saving to file: ", data_file_name)
  
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
    lat          = ifelse(is.null(input$gps_lat), NA, input$gps_lat),
    lng          = ifelse(is.null(input$gps_lng), NA, input$gps_lng),
    total_items  = total,
    t(item_vals),
    stringsAsFactors = FALSE
  )
  
  #Check if the CSV already exists in Drive
  file_id <- get_drive_file(data_file_name)
  
  if (!is.null(file_id)) {
    #Download existing CSV, append new row, re-upload
    drive_download(as_id(file_id), path = local_temp_file, overwrite = TRUE)
    existing <- read.csv(local_temp_file, stringsAsFactors = FALSE)
    updated  <- rbind(existing, new_row)
    write.csv(updated, local_temp_file, row.names = FALSE)
    drive_update(as_id(file_id), media = local_temp_file)
  } else {
    #No CSV yet — create it fresh in the Drive folder
    write.csv(new_row, local_temp_file, row.names = FALSE)
    drive_upload(
      media  = local_temp_file,
      path   = as_id(drive_folder_id),
      name   = data_file_name,
      type   = "text/csv"
    )
  }
}

#load_entries: reads all saved entries from Drive

load_entries <- function() {
  file_id <- get_drive_file()
  if (!is.null(file_id)) {
    drive_download(as_id(file_id), path = local_temp_file, overwrite = TRUE)
    read.csv(local_temp_file, stringsAsFactors = FALSE)
  } else {
    NULL
  }
}

