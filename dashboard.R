#Event summary dashboard tab.
#Shows collective impact across all entries for a selected event code.
#Sourced by app.R at startup.

library(leaflet)
library(ggplot2)

#dashboard_ui: the second tab UI

dashboard_ui <- tabPanel("Event Summary",
                         
                         div(style = "max-width: 500px; margin: 0 auto; padding: 12px;",
                             
                             #Event selector
                             div(class = "card",
                                 div(class = "section-lbl", style = "margin-bottom: 10px;", "Select event"),
                                 uiOutput("event_selector"),
                                 actionButton("refresh_dashboard", "Refresh",
                                              style = "margin-top: 10px; padding: 8px 20px;
                            background: #1D9E75; color: white;
                            border: none; border-radius: 6px;
                            font-size: 13px; cursor: pointer;")
                             ),
                             
                             #Event stats
                             uiOutput("dashboard_stats"),
                             
                             #Map
                             uiOutput("dashboard_map_container"),
                             
                             #Material breakdown
                             uiOutput("dashboard_chart_container"),
                             
                             #Top items
                             uiOutput("dashboard_top_items")
                         )
)

dashboard_server <- function(input, output, groups, group_ids) {
  
  #List available event CSVs from Google Drive
  available_events <- reactive({
    input$refresh_dashboard
    files <- drive_ls(path = as_id(drive_folder_id), pattern = "^etap_")
    if (nrow(files) == 0) return(character(0))
    #Strip etap_ prefix and .csv suffix to get event codes
    codes <- gsub("^etap_", "", gsub("\\.csv$", "", files$name))
    codes
  })
  
  output$event_selector <- renderUI({
    events <- available_events()
    if (length(events) == 0) {
      p("No events found. Submit an entry first.",
        style = "color: #888; font-size: 13px;")
    } else {
      selectInput("selected_event", label = NULL, choices = events)
    }
  })
  
  #Load entries for the selected event
  event_data <- reactive({
    input$refresh_dashboard
    req(input$selected_event)
    filename <- paste0("etap_", input$selected_event, ".csv")
    files <- drive_ls(path = as_id(drive_folder_id), pattern = filename)
    if (nrow(files) == 0) return(NULL)
    tmp <- tempfile(fileext = ".csv")
    drive_download(as_id(files$id[1]), path = tmp, overwrite = TRUE)
    read.csv(tmp, stringsAsFactors = FALSE)
  })
  
  #Stats card
  output$dashboard_stats <- renderUI({
    df <- event_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    n_submissions  <- nrow(df)
    n_volunteers   <- length(unique(df$volunteer_id))
    total_items    <- sum(df$total_items, na.rm = TRUE)
    n_with_gps     <- sum(!is.na(df$lat) & df$lat != "", na.rm = TRUE)
    
    div(class = "card",
        div(class = "section-lbl", style = "margin-bottom: 12px;",
            paste("Event:", input$selected_event)),
        div(style = "display: flex; flex-wrap: wrap; gap: 16px;",
            div(
              div(style = "font-size: 11px; color: #555; text-transform: uppercase;
                       letter-spacing: .05em;", "Total items"),
              div(style = "font-size: 28px; font-weight: 500; color: #085041;",
                  total_items)
            ),
            div(
              div(style = "font-size: 11px; color: #555; text-transform: uppercase;
                       letter-spacing: .05em;", "Submissions"),
              div(style = "font-size: 28px; font-weight: 500; color: #1D9E75;",
                  n_submissions)
            ),
            div(
              div(style = "font-size: 11px; color: #555; text-transform: uppercase;
                       letter-spacing: .05em;", "Volunteers"),
              div(style = "font-size: 28px; font-weight: 500; color: #1D9E75;",
                  n_volunteers)
            ),
            div(
              div(style = "font-size: 11px; color: #555; text-transform: uppercase;
                       letter-spacing: .05em;", "GPS entries"),
              div(style = "font-size: 28px; font-weight: 500; color: #1D9E75;",
                  n_with_gps)
            )
        )
    )
  })
  
  #Map
  output$dashboard_map_container <- renderUI({
    df <- event_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    gps_df <- df[!is.na(df$lat) & !is.na(df$lng), ]
    if (nrow(gps_df) == 0) return(NULL)
    
    div(class = "card",
        div(class = "section-lbl", style = "margin-bottom: 10px;", "Entry locations"),
        leafletOutput("dashboard_map", height = 280)
    )
  })
  
  output$dashboard_map <- renderLeaflet({
    df <- event_data()
    gps_df <- df[!is.na(df$lat) & !is.na(df$lng), ]
    leaflet(gps_df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng    = ~lng,
        lat    = ~lat,
        radius = 6,
        color  = "#1D9E75",
        fillOpacity = 0.8,
        popup  = ~paste0(
          "<b>", site_name, "</b><br>",
          volunteer_id, "<br>",
          total_items, " items<br>",
          condition
        )
      )
  })
  
  #Material breakdown chart
  output$dashboard_chart_container <- renderUI({
    df <- event_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    div(class = "card",
        div(class = "section-lbl", style = "margin-bottom: 10px;",
            "Material breakdown"),
        renderPlot({
          #Sum item columns by group
          group_totals <- sapply(group_ids, function(gid) {
            ids <- paste0(gid, "_", seq_along(groups[[gid]]$items))
            ids_present <- ids[ids %in% names(df)]
            if (length(ids_present) == 0) return(0)
            sum(colSums(df[, ids_present, drop = FALSE], na.rm = TRUE))
          })
          
          plot_data <- data.frame(
            group = sapply(group_ids, function(gid) groups[[gid]]$label),
            count = as.integer(group_totals),
            stringsAsFactors = FALSE
          )
          plot_data <- plot_data[plot_data$count > 0, ]
          if (nrow(plot_data) == 0) return(NULL)
          plot_data$group <- factor(plot_data$group,
                                    levels = plot_data$group[order(plot_data$count)])
          
          ggplot(plot_data, aes(x = group, y = count)) +
            geom_col(fill = "#1D9E75") +
            coord_flip() +
            labs(x = NULL, y = "Items") +
            theme_minimal() +
            theme(
              axis.text  = element_text(size = 11),
              axis.title = element_text(size = 11),
              panel.grid.major.y = element_blank()
            )
        }, height = 220)
    )
  })
  
  #Top items
  output$dashboard_top_items <- renderUI({
    df <- event_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    #Build item label lookup
    item_labels <- unlist(lapply(group_ids, function(gid) {
      setNames(groups[[gid]]$items,
               paste0(gid, "_", seq_along(groups[[gid]]$items)))
    }))
    
    #Sum all item columns
    item_cols <- names(df)[names(df) %in% names(item_labels)]
    if (length(item_cols) == 0) return(NULL)
    
    item_totals <- colSums(df[, item_cols, drop = FALSE], na.rm = TRUE)
    item_totals <- item_totals[item_totals > 0]
    if (length(item_totals) == 0) return(NULL)
    
    top5 <- sort(item_totals, decreasing = TRUE)[1:min(5, length(item_totals))]
    names(top5) <- item_labels[names(top5)]
    
    div(class = "card",
        div(class = "section-lbl", style = "margin-bottom: 10px;", "Top items"),
        lapply(seq_along(top5), function(i) {
          div(style = "display: flex; justify-content: space-between;
                     padding: 5px 0; border-bottom: 0.5px solid #eee;
                     font-size: 13px;",
              span(names(top5)[i]),
              span(style = "font-weight: 500; color: #1D9E75;", top5[i])
          )
        })
    )
  })
}

