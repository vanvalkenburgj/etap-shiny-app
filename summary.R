#Volunteer summary screen shown after each submission.
#Tracks a running session total across multiple submissions.

library(ggplot2)

#summary_server: call this inside the server function in app.R

summary_server <- function(input, output, session, all_input_ids, groups, group_ids) {
  
  #Session accumulator
  #Stores all submissions from this session as a list of rows
  session_entries <- reactiveVal(list())
  
  #record_entry: called on each submit to add to the session total
  record_entry <- function(input, all_input_ids, total) {
    
    item_vals <- sapply(all_input_ids, function(id) {
      v <- input[[id]]
      if (is.null(v) || is.na(v)) 0L else as.integer(v)
    })
    
    new_entry <- list(
      timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      site_name   = input$site_name,
      condition   = input$condition,
      total_items = total,
      item_vals   = item_vals
    )
    
    current <- session_entries()
    session_entries(c(current, list(new_entry)))
  }
  
  #session_total: sum of all items across all submissions this session
  session_total <- reactive({
    entries <- session_entries()
    if (length(entries) == 0) return(0)
    sum(sapply(entries, function(e) e$total_items))
  })
  
  #session_by_group: item counts summed by material group
  session_by_group <- reactive({
    entries <- session_entries()
    if (length(entries) == 0) return(NULL)
    
    #Sum item_vals across all entries
    combined <- Reduce("+", lapply(entries, function(e) e$item_vals))
    
    #Map each input ID back to its group label
    group_totals <- sapply(group_ids, function(gid) {
      ids <- paste0(gid, "_", seq_along(groups[[gid]]$items))
      sum(combined[ids], na.rm = TRUE)
    })
    
    data.frame(
      group = sapply(group_ids, function(gid) groups[[gid]]$label),
      count = as.integer(group_totals),
      stringsAsFactors = FALSE
    )
  })
  
  #session_top_items: top 5 individual items logged this session
  session_top_items <- reactive({
    entries <- session_entries()
    if (length(entries) == 0) return(NULL)
    
    combined <- Reduce("+", lapply(entries, function(e) e$item_vals))
    
    #Build a named vector of item label -> count
    item_labels <- unlist(lapply(group_ids, function(gid) {
      groups[[gid]]$items
    }))
    names(combined) <- item_labels
    
    #Sort and take top 5 with count > 0
    sorted <- sort(combined, decreasing = TRUE)
    sorted <- sorted[sorted > 0]
    if (length(sorted) == 0) return(NULL)
    head(sorted, 5)
  })
  
  #session_condition_split: intact vs degraded across session
  session_condition <- reactive({
    entries <- session_entries()
    if (length(entries) == 0) return(list(intact = 0, degraded = 0))
    
    intact   <- sum(sapply(entries, function(e)
      if (grepl("Intact", e$condition)) e$total_items else 0))
    degraded <- sum(sapply(entries, function(e)
      if (grepl("Degraded", e$condition)) e$total_items else 0))
    
    list(intact = intact, degraded = degraded)
  })
  
  #render_summary: builds the full summary UI after each submission
  render_summary <- function(input) {
    by_group  <- session_by_group()
    top_items <- session_top_items()
    condition <- session_condition()
    n_entries <- length(session_entries())
    
    tagList(
      
      #Session stats card
      div(class = "card", style = "background: #e8f5f0;",
          h4(paste0("\u2713 Entry logged \u2014 ", n_entries,
                    ifelse(n_entries == 1, " submission", " submissions"),
                    " this session")),
          p(paste("Site:", input$site_name)),
          p(paste("Date:", format(input$cleanup_date, "%B %d, %Y"))),
          div(style = "display: flex; gap: 24px; margin-top: 8px;",
              div(
                div(style = "font-size: 11px; color: #555; text-transform: uppercase;
                         letter-spacing: .05em;", "Session total"),
                div(style = "font-size: 28px; font-weight: 500; color: #085041;",
                    session_total())
              ),
              div(
                div(style = "font-size: 11px; color: #555; text-transform: uppercase;
                         letter-spacing: .05em;", "Intact"),
                div(style = "font-size: 28px; font-weight: 500; color: #1D9E75;",
                    condition$intact)
              ),
              div(
                div(style = "font-size: 11px; color: #555; text-transform: uppercase;
                         letter-spacing: .05em;", "Degraded"),
                div(style = "font-size: 28px; font-weight: 500; color: #e07b39;",
                    condition$degraded)
              )
          ),
          # GPS status line
          div(style = "margin-top: 10px; font-size: 12px; color: #555;",
              if (!is.null(input$gps_lat)) {
                paste0("\u2713 GPS: ", round(input$gps_lat, 5),
                       ", ", round(input$gps_lng, 5))
              } else {
                "\u26a0 No GPS for this entry"
              }
          )
      ),
      
      # Material breakdown chart
      if (!is.null(by_group) && sum(by_group$count) > 0) {
        div(class = "card",
            div(class = "section-lbl", style = "margin-bottom: 10px;",
                "Material breakdown \u2014 session total"),
            renderPlot({
              plot_data <- by_group[by_group$count > 0, ]
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
      },
      
      #Top items
      if (!is.null(top_items)) {
        div(class = "card",
            div(class = "section-lbl", style = "margin-bottom: 10px;",
                "Top items this session"),
            lapply(seq_along(top_items), function(i) {
              div(style = "display: flex; justify-content: space-between;
                         padding: 5px 0; border-bottom: 0.5px solid #eee;
                         font-size: 13px;",
                  span(names(top_items)[i]),
                  span(style = "font-weight: 500; color: #1D9E75;", top_items[i])
              )
            })
        )
      },
      
      #Log another entry button
      actionButton("reset", "Log another entry",
                   style = "width: 100%; padding: 12px; background: white;
                            border: 1.5px solid #1D9E75; color: #1D9E75;
                            border-radius: 8px; font-size: 15px;
                            cursor: pointer; margin-bottom: 16px;")
    )
  }
  
  #Return both record_entry and render_summary so app.R can call them
  list(
    record_entry   = record_entry,
    render_summary = render_summary
  )
}

