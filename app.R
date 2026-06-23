library(shiny)
library(shinyjs)
source("storage.R")
drive_auth(email = "etapshinyapp@gmail.com")
source("gps.R")
source("summary.R")

#ETAP item definitions

groups <- list(
  
  plastic = list(
    label = "Plastic",
    items = c(
      "Bottles & Containers",
      "Straws & Stirrers",
      "Bottle Caps & Tabs",
      "Beverage Rings",
      "Food Wrappers & Snack Bags",
      "Food & Drink Pouches",
      "Cups",
      "Lids",
      "Utensils",
      "Plates & Bowls",
      "Clamshells",
      "Grocery & Retail Bags",
      "Small Fragments (1 tally = 1 cup)",
      "Other Plastic"
    )
  ),
  
  foam = list(
    label = "Foam",
    items = c(
      "Cups",
      "Plates & Bowls",
      "Clamshells",
      "Other Foam (1 tally = 1 cup)"
    )
  ),
  
  paper = list(
    label = "Paper",
    items = c(
      "Cardboard",
      "Bags",
      "Newspaper, Junk Mail, Receipts & Office Paper",
      "Cups",
      "Beverage & Food Cartons",
      "Other Paper"
    )
  ),
  
  glass = list(
    label = "Glass",
    items = c(
      "Bottles, Jars & Containers",
      "Small Fragments & Other Glass (1 tally = 1 cup)"
    )
  ),
  
  metal = list(
    label = "Metal",
    items = c(
      "Bottles, Cans & Containers",
      "Bottle Caps & Tabs",
      "Other Metal"
    )
  ),
  
  fishing = list(
    label = "Fishing",
    items = c(
      "Hooks, Lures & Floats",
      "Traps & Trap Parts",
      "Nets & Ropes (1 tally = 1 foot)",
      "Fishing Line (1 tally = 1 foot)",
      "Tangled Fishing Line Bundles (1 tally = 1 sq ft)",
      "Other Fishing"
    )
  ),
  
  automotive = list(
    label = "Automotive",
    items = c(
      "Tires",
      "Other Automotive"
    )
  ),
  
  smoking = list(
    label = "Smoking",
    items = c(
      "Cigarettes & Cannabis",
      "E-Cigarettes & Vaping",
      "Lighters"
    )
  ),
  
  other = list(
    label = "Other",
    items = c(
      "Chemical, Paint & Other Hazardous",
      "Batteries & Electronics",
      "Building Materials",
      "Furniture & Carpet",
      "Appliances",
      "Medical Waste, Sharps & Biohazardous",
      "Textiles, Clothing & Shoes",
      "Toiletries / Personal Hygiene",
      "Balloons",
      "Toys, Sports & Rec Equipment",
      "Whole Bags of Mixed Trash",
      "Write-in 1",
      "Write-in 2",
      "Write-in 3",
      "Write-in 4",
      "Write-in 5"
    )
  )
)

group_ids <- names(groups)

#Collapsible card for material groups

group_card <- function(group_id) {
  grp     <- groups[[group_id]]
  card_id <- paste0("card_", group_id)
  
  div(class = "card",
      # Clickable header toggles the body
      tags$div(
        class = "group-header",
        onclick = paste0("toggleGroup('", card_id, "')"),
        tags$span(class = "section-lbl", grp$label),
        tags$span(class = "chevron", id = paste0("chev_", card_id), "▼")
      ),
      # Collapsible body
      div(id = card_id, class = "group-body",
          lapply(seq_along(grp$items), function(i) {
            input_id <- paste0(group_id, "_", i)
            fluidRow(
              column(8, p(grp$items[i],
                          style = "margin: 6px 0; font-size: 13px;")),
              column(4,
                     numericInput(
                       inputId = input_id,
                       label   = NULL,
                       value   = 0,
                       min     = 0,
                       step    = 1
                     )
              )
            )
          })
      )
  )
}

#UI

ui <- fluidPage(
  
  tags$head(
    useShinyjs(),
    gps_script,
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body         { max-width: 500px; margin: 0 auto; padding: 12px;
                     font-family: sans-serif; background: #f5f5f5; }
      h4           { margin-top: 0; }
      .card        { background: white; border-radius: 10px; padding: 16px;
                     margin-bottom: 12px; box-shadow: 0 1px 3px rgba(0,0,0,.08); }
      .section-lbl { font-size: 11px; font-weight: 600; text-transform: uppercase;
                     letter-spacing: .06em; color: #555; }
      .group-header{ display: flex; justify-content: space-between;
                     align-items: center; cursor: pointer;
                     padding-bottom: 10px; border-bottom: 1px solid #eee;
                     margin-bottom: 10px; }
      .group-header:hover { opacity: 0.75; }
      .chevron     { font-size: 12px; color: #aaa; transition: transform .2s; }
      .group-body  { display: block; }
      .group-body.collapsed { display: none; }
      .total-box   { background: #e8f5f0; border-radius: 8px; padding: 12px 16px;
                     display: flex; justify-content: space-between;
                     align-items: center; margin-bottom: 12px; }
      .total-box span   { font-size: 13px; color: #085041; }
      .total-box strong { font-size: 22px; color: #085041; }
      .submit-btn  { width: 100%; padding: 12px; background: #1D9E75;
                     color: white; border: none; border-radius: 8px;
                     font-size: 15px; cursor: pointer; margin-bottom: 16px; }
      .submit-btn:active { opacity: .85; }
      input[type=number] { width: 70px; text-align: center; }
      .form-group  { margin-bottom: 10px; }
    ")),
    #Collapse / expand toggle JS
    tags$script(HTML("
      function toggleGroup(id) {
        var body = document.getElementById(id);
        var chev = document.getElementById('chev_' + id);
        if (body.classList.contains('collapsed')) {
          body.classList.remove('collapsed');
          chev.style.transform = 'rotate(0deg)';
        } else {
          body.classList.add('collapsed');
          chev.style.transform = 'rotate(-90deg)';
        }
      }
    "))
  ),
  
#Header

div(class = "card",
    h4("ETAP Field Entry"),
    p("Log trash items by material group below.",
      style = "color: black; font-size: 13px; margin: 0;")
),

#Session Metadata

div(class = "card",
    div(class = "section-lbl", style = "margin-bottom: 12px;", "Session info"),
    textInput("site_name",    "Site name",        placeholder = "e.g. Freedom park"),
    dateInput("cleanup_date", "Cleanup date",     value = Sys.Date()),
    textInput("volunteer_id", "Volunteer ID / name"),
    selectInput("site_type",  "Site type",
                choices = c("Terrestrial", "Aquatic edge", "In-water")),
    
    #Condition: official ETAP uses only two options
    div(class = "section-lbl", style = "margin: 10px 0 6px;", "Item condition"),
    selectInput("condition", label = NULL,
                choices = c("Intact / Unfouled", "Degraded / Heavily Fouled"))
),

#Material group cards

lapply(group_ids, group_card),

#Running Total

div(class = "total-box",
    span("Total items logged"),
    strong(textOutput("grand_total", inline = TRUE))
),

#Submit with new GPS status
gps_status_ui,
actionButton("submit", "Submit entry", class = "submit-btn",
             onclick = "captureGPS()"),

#Confirmation

uiOutput("confirmation")

)

#Server

server <- function(input, output, session) {
  gps_server(input, output)
  summary <- summary_server(input, output, session, all_input_ids, groups, group_ids)
  
  #Build a flat list of all input IDs across every group
  all_input_ids <- unlist(lapply(group_ids, function(gid) {
    paste0(gid, "_", seq_along(groups[[gid]]$items))
  }))
  
  #Reactive: sum all item counts
  total <- reactive({
    vals <- sapply(all_input_ids, function(id) {
      v <- input[[id]]
      if (is.null(v) || is.na(v)) 0L else as.integer(v)
    })
    sum(vals)
  })
  
  output$grand_total <- renderText({ total() })
  
  #On submit: show confirmation
  observeEvent(input$submit, {
    save_entry(input, all_input_ids, total())
    summary$record_entry(input, all_input_ids, total())
    
    output$confirmation <- renderUI({
      summary$render_summary(input)
    })
  })
  
  #Reset all inputs
  observeEvent(input$reset, {
    updateTextInput(session,  "site_name",    value = "")
    updateTextInput(session,  "volunteer_id", value = "")
    updateDateInput(session,  "cleanup_date", value = Sys.Date())
    updateSelectInput(session, "site_type",   selected = "Terrestrial")
    updateSelectInput(session, "condition",   selected = "Intact / Unfouled")
    for (id in all_input_ids)
      updateNumericInput(session, id, value = 0)
    output$confirmation <- renderUI({ NULL })
  })
}

#GPS Status UI
gps_status_ui <- uiOutput("gps_status_display")


shinyApp(ui, server)




