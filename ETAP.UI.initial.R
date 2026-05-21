library(shiny)

#Item types my material groups

plastic_items <- c(
  "Bottles & caps",
  "Bags & film",
  "Straws & utensils",
  "Foam pieces",
  "Other plastic"
)

other_items <- c(
  "Paper & cardboard",
  "Metal cans & foil",
  "Glass",
  "Cigarette butts",
  "Textiles & rubber"
)

#Numeric inputs for material groups

item_inputs <- function(group_id, items) {
  lapply(seq_along(items), function(i) {
    fluidRow(
      column(8, p(items[i], style = "margin: 8px 0;")),
      column(4,
             numericInput(
               inputId  = paste0(group_id, "_", i),
               label    = NULL,
               value    = 0,
               min      = 0,
               step     = 1
             )
      )
    )
  })
}

#UI

ui <- fluidPage(
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body        { max-width: 480px; margin: 0 auto; padding: 12px;
                    font-family: sans-serif; background: #f5f5f5; }
      h4          { margin-top: 0; }
      .card       { background: white; border-radius: 10px; padding: 16px;
                    margin-bottom: 14px; box-shadow: 0 1px 3px rgba(0,0,0,.08); }
      .section-lbl{ font-size: 11px; text-transform: uppercase;
                    letter-spacing: .06em; color: #888; margin-bottom: 8px; }
      .total-box  { background: #e8f5f0; border-radius: 8px; padding: 12px 16px;
                    display: flex; justify-content: space-between;
                    align-items: center; margin-bottom: 14px; }
      .total-box span { font-size: 13px; color: #085041; }
      .total-box strong { font-size: 22px; color: #085041; }
      .submit-btn { width: 100%; padding: 12px; background: #1D9E75;
                    color: white; border: none; border-radius: 8px;
                    font-size: 15px; cursor: pointer; }
      .numeric-label { display: none; }
      input[type=number] { width: 70px; text-align: center; }
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
    div(class = "section-lbl", "Session info"),
    textInput("site_name", "Site name", placeholder = "e.g. Freedom Park"),
    dateInput("cleanup_date", "Cleanup date", value = Sys.Date()),
    textInput("volunteer_id", "Volunteer ID / name"),
    selectInput("site_type", "Site type",
                choices = c("Terrestrial", "Aquatic edge", "In-water")),
    selectInput("condition", "Item condition",
                choices = c("Intact", "Degraded", "Fragment"))
),

#Plastic items
div(class = "card",
    div(class = "section-lbl", "Plastic"),
    item_inputs("plastic", plastic_items)
),

#Other materials
div(class = "card",
    div(class = "section-lbl", "Other materials"),
    item_inputs("other", other_items)
),

#Running total
div(class = "total-box",
    span("Total items logged"),
    strong(textOutput("grand_total", inline = TRUE))
),

#Submit
actionButton("submit", "Submit entry", class = "submit-btn"),

#Confirmation message (hidden until submit)
br(), br(),
uiOutput("confirmation")
)

#Server

server <- function(input, output, session) {
  
  # Reactive: sum all item counts
  total <- reactive({
    plastic_vals <- sapply(seq_along(plastic_items), function(i)
      input[[paste0("plastic_", i)]] %||% 0)
    other_vals   <- sapply(seq_along(other_items),   function(i)
      input[[paste0("other_",   i)]] %||% 0)
    sum(plastic_vals, other_vals, na.rm = TRUE)
  })
  
  output$grand_total <- renderText({ total() })
  
  # On submit: show confirmation (storage comes later)
  observeEvent(input$submit, {
    output$confirmation <- renderUI({
      div(class = "card", style = "background: #e8f5f0;",
          h4(paste0("Entry saved", total(), " items logged")),
          p(paste("Site:", input$site_name)),
          p(paste("Date:", format(input$cleanup_date, "%B %d, %Y"))),
          p(paste("Condition:", input$condition)),
          actionButton("reset", "Log another entry",
                       style = "margin-top: 8px;")
      )
    })
  })
  
  # Reset all inputs
  observeEvent(input$reset, {
    updateTextInput(session, "site_name",     value = "")
    updateTextInput(session, "volunteer_id",  value = "")
    updateDateInput(session, "cleanup_date",  value = Sys.Date())
    updateSelectInput(session, "site_type",   selected = "Terrestrial")
    updateSelectInput(session, "condition",   selected = "Intact")
    for (i in seq_along(plastic_items))
      updateNumericInput(session, paste0("plastic_", i), value = 0)
    for (i in seq_along(other_items))
      updateNumericInput(session, paste0("other_", i),   value = 0)
    output$confirmation <- renderUI({ NULL })
  })
}

shinyApp(ui, server)



