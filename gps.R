#Handles GPS coordinate capture via the browser geo location API.
#This file is sourced by app.R at startup.
#GPS is captured per submission and is optional — entries save without it.

#GPS
#Call this inside tags$head() in app.R to load the GPS capture function

gps_script <- tags$script(HTML("
  function captureGPS() {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(
        function(position) {
          // Success — pass coordinates to Shiny
          Shiny.setInputValue('gps_lat', position.coords.latitude);
          Shiny.setInputValue('gps_lng', position.coords.longitude);
          Shiny.setInputValue('gps_status', 'locked');
        },
        function(error) {
          // Failed or denied — pass NA to Shiny
          Shiny.setInputValue('gps_lat', null);
          Shiny.setInputValue('gps_lng', null);
          Shiny.setInputValue('gps_status', 'failed');
        },
        {
          enableHighAccuracy: true,
          timeout: 10000
        }
      );
    } else {
      // Browser doesn't support geolocation
      Shiny.setInputValue('gps_status', 'unsupported');
    }
  }
"))

#GPS status for UI
# Drop this anywhere in your UI to show the current GPS status to the volunteer

gps_status_ui <- uiOutput("gps_status_display")

# ── GPS status server ─────────────────────────────────────────────────────────
# Call gps_server(input, output) inside your server function in app.R

gps_server <- function(input, output) {
  output$gps_status_display <- renderUI({
    status <- input$gps_status
    
    if (is.null(status)) {
      # GPS not yet requested
      div(style = "font-size: 12px; color: #888; margin-bottom: 10px;",
          "\u23f3 GPS will capture on submission")
      
    } else if (status == "locked") {
      div(style = "font-size: 12px; color: #1D9E75; margin-bottom: 10px;",
          paste0("\u2713 GPS captured: ",
                 round(input$gps_lat, 5), ", ",
                 round(input$gps_lng, 5)))
      
    } else if (status == "failed") {
      div(style = "font-size: 12px; color: #e07b39; margin-bottom: 10px;",
          "\u26a0 GPS unavailable \u2014 entry will save without coordinates")
      
    } else if (status == "unsupported") {
      div(style = "font-size: 12px; color: #e07b39; margin-bottom: 10px;",
          "\u26a0 GPS not supported by this browser")
    }
  })
}

