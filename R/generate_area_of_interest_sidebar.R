generate_area_of_interest_sidebar <- function() {
  nav_panel(
    title = "Area of Interest",
    icon = icon("map-location-dot"),
    # Top row: Two vertical rectangles side by side
    layout_columns(
      col_widths = c(6, 6),
      
      # Left column: Description
      card(
        card_header("Area of Interest"),
        card_body(
          style = "height: 550px; overflow-y: auto;",
          includeMarkdown("markdown/area_of_interest.md")
        )
      ),
      
      # Right column: Area Selection Info
      card(
        card_header("Area Selection Information"),
        card_body(
          style = "height: 300px; overflow-y: hidden;",
          
          div(
            style = "text-align: center;",
            radioButtons(
              "weaAreaSelector",
              "Select Energy Area:",
              choices = c("Loading..." = "loading"),
              selected = "loading",
              inline = TRUE
            ),
            p("Select one Wind Energy Area to filter the dataset. The map will update to show only the selected area.")
          ),
          
          leafletOutput("weaMap", height = "250px")
        )
      )
    )
  )
}