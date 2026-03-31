generate_area_of_interest_sidebar <- function() {
  nav_panel(
    title = "Area of Interest",
    icon = icon("map-pin"),
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
          style = "height: 550px; overflow-y: auto; display: block;",

          div(
            style = "text-align: center;",
            # --- THE FIX: Hardcoded static choices ---
            radioButtons(
              "aoiAreaSelector",
              "Select Area of Interest:",
              choices = c(
                "All Areas" = "all",
                "Brookings" = "Brookings",
                "Coos Bay" = "Coos Bay",
                "Morro Bay" = "Morro Bay",
                "Humboldt" = "Humboldt",
                "Southern California" = "Southern California",
                "Central California" = "Central California",
                "Northern California" = "Northern California",
                "Washington/Oregon" = "Washington/Oregon",
                "AB 525 Suitable Sea Space" = "AB 525 Suitable Sea Space",
                "AB 525 Sea Space Area of Interest" = "AB 525 Sea Space Area of Interest"
              ),
              selected = "all",
              inline = TRUE
            ),
            p(
              "Select one Area of Interest to filter the dataset. The map will update to highlight the selected area. *Please note the `All Areas` option is for visual purposes **only**. To generate a model run please select any other area of interest "
            )
          ),

          # Dynamic banner placeholder
          uiOutput("aoi_grid_banner"),

          div(
            style = "min-height: 350px; margin-bottom: 15px;",
            leafletOutput("aoiMap", height = "350px")
          )
        )
      )
    )
  )
}
