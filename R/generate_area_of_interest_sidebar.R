generate_area_of_interest_sidebar <- function() {
  nav_panel(
    title = "Area of Interest",
    icon = icon("map-location-dot"),
    card(
      card_header("Area of Interest"),
      card_body(
        includeMarkdown("markdown/area_of_interest.md")
      )
    ),
    card(
      card_header("Area Selection"),
      card_body(
        pickerInput(
          "weaAreaSelector",
          "Select Energy Area:",
          choices = NULL,
          selected = NULL,
          multiple = FALSE,
          options = list(
            `none-selected-text` = " No area selected"
          )
        ),
        p("Select one Wind Energy Area to filter the dataset. The map will update to show only the selected area."),
        #Summary information
        conditionalPanel(condition = "input.weaAreaSelector != null && input.weaAreaSelector != ''",
                         div(
                           style = "margin-top: 15px; background-color: #f8f9fa; border-radius: 5px;"
                         )
        )
      )
    ),
    card(card_header("WEA Areas Map"),
         card_body(
           p("this map shows the Wind Energy Areas (WEA's). Use the selector above to filter areas by name."),
           leafletOutput("weaMap", height = "500px")
         )
    )
  )
}