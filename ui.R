page_navbar(
  title = "Suitability Modeling",
  theme = bs_theme(bootswatch = "cerulean"),
  
  # Tab 1: Overview
  nav_panel(
    title = "Overview",
    icon = icon("house-chimney"),
    layout_columns(
      card(
        card_header("Overview"),
        card_body(
          p("Information about project and/or maybe information about how to use tool")
        )
      )
    )
  ),
  
  # Tab 2: Data Table with Inset Tabs
  nav_panel(
    title = " Natural Resources Submodel",
    icon = icon("sliders"),
    
    # Layout with sidebar for picker inputs and main area for tabs
    layout_sidebar(
      # Sidebar for picker inputs that change based on tab selection
      sidebar = sidebar(
        # Container for dynamic picker inputs
        uiOutput("dynamicSidebar")
      ),
      # Main area with the tab navigation
      navset_tab(
        id = "dataTabs",
        # Inset Tab 1
        nav_panel(
          title = "Habitat",
          icon = icon("earth-oceania"),
          value = "habitat",
          
          # Info about map settings
          card(
            card_header("Map Configuration"),
            card_body(
              p("Select layers and scores in the sidebar to generate individual maps."),
              p("You can configure up to 5 different maps with their own settings."),
              p("Click 'Generate Combined Map' to calculate the geometric mean of all selected maps.")
            )
          ),
          
          # Container for multiple maps
          uiOutput("multipleMapsContainer")
          
          # Note: The combinedMapContainer is removed since the combined map is 
          # already included in the multipleMapsContainer in the server code
        ),
        
        # Inset Tab 2
        nav_panel(
          title = "Species",
          icon = icon("otter"),
          value = "species",
          layout_columns(
            card(
              card_header("Species Submodel"),
              card_body(
                p("we love cetaceans and other species")
              )
            )
          )
        ),
        # Inset Tab 3
        nav_panel(
          title = "Birds",
          icon = icon("crow"),
          value = "birds",
          layout_columns(
            card(
              card_header("Birds Submodel"),
              card_body(
                p("we love birds")
              )
            )
          )
        ),
        # Inset Tab 4
        nav_panel(
          title = "Combined Submodel",
          icon = icon("object-group"),
          value = "combined_model",
          layout_columns(
            card(
              card_header("Combined Submodel"),
              card_body(
                p("we love everybody loading together")
              )
            )
          )
        )
      )
    )
  ),
  # Tab 3: Methods
  nav_panel(
    title = "Methods",
    icon = icon("route"),
    card(
      card_header("Methods"),
      card_body(
        p("how did we come up with any of this."),
      )
    )
  ), 
  # Tab 3: Data
  nav_panel(
    title = "Data",
    icon = icon("database"),
    card(
      card_header("Data Sources"),
      card_body(
        p("What data was used in order to create these modeling outputs."),
        p("Data Sources:"),
        tags$ul(
          tags$li("Data Source"),
          tags$li("Data Source"),
          tags$li("Data Source")
        ),
        hr(),
        p("Created to help inform modeling processes.")
      )
    )
  )
)