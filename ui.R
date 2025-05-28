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
    title = "Submodels",
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
              p("Select layers and weights in the sidebar to generate individual maps."),
              p("You can configure up to 5 different maps with their own settings.")
            )
          ),
          
          # Container for multiple maps
          uiOutput("multipleMapsContainer"),
          
          # Combined map at the bottom
          uiOutput("combinedMapContainer")
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
          title = "Fisheries",
          icon = icon("fish-fins"),
          value = "fisheries",
          layout_columns(
            card(
              card_header("Fisheries Submodel"),
              card_body(
                p("we love fish")
              )
            )
          )
        )
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
        # h3("data sources"),
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
