tagList(
  # Custom header with logo
  div(
    class = "container-fluid",
    style = "margin: 10px 0; display: flex; justify-content: space-between; align-items: center;",
    
    # Left side blank with a placeholder
    div(style = "visibility: hidden;", "Placeholder"),
    
    # Right side will have NOAA logo
    div(
      # note do not include www folder in file path or link will appear broken
      img(src = "logos/NOAA_FISHERIES_logoH_web.png", height = "60px", alt = "NOAA Fisheries Logo")
    )
  ),
  
  # Navbar
  page_navbar(
    theme = theme %>% 
      bs_add_rules(
        # Custom CSS to style the navbar
        ".navbar-brand { color: white !important; }
       
       /* Style for top-level navbar links only - blue background with white text */
       .navbar .nav-item .nav-link:not(.active) { 
         color: white !important; 
         background-color: #003087 !important;
       }
       
       /* Style for active top-level navbar links only - white background with blue text*/
       .navbar .nav-item .nav-link.active { 
         color: #003087 !important;
         background-color: white !important;
         border-radius: 4px 4px 0 0;
       }
       
       /* Background for the nav-tabs container but NOT for the main navbar - light gray*/
       .navset-tab > .nav-tabs,
       #dataTabs > .nav-tabs,
       .card .nav-tabs,
       div:not(.navbar) .nav-tabs {
         display: flex;
         justify-content: center;
         background-color: #F1F2F3 !important;
         padding-top: 4px !important;
         border-bottom: none !important;
       }
       
       /* Non-active inset tab links but NOT navbar links -  */
       .navset-tab > .nav-tabs .nav-link:not(.active),
       #dataTabs > .nav-tabs .nav-link:not(.active),
       .card .nav-tabs .nav-link:not(.active),
       div:not(.navbar) .nav-tabs .nav-link:not(.active) {
         background-color: #F1F2F3 !important;
         color: #323C46 !important;
         border: none !important;
         margin-right: 2px !important;
         border-radius: 4px 4px 0 0 !important;
       }
       
       /* Active inset tab links but NOT navbar links */
       .navset-tab > .nav-tabs .nav-link.active,
       #dataTabs > .nav-tabs .nav-link.active,
       .card .nav-tabs .nav-link.active,
       div:not(.navbar) .nav-tabs .nav-link.active {
         background-color: white !important;
         color: black !important;
         border: none !important;
         border-top: 3px solid #0085CA !important;
         margin-right: 2px !important;
         border-radius: 4px 4px 0 0 !important;
       }
       
       /* Fix tab content area background */
       .navset-tab > .tab-content,
       #dataTabs > .tab-content,
       .card .tab-content,
       div:not(.navbar) .tab-content {
         background-color: white !important;
         padding: 15px !important;
         border-radius: 0 0 4px 4px !important;
       }
       
       /* Hover effect for inset tabs */
       .navset-tab > .nav-tabs .nav-link:hover:not(.active),
       #dataTabs > .nav-tabs .nav-link:hover:not(.active),
       .card .nav-tabs .nav-link:hover:not(.active),
       div:not(.navbar) .nav-tabs .nav-link:hover:not(.active) {
         background-color: #E0E0E0 !important;
       }
       
       /* Direct targeting for dynamically created tabs in the UI */
       #dataTabs .nav-tabs {
         background-color: #F1F2F3 !important;
       }
       
       #dataTabs .nav-tabs .nav-item .nav-link:not(.active) - gray background with black text {
         background-color: #F1F2F3 !important;
         color: #323C46 !important;
         border: #E0E0E0 !important;
       }
       
       #dataTabs .nav-tabs .nav-item .nav-link.active - white background with black text and blue border {
         background-color: white !important;
         color: black !important;
         border: none !important;
         border-top: 3px solid #0085CA !important;
       }"
      ),
    title = "Suitability Modeling", # title text
    
    # Tab 1: Overview
    nav_panel(
      title = "Overview",
      icon = icon("house-chimney"),
      layout_columns(
        card(
          card_header("Overview"),
          card_body(
            includeMarkdown("markdown/overview.md")
          )
        )
      )
    ),
    
    # Tab 2: Natural Resources Submodel Tab with Inset Tabs
    nav_panel(
      title = "Natural Resources Submodel",
      icon = icon("cloud-sun"),
      
      # Layout with sidebar for tabs
      layout_sidebar(
        # Sidebar for picker inputs that change based on tab selection
        sidebar = sidebar(
          # Dynamic picker inputs
          uiOutput("dynamicSidebar") # Settings for dynamic sidebar live in server.R
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
                includeMarkdown("markdown/habitat_map_settings.md")
              )
            ),
            
            # Container for multiple maps
            uiOutput("multipleMapsContainer")
            
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
                  includeMarkdown("markdown/species.md")
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
                  includeMarkdown("markdown/birds.md")
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
                  includeMarkdown("markdown/combined_natural_resources_submodel.md")
                )
              )
            )
          )
        )
      )
    ),
    # Tab 3: Fisheries Submodel
    nav_panel(
      title = "Fisheries Submodel",
      icon = icon("fish-fins"),
      card(
        card_header("Fisheries Submodel"),
        card_body(
          includeMarkdown("markdown/fisheries.md")
        )
      )
    ), 
    # Tab 4: Industry & Operations Submodel
    nav_panel(
      title = "Industry & Operations Submodel",
      icon = icon("industry"),
      
      # Layout with sidebar for tabs
      layout_sidebar(
        # Sidebar for picker inputs
        sidebar = sidebar(
          # Dynamic picker inputs
          uiOutput("industryOperationsSidebar")
        ),
        card(
          card_header("Industry & Operations Submodel"),
          card_body(
            includeMarkdown("markdown/industry_operations_submodel.md")
          )
        ),
        # Container for multiple maps
        uiOutput("industryMapContainer")
      )
    ),
    # Tab 5: Methods
    nav_panel(
      title = "Methods",
      icon = icon("route"),
      card(
        card_header("Methods"),
        card_body(
          includeMarkdown("markdown/methods.md")
        )
      )
    ), 
    # Tab 6: Data
    nav_panel(
      title = "Data",
      icon = icon("database"),
      card(
        card_header("Dataset Information"),
        card_body(
          p("This application uses multiple datasets that are updated periodically."),
          p(strong("Most recent data update: "), most_recent_update),
          br(),
          tableOutput("data_timestamps_table")
        )
      )
    )
  )
)



