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
    theme = theme,
    # %>%
    # bs_add_rules(
    #     # # Custom CSS to directly target the active nav link
    #     # ".nav-item .nav-link.active { color: #0085CA !important; }
    #     # .nav-item .nav-link:not(.active) { color: #004391 !important; }
    #     # .navbar-brand { color: #0085CA !important; }
    #   ".nav-tabs .nav-link:not(.active) {background-color: #287CB8 !important; }"),
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
      card(
        card_header("Industry & Operations Submodel"),
        card_body(
          includeMarkdown("markdown/industry_operations_submodel.md")
        )
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
        card_header("Data Sources"),
        card_body(
          includeMarkdown("markdown/data.md")
        )
      )
    )
  )
)


  
  