tagList(
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  ),

  # Custom header with logo
  div(
    class = "container-fluid",
    style = "margin: 10px 0; display: flex; justify-content: space-between; align-items: center;",

    # Left side blank with a placeholder
    div(style = "visibility: hidden;", "Placeholder"),

    # Right side will have NOAA logo
    div(
      # note do not include www folder in file path or link will appear broken
      img(
        src = "logos/NOAA_FISHERIES_logoH.png",
        height = "60px",
        alt = "NOAA Fisheries Logo"
      )
    )
  ),

  # Navbar
  page_navbar(
    theme = theme,
    title = "Suitability Modeling", # title text
    fillable = TRUE,

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
    # Tab 2: Area of Interest
    generate_area_of_interest_sidebar(),

    # Tab 3: Natural Resources Submodel Tab with Inset Tabs
    nav_panel(
      title = "Natural Resources Submodel",
      icon = icon("sun"),

      # Layout with sidebar for tabs
      layout_sidebar(
        # Sidebar for picker inputs that change based on tab selection
        sidebar = sidebar(
          # static UI container for habitat
          conditionalPanel(
            condition = "input.dataTabs_natural_resources == 'habitat'",
            generate_habitat_sidebar(names(habitat_layer), score_values)
          ),

          # static ui container for species
          conditionalPanel(
            condition = "input.dataTabs_natural_resources == 'species'",
            generate_species_sidebar(names(species_layer), score_values)
          ),

          # static ui container for combined natural resources
          conditionalPanel(
            condition = "input.dataTabs_natural_resources == 'combined_model_natural_resources'",
            generate_natural_resources_combined_sidebar()
          )
        ),
        # Main area with the tab navigation
        navset_tab(
          id = "dataTabs_natural_resources",
          # Inset Tab 1
          nav_panel(
            title = "Habitat",
            icon = icon("earth-oceania"),
            value = "habitat",

            # Info about map settings
            card(
              card_header("Map Configuration"),
              card_body(
                includeMarkdown("markdown/habitat.md")
              )
            ),

            # Container for multiple maps
            uiOutput("multipleMapsContainer_habitat")
          ),

          # Inset Tab 2
          nav_panel(
            title = "Species",
            icon = icon("otter"),
            value = "species",

            # Info about map settings
            card(
              card_header("Scoring Information"),
              card_body(
                includeMarkdown("markdown/species.md")
              )
            ),

            # Container for multiple maps
            uiOutput("multipleMapsContainer_species")
          ),
          # Inset Tab 3
          nav_panel(
            title = "Combined Submodel",
            icon = icon("object-group"),
            value = "combined_model_natural_resources",

            # Info card
            card(
              card_header("Combined Natural Resources Submodel"),
              card_body(
                includeMarkdown(
                  "markdown/combined_natural_resources_submodel.md"
                )
              )
            ),

            # Map container - ALWAYS present, conditionally populated
            card(
              card_header("Combined Natural Resources Submodel Result"),
              card_body(
                # Placeholder that gets replaced when map is generated
                uiOutput("naturalResourcesCombinedMapContainer")
              )
            )
          )
        )
      )
    ),
    # Tab 4: Fisheries Submodel
    nav_panel(
      title = "Fisheries Submodel",
      icon = icon("fish-fins"),
      # Layout with sidebar for tabs
      layout_sidebar(
        # Sidebar for picker inputs that change based on tab selection
        sidebar = sidebar(
          # fisheries sidebar static
          conditionalPanel(
            condition = "input.dataTabs_fisheries == 'fisheries'",
            generate_fisheries_sidebar(
              names(fisheries_layer),
              score_values_ranked_importance
            )
          ),

          # trawl sidebar static
          conditionalPanel(
            condition = "input.dataTabs_fisheries == 'trawl'",
            generate_trawl_fisheries_sidebar(
              trawl_fisheries_layer,
              score_values_trawl_fisheries
            )
          ),

          # fisheries combined sidebar
          conditionalPanel(
            condition = "input.dataTabs_fisheries == 'combined_model_fisheries'",
            generate_fisheries_combined_sidebar()
          )
        ),
        # Main area with the tab navigation
        navset_tab(
          id = "dataTabs_fisheries",
          # Inset Tab 1
          nav_panel(
            title = "Fisheries",
            icon = icon("microscope"),
            value = "fisheries",

            # Info about map settings
            card(
              card_header("Map Configuration"),
              card_body(
                includeMarkdown("markdown/fisheries.md")
              )
            ),

            # Container for multiple maps
            uiOutput("multipleMapsContainer_fisheries")
          ),
          # Inset Tab 2
          nav_panel(
            title = "Trawl Fisheries",
            icon = icon("fish-fins"),
            value = "trawl",

            # Info about map settings
            card(
              card_header("Map Configuration"),
              card_body(
                includeMarkdown("markdown/trawl_fisheries.md")
              )
            ),

            # Container for multiple maps
            uiOutput("multipleMapsContainer_trawl")
          ),
          # Inset Tab 3
          nav_panel(
            title = "Combined Submodel",
            icon = icon("object-group"),
            value = "combined_model_fisheries",

            # Info card
            card(
              card_header("Combined Fisheries Submodel"),
              card_body(
                includeMarkdown("markdown/combined_fisheries_submodel.md")
              )
            ),
            card(
              card_header("Combined Fisheries Submodel Result"),
              card_body(
                uiOutput("fisheriesCombinedMapContainer")
              )
            )
          )
        )
      )
    ),
    # Tab 5: Industry & Operations Submodel
    nav_panel(
      title = "Industry & Operations Submodel",
      icon = icon("ship"),
      # Layout with sidebar for tabs
      layout_sidebar(
        # Sidebar for picker inputs that change based on tab selection
        sidebar = sidebar(
          # static surveys sidebar
          conditionalPanel(
            condition = "input.dataTabs_industry_operations == 'surveys'",
            generate_surveys_sidebar(names(surveys_layer), score_values)
          ),

          # static cables sidebar
          conditionalPanel(
            condition = "input.dataTabs_industry_operations == 'cables'",
            generate_cables_sidebar(names(submarine_cables_layer), score_values)
          ),

          # combined static sidebar
          conditionalPanel(
            condition = "input.dataTabs_industry_operations == 'combined_model_industry_operations'",
            generate_industry_operations_combined_sidebar()
          )
        ),
        # Main area with the tab navigation
        navset_tab(
          id = "dataTabs_industry_operations",
          # Inset Tab 1
          nav_panel(
            title = "Scientific Surveys",
            icon = icon("microscope"),
            value = "surveys",

            # Info about map settings
            card(
              card_header("Map Configuration"),
              card_body(
                includeMarkdown("markdown/surveys.md")
              )
            ),

            # Container for multiple maps
            uiOutput("multipleMapsContainer_surveys")
          ),

          # Inset Tab 2
          nav_panel(
            title = "Submarine Cables",
            icon = icon("industry"),
            value = "cables",

            # Info about map settings
            card(
              card_header("Map Configuration"),
              card_body(
                includeMarkdown("markdown/cables.md")
              )
            ),

            # Container for multiple maps
            uiOutput("multipleMapsContainer_cables")
          ),
          # Inset Tab 3
          nav_panel(
            title = "Combined Submodel",
            icon = icon("object-group"),
            value = "combined_model_industry_operations",

            # Info card
            card(
              card_header("Combined Industry & Operations Submodel"),
              card_body(
                includeMarkdown(
                  "markdown/combined_industry_operations_submodel.md"
                )
              )
            ),
            card(
              card_header("Combined Industry & Operations Submodel Result"),
              card_body(
                uiOutput("industryOperationsCombinedMapContainer")
              )
            )
          )
        )
      )
    ),
    # Tab 5: Model Output
    nav_panel(
      title = "Full Model",
      icon = icon("calculator"),
      # Layout with sidebar
      layout_sidebar(
        sidebar = sidebar(
          generate_full_model_sidebar()
        ),

        # Main content area
        card(
          card_header("Full Combined Model"),
          card_body(
            includeMarkdown("markdown/full_model.md"),

            # Add the single line here:
            htmlOutput("fullCombinedMapContainer")
          )
        )
      )
    ),
    # Tab 6: Methods
    nav_panel(
      title = "Methods",
      icon = icon("route"),
      card(
        card_header("Methods"),
        card_body(
          withMathJax(includeMarkdown("markdown/methods.md")) # function within R Shiny to render LaTex equations
        )
      )
    ),
    # Tab 7: Data
    nav_panel(
      title = "Data",
      icon = icon("database"),

      # Info card
      card(
        card_header("Dataset Information"),
        card_body(
          p(strong("Most recent data update: "), most_recent_update),

          uiOutput("active_grid_banner"),

          # data table that shows when data has been edited
          tableOutput("data_timestamps_table")
        )
      ),
    )
  )
)
