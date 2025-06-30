create_maps_container <- function(configs, namespace, combined_map_output_id, combined_map_generated, combined_map_title, selected_methods = NULL) {
  
  # Create individual map outputs
  map_outputs <- lapply(seq_along(configs), function(i) {
    map_id <- paste0(namespace, "_map_", i)
    
    div(
      class = "col-md-6 mb-3", #take up 6/12 bootstrap columns
      card(
        card_header(
          h5(paste("Map", i, ":", configs[[i]]$layer, "- Score:", configs[[i]]$score))
        ),
        card_body(
          leafletOutput(map_id, height = "400px")
        )
      )
    )
  })
  
  # Create combined map sections based on selected methods
  combined_map_sections <- NULL
  if(combined_map_generated && !is.null(selected_methods) && length(selected_methods) > 0) {
    
    combined_map_sections <- lapply(selected_methods, function(method) {
      # Create appropriate title for each method
      method_title <- switch(method,
                             "geometric_mean" = "Combined Map - Geometric Mean",
                             "lowest" = "Combined Map - Lowest Value",
                             "product" = "Combined Map - Product",
                             "Combined Map")
      
      # Create appropriate output ID for each method
      output_id <- if(method == "geometric_mean") {
        combined_map_output_id  # Use the main output ID for geometric mean
      } else {
        paste0(combined_map_output_id, "_", method)
      }
      
      div(
        class = "col-12 mb-3", # take up 12/12 bootstrap columns
        card(
          card_header(
            h4(method_title)
          ),
          card_body(
            leafletOutput(output_id, height = "500px")
          )
        )
      )
    })
  }
  
  # Return the complete container
  div(
    class = "container-fluid",
    if(length(configs) > 0) {
      div(
        class = "row",
        map_outputs
      )
    } else {
      div(
        class = "alert alert-info",
        "No valid map configurations found. Please enable at least one map and select both a layer and score."
      )
    },
    if(!is.null(combined_map_sections)) {
      div(
        class = "row",
        combined_map_sections
      )
    }
  )
}