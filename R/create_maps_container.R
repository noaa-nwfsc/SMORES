create_maps_container <- function(configs, namespace, combined_map_output_id, combined_map_generated, combined_map_title) {
  
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
  
  # Create the combined map section if it has been generated
  combined_map_section <- NULL
  if(combined_map_generated) {
    combined_map_section <- div(
      class = "col-12 mb-3", # take up 12/12 bootstrap columns
      card(
        card_header(
          h4(combined_map_title)
        ),
        card_body(
          leafletOutput(combined_map_output_id, height = "500px")
        )
      )
    )
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
    if(!is.null(combined_map_section)) combined_map_section
  )
}