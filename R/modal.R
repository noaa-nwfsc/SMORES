#' Show a modal dialog with a spinner
#'
#' This function displays a modal dialog with a loading spinner to indicate
#' that a long-running operation is in progress.
#'
#' @param title The title to display in the modal header
#' @param message The message to display below the spinner
#' @param session The Shiny session object (optional)
#' @param placeholder_id The ID for the spinner placeholder UI element (defaults to "modalSpinnerPlaceholder")
#' @param spinner_type The type of spinner to use (defaults to 3)
#' @param spinner_color The color of the spinner (defaults to "#003087")
#' @param spinner_size The size of the spinner (defaults to 2)
#'
#' @return The modal dialog is displayed and a placeholder UI element is created
#'
#' @examples
#' # Basic usage:
#' show_spinner_modal("Processing Data", "Please wait while data is being processed...")
#'
#' # With custom spinner settings:
#' show_spinner_modal("Generating Report", "Creating your report...", 
#'                    spinner_type = 8, 
#'                    spinner_color = "#FF5733",
#'                    spinner_size = 1.5)
show_spinner_modal <- function(title, 
                               message, 
                               session = getDefaultReactiveDomain(),
                               placeholder_id = "modalSpinnerPlaceholder",
                               spinner_type = 3,
                               spinner_color = "#003087",
                               spinner_size = 2) {
  
  # Show modal with spinner
  showModal(modalDialog(
    title = title,
    div(
      style = "text-align: center; padding: 40px;",
      tags$div(
        style = "position: relative; min-height: 100px;",
        shinycssloaders::withSpinner(
          uiOutput(placeholder_id),
          type = spinner_type, 
          color = spinner_color, 
          color.background = "#FFFFFF",
          size = spinner_size
        )
      ),
      p(message)
    ),
    footer = NULL,
    size = "l",
    easyClose = FALSE,
    fade = TRUE
  ))
  
  # Render a placeholder for the spinner
  output <- session$output
  output[[placeholder_id]] <- renderUI({
    div(style = "height: 100px;")
  })
  
  # Return nothing but ensure rendering occurs
  invisible(NULL)
}