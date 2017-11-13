#' Run app
#'
#' Runs the Shiny App
#' @return The Shiny App
#' @export
#'
#' @examples
#' \dontrun{
#' runApp()
#' }
runApp <- function() {
  appDir <- system.file("app", package = "cadasterwebui")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `cadasterwebui`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
