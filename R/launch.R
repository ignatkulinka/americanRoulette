#' Run the shiny application inside a web browser window
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "americanRoulette"),
                display.mode = "normal",
                launch.browser = TRUE)
}


