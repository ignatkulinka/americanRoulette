#' Run the graphical interface to the game in a web browser
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "americanRoulette"),
                display.mode = "normal",
                launch.browser = TRUE)
}


