#' @export
run_survey <- function() {

  appDir <- system.file("survey", "surveyapp", package = "bsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `survey`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "auto")

}
