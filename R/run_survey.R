#' @export
run_survey <- function() {

  app_dir <- system.file("survey", "surveyapp", package = "bsurvey")
  if (app_dir == "") {
    stop("Directory does not exist!", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "auto")

}
