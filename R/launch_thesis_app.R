#' Launch the thesis direction web application
#'
#' Abre una aplicación de Shiny que permite registrar estudiantes, proyectos
#' y reuniones de seguimiento para coordinar los trabajos de grado.
#'
#' La aplicación funciona completamente en memoria: los datos se conservan
#' mientras la sesión de Shiny esté activa y se pueden exportar en formato CSV
#' para su respaldo.
#'
#' @return El resultado de [shiny::runApp()], invisiblemente.
#' @examples
#' if (interactive()) {
#'   launch_thesis_app()
#' }
#' @importFrom shiny runApp
#' @export
launch_thesis_app <- function() {
  app_dir <- system.file("thesis_app", package = "datadrivencv")
  if (app_dir == "") {
    stop("No se pudo encontrar la aplicación de trabajos de grado. Reinstale el paquete.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
  invisible(NULL)
}
