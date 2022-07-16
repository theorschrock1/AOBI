#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
    system.file(..., package = "AOBI")
}
#' Load all CSS and JS files in the www directory.
#'
#' Only CSS and JS files should be save in this dir.
#'
#' @noRd
app_dependencies <- function(path) {
    add_resource_path(
        'www', app_sys('app/www')
    )
    js_files <- list.files(path, pattern = "\\.js$")
    css_files <- list.files(path, pattern = "\\.css$")
    htmltools::htmlDependency(name = "AOBI", version = packageVersion("AOBI"),
        src = "app/www", package = "AOBI", script = js_files,
        stylesheet = css_files, all_files = FALSE)
}
