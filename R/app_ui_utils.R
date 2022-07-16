#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
#' @export
add_external_resources <- function() {
    add_resource_path("AOBI", app_sys("app/www"))
    tags$head(bs_theme_dependencies(theme = bs_global_get()),
        app_dependencies(app_sys("app/www")),useShinyjs())
}
#' Add resource path
#'
#' @noRd
add_resource_path <- function(prefix, directoryPath, warn_empty = FALSE) {
    list_f <- length(list.files(path = directoryPath)) == 0
    if (list_f & warn_empty) {
        warning("No resources to add from resource path (directory empty).")
    }
    else {
        addResourcePath(prefix, directoryPath)
    }
}

app_ui_shell <- function(request) {
    fluidPage(add_external_resources())
}
