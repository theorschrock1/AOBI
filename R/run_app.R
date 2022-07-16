#' Run the Shiny Application
#'
#' @inheritParams shiny::shinyApp
#'
#' @export
  run_app <-
    function(onStart = NULL,
             options = list(launch.browser=TRUE),
             enableBookmarking = NULL,
             user = NULL,
             pw = NULL) {
      app_server = rlang::new_function(fn_fmls(app_server), fn_body(app_server))

      shinyApp(
        ui = app_ui_shell,
        server = app_server,
        onStart = onStart,
        options = options,
        enableBookmarking = enableBookmarking
      )
    }

