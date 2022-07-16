#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' DO NOT REMOVE.
#' @noRd
app_ui <-
  function() {
  tagList(
   main_menu_ui(),
   splitRow(inputId='mainSplit',
            class="main-content",
            gutterSize = 10,
            sizes=c(.15,.85),
   sidebar_left_ui(),
   main_content_ui()
   ))
  }

