#' render the left skicky sidebar
#' @noRd
sidebar_left_ui=function(){
  div(id = "sidebar-left",
               class = 'bg-white shadow-sm sidebar-left',
                data_mananer_ui("project_data")
      )
  }
