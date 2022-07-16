#' Renders icon menu at the top of the application

#' @noRd
icon_menu_ui=function(logo_path=NULL){
  logo=NULL
  if (nnull(logo_path)) {

    logo= tags$a(
      class = 'pt-1',
      href = "#",
      tags$img(
        src = logo_path,
        width = "30",
        height = "18",
        alt = "",
        loading = "lazy"
      )
    )
  }
navbar_row(
           icon_tool_bar(
             icon_button_group(inputId=c("undo","redo","save"),c("arrow-left","arrow-right","floppy"),c("Undo","Redo","Save")),
             icon_button_group(
               c("data_source_new","data_source_refresh","pause_data_updates"),
               c("database-plus","database-refresh","database-lock"),
               c("New data source","Refresh data source","Pause auto updates")),
             icon_button_group(
               c("new_chart","clear_chart","transpose"),
               c("chart-box-plus-outline","tab-remove","rotate-left-variant"),
               c("New Chart","Clear Chart","Swap rows and colums")),
             icon_radio_group(inputId="sort_options",
                              c('sort_remove',
                                'sort_ascending',
                                'sort_descending'),
                              c('sort-variant-remove',
                                'sort-ascending',
                                'sort-descending'),
                              c('Clear sort',
                                'Sort ascending',
                                'Sort descending'))
           ))
}
