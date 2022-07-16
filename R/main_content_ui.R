data_content_ui=function(ns,filter_pills=NULL,mark_pills=NULL,mark_selected="automatic"){
  flexCol(class='mt-2',
    filter_shelf(ns,pills=filter_pills),
    marks_shelf(ns,pills=mark_pills,mark_selected =mark_selected)
  )
}

graph_content_ui=function(ns,col_pills=NULL,row_pills=NULL){
  flexCol(class='my-2',
    row_shelf(ns,pills=col_pills),
    column_shelf(ns,pills=row_pills),
    flexCol(
      class = 'w-100 flex-fill bg-white',
      flexRow(class = 'p-2 w-100  border-bottom', "Sheet 1"),
      flexRow(class = 'flex-fill overflow-hidden',
        uiOutput(ns("table"),class='overflow-auto'))
    ))

}
filter_content_ui=function(ns,filters=NULL){
  div(class='my-2',
          data_filters(inputId=ns('filter_content'),
            class = 'w-100',
          filters))
}
row_shelf=function(ns,pills =NULL){
  div(class="row-card",
      pill_card(inputId =ns("row_shelf"),
                type='row',
                name="Rows",
                sortable = FALSE,
                sortableDiv(
                  inputId = ns("row_shelf"),
                  class = glue("d-flex flex-row flex-fill flex-wrap bg-transparent shelf put-clone"),
                  options = filter_shelf_sortable_ops("rows"),pills )))
}
column_shelf=function(ns,pills=NULL){
      pill_card(inputId =ns("column_shelf"),
                type='row',
                name="Columns",
                sortable = FALSE,
                sortableDiv(
                  inputId = ns("column_shelf"),
                  class = glue("d-flex flex-row flex-fill flex-wrap bg-transparent shelf  put-clone"),
                  options = filter_shelf_sortable_ops('columns'),pills ))
}

main_content_ui=function(id='Sheet1',col_pills=NULL,row_pills=NULL,filter_pills=NULL,mark_pills=NULL,mark_selected="automatic"){
  ns<-NS(id)
  ao_sheet(inputId=ns('sheet'),class='w-100 h-100',
           splitRow(inputId=ns('sheetSplit'),
                    class="main-content",
                    gutterSize = 5,
                    minSizes = c(175,350,0),
                    sizes=c(.20,.80,0),
                    snapOffset = 0,
                    data_content_ui(ns,filter_pills=filter_pills,mark_pills=mark_pills,mark_selected=mark_selected),
                    graph_content_ui(ns,col_pills=col_pills,row_pills=row_pills),
                    filter_content_ui(ns)
                    ))
}
