s_print=function(x){
  g_print('{expr_text( enexpr(x))}:
          {expr_text(x)}')
}
format_pill_input=function(input){

  tmp<-input$data
  input$data<-NULL
  out=lapply(input,unlist)
  out$data=tmp
  out
}
main_content_server <- function(id,sheet) {
  #Documentation

  #Non-reactive assertions
  id <- assert_string(id)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #Reactive assertions
    vals=reactiveValues(rows=0,cols=0,marks=0,filters=0,query_data=0,update_agg_filters=0)
    #filters=reactiveValues(agg=NULL,pre=NULL,agg_update=0)
    sht <- assert_reactive(x = sheet, output_type = "r6",
                             .m = "main_content_server")
    #TO DO
    sheet<-eventReactive(sht(),{
      req(sht)
      sheet<-sht()
      sheet$rv=vals
      sheet

    })
    observeEvent(input$column_shelf,{
      req(sheet())
      trigger(vals$cols)

      g_print("cols:{vals$cols}")
      sheet=sheet()
      state=format_pill_input(input$column_shelf)
      saveRDS(state,"Sheets/columns_state.rds")
      sheet$set_state("columns",  state)$set_query()

      #saveRDS(sheet,"Sheets/current_sheet.rds")


    })

    observeEvent(input$row_shelf,{
      req(sheet())
      trigger(vals$rows)
      g_print("rows:{vals$rows}")
      sheet=sheet()
      state=format_pill_input(input$row_shelf)
      saveRDS(state,"Sheets/rows_state.rds")
      sheet$set_state("rows",  state)$set_query()
      #saveRDS(sheet,"Sheets/current_sheet.rds")

    })
    mark_state<- reactive({
     state=format_pill_input(input$marks_shelf)
     if(len0(state))return(list())
     state$mark_type=input$mark_type
    state
    })
    observeEvent(  mark_state(),{
      req(sheet())
      sheet=sheet()
      #print(mark_state())
      saveRDS(mark_state(),"Sheets/marsk_state.rds")
      sheet$set_state("marks", mark_state())$set_query()
      #saveRDS(sheet,"Sheets/current_sheet.rds")

    })

    observeEvent(input$filter_shelf,{
      #print("here")
      req(sheet())
      sheet=sheet()
      state=format_pill_input(input$filter_shelf)

      #saveRDS(sheet,"Sheets/current_sheet.rds")
      saveRDS(state,"Sheets/filter_state.rds")
      #print(input$sheetSplit)
      #state=readRDS("Sheets/filter_state.rds")

      if(nnull(state[[1]])){
        sizes=input$sheetSplit
      if(sizes[3]<.05){
      updateSplitDiv('sheetSplit',sizes=c(sizes[1],sizes[2]-.15,.15))
      }
        sheet$set_state("filters",state)$set_filters()$render_filters()

      }else{
        sizes=input$sheetSplit
        if(sizes[3]!=0){
        updateSplitDiv('sheetSplit',sizes=c(sizes[1],sizes[2]+sizes[3],0))
        }
        sheet$set_state("filters",state)$set_filters()
       }
  })
    observeEvent(input$filter_content,{
      req(sheet())
      fc=input$filter_content
      print("filter_value_change")
      write(fc,"Sheets/filter_ui_states.txt")
      filter_vals=jsonlite::fromJSON(fc)
      if(len0(filter_vals))
        return()
      sht=sheet()
      sht$set_filter_values(filter_vals)
     #filters$agg=agg
     #filters$pre=pre

    })

    observeEvent(vals$update_agg_filters,{
      req(sheet())
      print("Filter:agg_update")
      sheet<-sheet()
      sheet$update_filter_uis()
      #trigger(vals$update_table)
    })
#
    table<-eventReactive(vals$query_data,{
      req(sheet())
      print("query_data")
      sheet()$query$dataDT$data
    })
    output$table<-renderTable({
      table<-table()
    }, spacing ="s",na='')
})
}



