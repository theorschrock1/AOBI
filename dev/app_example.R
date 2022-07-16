
if (interactive()) {
  sDevTools::clear_env_load_all()
  library(sDevTools)
  library(shiny)
  library(AOBI)
  ui <- fluidPage(
    AOBI::add_external_resources(),
    html_dependency_rebootJS(),
    main_menu_ui(),
    splitRow(inputId='mainSplit',
             class="main-content",
             gutterSize = 5,
             minSizes = c(175,500),
             sizes=c(.15,.85),
             snapOffset = 0,
             div(id = "sidebar-left",
                 class = 'bg-white shadow-sm sidebar-left side-bar',
                 data_manager_ui("mod_id")),
             div(id='sheet_content')),
    sheet_dropdown()
  )

  server <- function(input, output, session) {

   sheet= reactive({

     self=Sheet$new(name='Sheet1',datasets=readRDS("Sheets/datasets.rds"))
     #self$set_state_from_file()
    # self$query_info
     return(self)
    })
   dataset=reactive({
     sheet()$datasets[[1]]
    })
    data_manager_server(id="mod_id",dataset)
    observeEvent(sheet(),{
      sht<-sheet()
      id=sht$sheetid

     insertUI("#sheet_content",where='afterBegin',ui=main_content_ui(
       id = id,
       col_pills =  sht$get_state('columns'),
       row_pills =   sht$get_state('rows'),
       mark_pills =  sht$get_state('marks'),
       filter_pills =  sht$get_state('filters'),
       mark_selected = sht$shelf$marks$mark_type
     ),immediate = TRUE)
    main_content_server(id= id,sheet)
    })
    }
  shinyApp(ui, server, options = list(launch.browser=TRUE))
}
Bself$J_filter<-lapply(value,function(x)parse_expr(x$var))

write.csv(diamonds,'diamonds.csv')
