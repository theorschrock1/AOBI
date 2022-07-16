#Check Usage -----
checks=checkPackageUsage()
runTests(package="AOBI")
#Dismiss Usage Warnings -----
#  suppressUsageWarnings(checks)
###Dev Setup -----
## INSTALL: CTRL + SHIFT + B


sDevTools::clearEnv() ## CTRL + SHIFT + R

sDevTools::loadUtils()
#Dev -----
# Module UI
# Module UI
main_content_ui <- function(id) {
#Documentation
fdoc("main_graph module", "[html]")
ns <- NS(id)
tagList()
}

# Module Server
main_content_server <- function(id,sheet) {
#Documentation
fdoc("main_graph module", "[html]")
#Non-reactive assertions
id <- assert_string(id)
moduleServer(id, function(input, output, session) {
ns <- session$ns
#Reactive assertions
sheet <- assert_reactive(x = sheet, output_type = "R6",
.m = "main_content_server")
#TO DO
#
})
}
fn_document(main_content_server,
{
if (interactive()) {
  library(shiny)
  library(AOBI)
ui <- fluidPage(
    AOBI::add_external_resources(),
    titlePanel("main_content module example"),
    main_content_ui("mod_id")
     )

server <- function(input, output, session) {

  out<-main_content_server(id="mod_id"sheet)

  observe({
  print(out())
  })
}
shinyApp(ui, server)
}
})
fn_document(main_content_ui,rdname="main_content_server")
}}

