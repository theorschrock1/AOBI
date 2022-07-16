#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
    # App authorization
    auth<-authorize_user(id='app',user=user,pw=pw,app_ui=app_ui,logo_path = "www/logo.png",logo_width = 96,logo_height =64 )
    # Your application server logic
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white')
    })
}
