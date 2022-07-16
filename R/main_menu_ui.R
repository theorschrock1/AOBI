#' Renders the main menu dropdowns and icon tool bar

main_menu_ui=function(logo_path=NULL){
 sdf<-ao_nav_fixed_top(
    main_menu_dropdowns_ui(),
    icon_menu_ui(logo_path)
  )
}
#' Render a navbar row in a fixed top nav
#' @noRd
navbar_row=function(...,class="navbar-light bg-light"){
  tags$nav(class=glue("navbar m-0 p-0 {class}"),
           tags$div(class="navbar-nav px-1 m-0",...))
}
#' Wrapper for the apps fixed top nav
#' @noRd
ao_nav_fixed_top=function(...){
  tags$nav(class="fixed-top m-0 p-0",...)
}
