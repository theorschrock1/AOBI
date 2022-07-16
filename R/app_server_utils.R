#' Shorthand for reactiveValues
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList
#' assert if an object is a reactive expression.
#' 
#' See ?sDevtools::assert_reactive for details
#' 
#' @noRd
assert_reactive <- function (x, output_type = NULL, ..., .m = NULL) 
{
    v_collect()
    assert_string(output_type, null.ok = TRUE)
    res <- is(x, "reactiveExpr")
    dots = enexprs(...)
    if (!isTRUE(res)) {
        g_stop("{.x} must be a reactive expression")
    }
    if (nnull(output_type)) {
        assert <- eval(expr_glue("expr(check_{output_type}(x(),!!!dots))")[[1]])
        return(reactive({
            message <- eval(assert)
            fn_name <- ""
            if (!is.null(.m)) {
                fn_name <- glue(" for '{.m}'")
            }
            if (!isTRUE(message)) g_stop("invalid reactive input{fn_name}:\n  '{.x}' {message} ")
            invisible(x())
        }))
    }
    return(invisible(reactive({
        x()
    })))
}
