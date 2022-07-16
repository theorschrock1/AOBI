
#' @export
MarkShelf = R6Class(
  'MarkShelf',
  inherit=Shelf,
  public = list(
    initialize = function(...) {
      self$super_init(...)
    },
    super_init=import_fn(super_init)
  ),
  private = list(
    .mark_type=NULL,
    .init=FALSE
  ),
  active = list(
    special_state = function(value) {
      if (missing(value)) {
        variables=as.list(self$ids[self$marks%nin%'detail'])
        names(variables)<-self$marks[self$marks%nin%'detail']
        ns<-NS(self$sheetid)

        if(len0(variables)|private$.null_state)
          return(mark_buttons(ns('mark')))
        return(mark_buttons(ns('mark'),variables))

      }
      stop("marks_state is read only")

    },
    mark_type = function(value) {
      if (missing(value)) {
        return( self$check_null( private$.mark_type))
      }
      private$.mark_type=value

    }
  )
)
