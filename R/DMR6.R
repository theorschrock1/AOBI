

#' @export
DMR6<-R6::R6Class("DMR6",
                  public=list(
                    query=NULL,
                    initialize=function(datasets=NULL,query=NULL){
                      private$.datasets=assert_class(datasets,classes=c("Datasets","R6"))
                      self$query=query
                    }
                  ),
                  private = list(
                    .datasets=NULL
                  ),
                  active=list(
                    datasets = function(value) {
                      if (missing(value)) {
                        return(private$.datasets$datasets)
                      }
                      stop("datasets is read only")

                    }
                  ))
