
#' @export
Shelf<-R6::R6Class("Shelf",
                   inherit=DMR6,
                   public=list(
                     name=NULL,
                     sheetid=NULL,
                     initialize=function(name,sheetid=NULL,...){               self$super_init(...)
                       self$name=name
                       self$sheetid=sheetid

                     },
                     super_init=import_fn(super_init),
                     set_state=function(x){
                       map2(names(x),x,function(n,x){
                         self[[n]]<-x
                       })
                       return(invisible(NULL))
                     },
                     get_pill_data_items=function(names){
                       lapply(self$data,function(x,name){
                         xout=x[names]
                         lapply(xout,function(x){if(x=="NA")
                           return(NULL)
                           return(X)
                         })
                       },name=names)
                     },
                     get_formula_label=function(data){

                       data
                       var_id <- data$id

                       agg <-data$option_aggregation
                       conv <-data$conversion
                       var= self$datasets[[data$dataset_id]]$vars[[var_id]]
                       name=var$name
                       label=name
                       if(data$parent%detect%'measure'&&agg!='NA'){
                         label=glue('{agg}({name})')
                       }
                       if(data$parent%detect%'dimension'&&conv !='NA'){
                         label=glue('{conv}({name})')
                       }
                       label
                     },

                     update_pill_name=function(pill,data){

                       p=ShinyReboot::node(pill)
                       pill_id<-p$attr('id')
                       var_id <- p$data('id')

                       p$set_data(data[[pill_id]])
                       dataset_id <- p$data('dataset_id')
                       type <-
                         ifelse(hasHTMLclass(pill, "pill-measure"), "measure", "dimension")
                       agg <- p$data('option_aggregation')
                       conv <- p$data('conversion')
                       var= self$datasets[[dataset_id]]$vars[[var_id]]
                       name=var$name

                       p$data('name',name)
                       label=name
                       if(type=='measure'&&agg!='NA'){
                         label=glue('{agg}({name})')

                       }
                       if(type=='dimension'&&conv !='NA'){
                         label=glue('{conv}({name})')
                       }
                       p$find(class="pill-label")$html(label)
                       p$node
                     },
                     check_null=function(value){
                       if(private$.null_state)
                         return(NULL)
                       return(value)
                     }
                   ),
                   private=list(
                     .init=FALSE,
                     .null_state=TRUE,
                     .state=NULL,
                     .ids=NULL,
                     .marks=NULL,
                     .formula=NULL,
                     .types=NULL,
                     .data=NULL
                   ),
                   active=list(
                     ids = function(value) {
                       if (missing(value)) {

                         return(self$check_null( private$.ids))
                       }
                       private$.ids=value

                     },
                     types=function(value) {
                       if (missing(value)) {
                         return( self$check_null(  private$.types))
                       }
                       private$.types=value

                     },
                     marks = function(value) {
                       if (missing(value)) {
                         return(  self$check_null( private$.marks))
                       }
                       private$.marks=value

                     },
                     formula = function(value) {
                       if (missing(value)) {
                         return( self$check_null(    private$.formula))
                       }
                       private$.formula=value

                     },

                     state = function(value) {
                       if (missing(value)) {
                         return(self$check_null( private$.state))
                       }
                       private$.state<-
                         assert_character(value,len=1,null.ok = TRUE)
                     },
                     pill_state = function(value) {
                       if (missing(value)) {
                         if(is.null(private$.state))return(NULL)
                         out<-eval(html2R(self$state))
                         if(is(out,'shiny.tag'))
                           out=tagList(out)
                         out= out[sapply(out,function(x)hasHTMLclass(x,"pill-item"))]
                         out<-lapply(out, self$update_pill_name,self$data)
                         return(out)
                       }
                       private$.state<-
                         assert_character(value,len=1,null.ok = TRUE)

                     },
                     data = function(value) {
                       if (missing(value)) {
                         return(    private$.data)
                       }
                       private$.data=value

                     },
                     is_aggregate = function(value) {
                       if (missing(value)) {
                     out= unlist(lapply( self$data,function(x){x$option_aggregation!="NA"}),use.names=FALSE)
                         return(self$check_null(out))
                       }
                       stop("is_aggregate is read only")

                     },
                     pill_ids = function(value) {
                       if (missing(value)) {
                         return(self$check_null(names(self$data)))
                       }
                       stop("pill_ids is read only")

                     },
                     null_state = function(value) {
                        if (missing(value)) {
                            return( private$.null_state)
                        }
                       private$.null_state=assert_logical(value,len=1)

                    },
                     parsed_formulas=function(value){
                        if(missing_value){
                       return(parse_exprs(self$formula))
                        }
                        stop("parsed formulas is read-only")
                      }

                   )
)
