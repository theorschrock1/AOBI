#' @export
reloadSht=function(){
  sDevTools::clear_env_load_all()
  self=Sheet$new(name='Sheet1',datasets=readRDS("Sheets/datasets.rds"))
  self$rv=reactiveValues(rows=0,cols=0,marks=0,filters=0,query_data=0,update_agg_filters=0)
  self$set_state_from_file()
Query$active$by
  x=jsonlite::fromJSON(readLines("Sheets/filter_ui_states.txt"))
  #print("set query")
  state=readRDS("Sheets/filter_state.rds")
  self$set_state('filters',state)
  state=readRDS("Sheets/columns_state.rds")
  self$set_state('columns',state)$set_query()
  state=readRDS("Sheets/columns_state.rds")
  self$set_state('rows',state)

  self$set_query()
  self$set_filters()
  self$render_filters()
  self<<-self
}

#' @export
Sheet<-R6::R6Class('Sheet',
                   inherit=DMR6,
                   public=list(
                       name=NULL,
                       shelf=list(),
                       rv=NULL,
                       current_data=NULL,
                       sheetid=NULL,
                       update_pills=NULL,
                     initialize=function(...,name,dataset_name=NULL,sheet_id=NULL){

                       self$super_init(...)
                       self$name=assert_string(name)

                       self$current_data<-
                         dataset_name%or%self$datasets[[1]]$dataset_id
                       self$sheetid<-sheet_id%or%paste0("sheet_",create_unique_id(10))
                        dots=list(...)
                       self$query=Query$new(datasets=dots$datasets)
                       private$.filters<-Filters$new()

                       self$shelf$rows <-
                         Shelf$new(
                           name = 'rows',
                           sheetid = self$sheetid,
                           query=self$query,
                           ...
                         )
                       self$shelf$columns <-
                         Shelf$new(
                           name = 'colums',
                           sheetid = self$sheetid,
                           query=self$query,
                           ...
                         )
                       self$shelf$marks <-
                         MarkShelf$new(
                           name = 'marks',
                           sheetid = self$sheetid,
                           query=self$query,
                           ...
                         )

                       self$shelf$filters <-
                         FilterShelf$new(
                           name = 'filters',
                           sheetid = self$sheetid,
                           filters=private$.filters,
                           query=self$query,
                           ...
                         )



                     },
                     super_init=import_fn(super_init),
                     get_state=function(name){
                special = drop_nulls(list(self$shelf[[name]]$special_state))%or%list()
                pills<-drop_nulls(self$shelf[[name]]$pill_state)%or%list()
                      out=c(special,pills)%or%NULL
                       return(out)
                     },
                     set_state=function(name,state){

                       self$shelf[[name]]$null_state=is_empty(drop_nulls(state))
                       self$shelf[[name]]$set_state(state)
                       return(invisible(self))
                     },
                     set_query=function(){
                      qi= self$query_info
                      if(is_empty( qi))
                        return(invisible(self))
                     dims=parse_exprs( unique(qi[type=="dimension"&mark!="filters"]$formula))
                     measures=parse_exprs( unique(qi[type=="measure"&mark!="filters"]$formula))
                     current_length=length(self$query$by)
                     dim_length=length(dims)
                    self$query$by=dims
                    self$query$J=measures
                    if( current_length!=dim_length)
                      self$trigger_event("update_agg_filters")
                    self$trigger_event("query_data")
                    return(invisible(self))
                      },

                     set_filters=function(){
                        qi=self$query_info

                        if(is_empty( qi)){
                          self$remove_all_filters()
                          return(invisible(self))
                        }
                        qi=qi[mark=="filters"]
                        if(is_empty( qi)){
                          self$remove_all_filters()
                          return(invisible(self))
                        }
                        self$query$pre_filter  <-
                          self$format_filter_data(qi[is_agg==FALSE])
                        self$query$agg_filter  <-
                          self$format_filter_data(qi[is_agg==TRUE])
                        current_filts<-qi[qi$pill_id%in%names(self$filters)]
                        map(seq_along(current_filts$pill_id),function(i){
                          tmp=current_filts[i]
                          tmpfilt=self$filters[[tmp$pill_id]]
                          tmpfilt$is_agg<-tmp$is_agg
                          tmpfilt$name<-tmp$formula

                        })
                        new_filts<-qi[qi$pill_id%nin%names(self$filters)]
                        if(nrow( new_filts)>0){
                        lapply(seq_along(new_filts$pill_id),function(i){
                              tmp<- new_filts[i]

                              self$create_filter(
                                pill_id = tmp$pill_id,
                                range = self$ranges,
                                sheetId = self$sheetid,
                                name = tmp$formula,
                                is_agg=tmp$is_agg
                              )
                              return(invisible(NULL))
                            })
                        }
                        delete_filts<-names(self$filters)%NIN%qi$pill_id
                        s_print(delete_filts)
                        self$delete_filters(delete_filts)
                        return(invisible(self))
                      },
                     format_filter_data=function(filts){
                       filts[, row := 1:nrow(filts)]
                       out = lapply(split(filts[, .(var = formula,
                                                    type = filter_type,
                                                    exclude = exclude,
                                                    row)], by = 'row', keep.by = F), function(x) {
                                                      out = as.list(x)
                                                      c(out, list(range = NULL))
                                                    })
                       names(out) <- NULL
                       out
                        },
                     set_state_from_file=function(){
                      self$set_state('columns',readRDS("Sheets/columns_state.rds"))
                      self$set_state('rows',readRDS("Sheets/rows_state.rds"))
                      self$set_state('marks',readRDS("Sheets/marsk_state.rds"))
                      self$set_state('filters',readRDS("Sheets/filter_state.rds"))
                      },
                     delete_filters=function(ids,session=get_session()){

                lapply(ids,function(x){
                  private$.filters$delete_filter(x)
                  if(nnull(session)){
                    ns<-NS(self$sheetid)
                    removeUI(selector=glue('#{ns(x)}'),immediate = TRUE)
                  }}
                  )
                  return(invisible(NULL))
                },
                     create_filter=function(...){

                  private$.filters$new_filter(Filter$new(...))
                  return(invisible(self))
                },
                     ranges=function(formula=NULL,is_agg){
                       if(is_agg)
                          return(self$query$agg_filter_ranges(formula))
                       self$query$pre_filter_ranges(formula)
                     },
                     render_filters=function(selector=NULL,where="afterBegin",pill_ids=NULL){
                  filts<-self$filters
                  if(nnull(pill_ids))
                    filts<-filts[pill_ids]
                  if(is_empty( filts))
                    return(invisible(self))
                  if(is.null(selector))
                    selector=paste0("#",self$ns('filter_content'))

                lapply( filts,function(x)x$insert_ui(selector=selector,where=where))
                  return(invisible(self))
                     },
                     update_filter_uis=function(agg_only=TRUE){
                       filts<-self$filters
                       if(len0(filts))
                         return(invisible(self))

                       lapply(filts, function(f,agg_only) {
                           if (agg_only&&!f$is_agg){
                             print('skipping non-agg filter')
                             return()
                           }
                           f$update_ui()
                         },agg_only=agg_only)

                       return(invisible(self))
                      },
                     set_filter_values=function(x){
                       filts<-self$filters

                       out<- map2(names(x),x,function(name,value,filters){
                         nm<-last(str_split(name,"-")[[1]])
                        tmp<- filters[[nm]]
                         tmp$value<-value
                         fs=tmp$filter_state

                         return(data.table(state=list(fs),is_agg=tmp$is_agg))
                       },filters=filts) %>% rbindlist()

                      # self$set_query()
                       self$query$pre_filter<-out[is_agg==F]$state
                       self$query$agg_filter<-out[is_agg==T]$state
                       self$current_agg_filter_state=out[is_agg==T]$state
                       self$current_pre_filter_state=out[is_agg==F]$state
                     },
                     remove_all_filters=function(){
                       self$query$pre_filter  <-NULL
                       self$query$agg_filter  <-NULL
                       self$delete_filters(names(self$filters))
                     },
                     trigger_event=function(x){
                       self$rv[[x]]<-self$rv[[x]]+1
                      }

                   ),
                   private=list(
                     .vars=NULL,
                     .datasets=NULL,
                     .dataset_name=NULL,
                     .init=FALSE,
                     .filters=NULL,
                     .last_pre_filter_state=NULL,
                     .last_agg_filter_state =NULL
                   ),
                   active=list(
                     current_pre_filter_state = function(value) {
                        if (missing(value)) {
                            return(private$.last_pre_filter_state)
                        }

                       if(!identical(value,private$.last_pre_filter_state)){
                         self$trigger_event("query_data")
                         self$trigger_event("update_agg_filters")
                       }
                       private$.last_pre_filter_state=value
                     },
                     current_agg_filter_state = function(value) {
                       if (missing(value)) {
                         return(private$.last_agg_filter_state)
                       }
                       if(!identical(value,private$.last_agg_filter_state)){
                        self$trigger_event("query_data")
                     }
                     private$.last_agg_filter_state= value
                     },
                      ns = function(value) {
                         if (missing(value)) {
                           return(NS(self$sheetid))
                         }
                         stop("ns is read only")

                       },
                      query_info = function(value) {
                      if (missing(value)) {

                        sn=names(self$shelf)
                       out= lapply(sn,function(n){

                        tmp=self$shelf[[n]]
                       as.data.table(drop_empty(list(formula=tmp$formula,type=tmp$types,mark=rep(n,l(tmp$types)),var_id=tmp$ids,is_agg=tmp$is_aggregate,pill_id=tmp$pill_ids)))
                        }) %>% rbindlist()
                      if(nrow(out)==0)
                        return(out)
                       fn<-chr_approx(c( "dimension","measure"),c("exact","range"))
                       out[,filter_type:=fn(type)]
                       out[,exclude:=FALSE]
                          return(out)
                      }
                      stop("query is read only")

                      },
                      pre_filter_ids = function(value) {
                          if (missing(value)) {
                         return(  self$shelf$filters$ids[ self$shelf$filters$is_aggregate==FALSE])
                          }
                          stop("pre_filter_ids is read only")

                      },
                      post_filter_ids = function(value) {
                    if (missing(value)) {
                      return(  self$shelf$filters$ids[ self$shelf$filters$is_aggregate])
                    }
                    stop("pre_filter_ids is read only")

                  },
                      datasets = function(value) {
                      if (missing(value)) {
                          return(private$.datasets$datasets)
                      }
                      stop("datasets is read only")

                      },
                      filters=function(value) {
                    if (missing(value)) {
                      return(private$.filters$filters)
                    }
                    stop("filters is read only")

                      },
                      filter_states=function(value){
                         private$.filters$filter_states
                      }
                   ))



