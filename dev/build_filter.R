build_filter=function(filter,pointer='query_formula',use_formula=TRUE){
  if(is.null(filter$range ))
    return( NULL)
  if(use_formula==TRUE){

    assert_named_list(filter,
                      structure = list(
                        var = string(),
                        range = atomic(),
                        type = choice(choices = c('range', 'exact')),
                        exclude = TF(),
                        formula=call()))
    tmp=filter$formula
  }else{
    assert_named_list(filter,
                      structure = list(
                        var = string(),
                        range = atomic(),
                        type = choice(choices = c('range', 'exact')),
                        exclude = TF()))
    tmp=sym(filter$var)
  }



  if(filter$type=='range'){

    out<-expr(!!tmp%inrange%list(!!min(filter$range),!!max(filter$range)))
    out
  }
  if(filter$type=='exact'){
    out<-expr(!!tmp%in%!!parse_expr(expr_text(filter$range)))
  }
  if(filter$exclude)
    out<-expr(!(!!out))
  out
}
I=function(value){
  if (missing(value)) {
    if(is_null(self$J_filter))
      return(missing_arg())
    #pre=self$pre_filter
   # J=self$J_filter
    #by=self$by
    build_query_expr =self$active_data$build_query_expr

    #vnames<-as.list(names(self$J_filter))
    #names(vnames)<-names(self$J_filter)
    active_data=self$active_data
    self$prv$.agg_filter_ranges <- function(x=NULL) {

      query <- build_query_expr(J = self$J_filter,
                                BY = self$by,
                                pre_filter = self$pre_filter) %>%
        insert_data_expr(pre=active_data$pre_data)
      data = active_data$eval_data_expr(query)
      if(is.null(x)){
      vnames<-as.list(names(self$J_filter))
      names(vnames)<-names(self$J_filter)
      return(lapply(vnames, function(x)
        d_range(data[[x]])))
      }
      d_range(data[[x]])
    }
    if(is_missing(self$agg_filter))
      return(NULL)
    tmp<-self$active_data$build_query_expr(J=self$J_filter,BY=self$by,post_filter = self$agg_filter)
    return(insert_data_expr(tmp,pre=sym('.D')))

  }
  stop('I is read only')
},
