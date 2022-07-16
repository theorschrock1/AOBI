if (missing(value)) {
  if(is_null(self$prv$.current_filter)){
    return(missing_arg())
  }
  return(self$prv$.current_filter)
}
tmp=lapply(value,function(x)parse_expr(x$var))
forms=self$active_data$parse_DT_list( tmp)
value=map2(value,forms,function(x,i){x$formula=i
return(x)})
active_data<-self$active_data
self$prv$.pre_filter_ranges=function(){
  jnames=as.list(names( forms))
  names(jnames)<-jnames
  query<-active_data$build_query_expr(J=forms)
  query[[2]]<-active_data$pre_data
  data=active_data$eval_data_expr(query)
  lapply(jnames,function(x)d_range(data[[x]]))
}
ranges=self$prv$.pre_filter_ranges()
value=map2(value,ranges,function(x,i){
  if(is.null(x$range))
    x$range=i
  return(x)
})

self$prv$.current_filter=lapply(value,build_filter) %>% reduce(function(x,y)expr(!!x&!!y))
