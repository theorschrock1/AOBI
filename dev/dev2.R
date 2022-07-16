self$set_query()$
  set_filters()$
  render_filters()
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
library(ggplot2)
library(sDataTable)
diamonds<-DT(diamonds)
diamonds[,Today:=Sys.Date()]
diamonds[,Yesterday:=Sys.Date()-1]
diamonds[,Now:=Sys.time()]
d<-dynDT(diamonds)
d[,`time difference`:=Today-Yesterday]
d[,`is_cut_premium`:=cut=="Premium"]




saveRDS(Datasets$new(d),"Sheets/datasets.rds")

diamonds<-DT(diamonds)
diamonds[,Today:=Sys.Date()]
diamonds[,Yesterday:=Sys.Date()-1]
diamonds[,Now:=Sys.time()]
dt<-dynDT(diamonds)
dt[,`time difference`:=Today-Yesterday]
dt[,`is_cut_premium`:=cut=="Premium"]
dt[,`Letters`:="a"]
self=Sheet$new(name='Sheet1',datasets=list(diamonds=dt))
self$set_state("marks",list(ids=c("one",'two')))
self$shelf$marks$ids
self$get_state("marks")
dt$get_variable_id('depth')
updateSortableDiv(inputId = "column_shelf", content = col_state,session=session)

function(..., inputId,content=NULL,order = NULL,append=NULL,clear=FALSE,session = getDefaultReactiveDomain()) {

  options=list(...)
  assert_subset(names(options),choices=fn_fmls_names(assert_sortable_options))
  assert_sortable_options(...)
  assert_logical(clear)
  assert_any(,
             check_class(classes="shiny.tag",null.ok = TRUE),
           )
 drop_nulls(content)[[1]]
  check_list(drop_nulls(content),types='shiny.tag')
  assert_character(order,null.ok = TRUE)
  assert_any(append,
             check_class(classes="shiny.tag",null.ok = TRUE),
             check_list(types='shiny.tag'))


  if(nnull(append)){
    if(!all(sapply(append,tagHasAttribute,attr='data-id')))
      g_stop('All elements in a sortable div must have a `data-id` attribute.')
    if(is(append,'shiny.tag.list'))append=lapply(append,as.character) %>% unlist()
    append=as.character(append)%sep%""


  }
  if(nnull(content)){
    if(!all(sapply(content,tagHasAttribute,attr='data-id')))
      g_stop('All elements in a sortable div must have a `data-id` attribute.')
    if(is.list(content)&&!is(content,'shiny.tag'))content=lapply(content,as.character) %>% unlist()
    content=as.character(content) %sep%""

  }
  if(clear){
    content=""
  }
  print(content)
  message <- drop_nulls(
    list(
      order =  order,
      options=options,
      append=append,
      content=content
    )
  )
  session$sendInputMessage(inputId, message)
}




pill=html2R(glue("\n                  \n                <div id=\"var_4ZZD2nfIio16123102988040\" data-name=\"depth\" data-action_date_format=\"false\" data-action_delete=\"false\" data-action_edit=\"false\" data-action_label_format=\"false\" data-action_number_format=\"false\" data-action_order_props=\"false\" data-action_shape_props=\"false\" data-action_submenu_aggregation=\"true\" data-conversion=\"NA\" data-conversion_opts=\"NA\" data-hierachy_n=\"NA\" data-option_aggregation=\"SUM\" data-id=\"{dt$get_variable_id('depth')}\" class=\"d-flex flex-row pill-item pill-measure order-2 align-items-center\" draggable=\"false\" style=\"\">\n      <div class=\"pill-data-icon flex-nowrap\">\n        <span class=\"data-calculation\">=</span>\n        <span class=\"mdi mdi-numeric\"></span>\n      </div>\n      <div class=\"pill-area d-flex flex-fill\">\n        <div class=\"modify-hierachy expand\">\n          <span class=\"mdi mdi-plus-box-outline\"></span>\n        </div>\n        <div class=\"modify-hierachy collapse\">\n          <span class=\"mdi mdi-minus-box-outline\"></span>\n        </div>\n        <div class=\"pill-label flex-fill\" data-id=\"{dt$get_variable_id('depth')}\">SUM(depth)</div>\n        <div>\n          <span class=\"mdi mdi-menu-down icon-right dropdown-handle float-right\"></span>\n        </div>\n      </div>\n    </div>")) %>% eval()
