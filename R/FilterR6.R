#' @export
Filters=sUtils::R6manager('filters',"Filter")

Filters$set("active", "filter_states", function(value){
  if(missing(value)){
   return( lapply(private$.filters,function(x)x$filter_state))
  }
  stop("filter_states is read only")
})

#' @export
FilterShelf = R6Class(
  'FilterShelf',
  inherit=Shelf,
  public = list(
    initialize = function(filters,...){
      self$super_init(...)
      private$.filters=filters
    },
    super_init=import_fn(super_init),
    create_filters=function(){
      tmp=list()
     add2list= function(x,y,name){
       x[[name]]<-y
       x
     }

      mp= self$pill_ids
      if(is.null(mp))
        return()
      tmp=lapply(mp,function(x)tmp[[x]]<-list())
      names(tmp)<-mp
      ftypes=as.list(chr_approx(c( "dimension","measure"),c("exact","range"))(self$types))
      tmp=map2(tmp,self$formula,add2list,name='var') %>%
          map2(ftypes,add2list,name='type')%>%
          map2(FALSE,add2list,name='exclude')

    }
  ),
  private = list(
    .init=FALSE,
    .filters=NULL
  ),
  active = list(
    pre_filter_data = function(value) {
      if (missing(value)) {
        sub=self$is_aggregate ==FALSE
        return(self$get_pill_data_items("id",'conversion'))
      }
      stop("pre_filter_data is read only")

    },
    filters= function(value) {
      if (missing(value)) {
        return(private$.filters$filters)
      }
      stop("filters is read only")

    }
  )
)

#' @export
Filter = R6Class(
  'Filter',
  public = list(
    sheetId=NULL,
    pill_id=NULL,
    is_agg=NULL,
    initialize = function(pill_id,range,sheetId,name,value=NULL,is_agg=NULL) {
      self$pill_id<-pill_id
      self$range=range
      self$sheetId=sheetId
      self$name=name
      self$value=value
      self$is_agg=is_agg
    },
    insert_ui=function(selector,where='afterBegin'){
      if(self$is_ui_rendered)
        return()
      ui<-self$ui

      if(nnull(get_session())){
        insertUI(selector = selector,where=where,ui=ui,immediate = TRUE)
        self$is_ui_rendered <-TRUE
        }
    },
    update_ui=function(...,value=NULL,range=NULL,name=NULL,session=getDefaultReactiveDomain()){

      range=range%or%self$range
      value=value%or%self$value%or%range
      name=name%or%self$name
      print("updating filter")
      if(self$ui_type=='range'){
        value[which.min(value)]<-ifelse(min(range)>min(value),min(range),min(value))
        value[which.max(value)]<-ifelse(max(range)<max(value),max(range),max(value))

        s_print(value)
        s_print(range)
      out<-update_range_slider_input(
        inputId=self$id,
        label = name,
        lower_val = min(value),
        upper_val = max(value),
        range = range,
        session = session,
        ...)
      return(out)
      }
      if(self$ui_type=='radio_slider'){

        out<- update_radio_slider_input(
                                   session = session,
                                   inputId = self$id,
                                   range=range,
                                   value=value,
                                   label=name)
        return(out)
      }

      update_checkbox_group_input(
        inputId=self$id,
        label = name,
        choices = range,
        selected = value,...)

    }
  ),
  private = list(
    .is_inserted=FALSE,
    .value=NULL,
    .ui_type=NULL,
    .filter_state=NULL,
    .name=NULL,
    .range=NULL,
    .exclude=FALSE
  ),
  active = list(
    exclude=function(value){
      if (missing(value)) {
        return(private$.exclude)
      }
      private$.exclude=assert_logical(value,len=1)
    },
    filter_state = function(value) {
    if (missing(value)) {
      return(list(
        var = self$name,
        range = self$value,
        type = self$type,
        exclude = self$exclude
      ))
    }
    stop('filter_state is read only')

    },
    id = function(value) {
        if (missing(value)) {
            return(self$pill_id)
        }
        stop("id is read only")

    },
    ns = function(value) {
      if (missing(value)) {
        return(NS(self$sheetId))
      }
      stop("ns is read only")

    },
    inputId = function(value) {
      if (missing(value)) {
        return(self$ns(self$pill_id))
      }
      stop("inputId is read only")

    },
    range = function(value) {
      if (missing(value)) {
        return(private$.range(self$name,is_agg=self$is_agg))
      }
      private$.range<-assert_function(value)

    },
    name = function(value) {
      if (missing(value)) {
        return(private$.name)
      }
      if(is.null(private$.name))
        private$.name<-assert_string(value)
      if(private$.name!=value){
        print("filter name!=new_name")
      private$.name<-assert_string(value)
      self$value<-NULL
      self$update_ui()
      }

    },
    ui=function(value){
      if (missing(value)) {

        range=self$range
        value=self$value%or%range
        if(self$ui_type=='range')
          return( range_slider_input(
            inputId=self$inputId,
            label =self$name,
            units_label = NULL,
            range=range,
            lower_val = min(value),
            upper_val = max(value),
            signif = 5,
            class = "w-100"
          ))
        if(self$is_ui_option)
          return(checkbox_group_input(
            inputId=self$inputId,
            label = self$name,
            choices=range,
            type = self$option_ui_type,
            selected = range,
            clear_filter = TRUE,
            as_dropdown = self$is_ui_dropdown,
            search = TRUE,
            options_dropdown = TRUE
          ))

        if(self$ui_option=='radio_slider')
          return(radio_slider_input(
            inputId=self$inputId,
            label = var$name,
            choices=range,
            selected = value,
            variable_id = self$var_id
          ))

      }
      stop("range is read only")
    },
    value = function(value) {
      if (missing(value)) {
        if(is.null(private$.value)){
          return(NULL)
        }
        return(private$.value)
      }
      private$.value<-value

    },
    ui_type = function(value) {
      if (missing(value)) {
        if(is.null(private$.ui_type)){

          if(is.numeric(self$range)){
            return('range')
          }
          if(is.logical(self$range)){
            return('radio')
          }
          if(length(range)<21){
            return('checkbox')
          }
          return('checkbox_dropdown')
        }
        return(private$.ui_type)
      }
      private$.ui_type = assert_choice(value,
                                       c(
                                         "radio",
                                         'radio_slider',
                                         'range',
                                         'checkbox',
                                         'checkbox_dd',
                                         'radio_dd'
                                       ))
    },
    is_ui_option = function(value) {
      if (missing(value)) {
        return(self$ui_type%in%c("radio",
                                 'checkbox',
                                 'checkbox_dd',
                                 'radio_dd'))
      }
      stop("is_ui_option is read only")

    },
    option_ui_type= function(value){
      if (missing(value)) {
        if(self$ui_type%detect%'radio')
          return('radio')
        if(self$ui_type%detect%'checkbox')
          return('checkbox')
      }
      stop("discrere_ui_type is read only")
    },
    is_ui_dropdown = function(value) {
      if (missing(value)) {
        return(self$ui_type%ends_with%'dd')
      }
      stop("is_ui_dropdown is read only")

    },
    type = function(value) {
    if (missing(value)) {
        return(ifelse(self$ui_type=="range","range","exact"))
    }
    stop("type is read only")

},
    is_ui_rendered = function(value) {
    if (missing(value)) {
        return(private$.is_inserted)
    }
    private$.is_inserted<-assert_logical(value,len=1)

}
  )
)
