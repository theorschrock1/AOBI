data_manager_ui <- function(id) {
  #Documentation


  fdoc("Data manager module", "[invisible(NULL)]")
  ns <- NS(id)
  calc_field_model_id = ns('calc_field')

  tagList(
    div(class = 'sidebar-heading px-3 border-top', 'Data'),
    div(
      select_picker(
        ns("dataset"),
        selected = "diamonds",
        choices = c('diamonds', 'mtcars', 'iris'),
        width = '100%',
        class = 'border-top-0 border-right-0 border-left-0'
      ),
      class = "mx-2"
    ),
    h_arrange(class='p-1',
              icon_btn(
                inputId = ns("new_variable"),
                icon = 'pencil-plus-outline',
                toggleId = calc_field_model_id,
                tooltip = 'New variable'
              ),
              icon_btn(
                inputId = ns('search_table'),
                "table-search",
                tooltip = 'Search variables'
              ),
              icon_btn(
                inputId = ns("view_data"),
                icon = "view-comfy",
                tooltip = 'View data'
              )
    ),
    calculated_field_model(
      inputId = calc_field_model_id,
      top = 35,
      left = 20,
      width = '450px',
      height = '175px',
      value = '',
      autoComplete = "live",
      autoCompleters='static',
      mode = 'r',
      theme = 'crimson editor',
      highlightActiveLine = FALSE,
      showLineNumbers = FALSE,
      debounce=1000,
      css_dependency=NULL
    ),
    div(class = 'sidebar-heading px-3 border-top', 'Dimensions'),
    splitCol(
      inputId = ns('data_variables'),
      class = 'h-100',
      gutterSize = 20,
      sizes = c(.4, .4, .2),
      div(class = 'nav-card side-bar overflow-auto px-2', uiOutput(ns('dimensions_output'))),
      div(class = 'nav-card side-bar overflow-auto px-2', uiOutput(ns('measures_output'))),
      div(class = 'nav-card side-bar overflow-auto px-2', uiOutput(ns('parameters_output'))),
      gutterAttrs = list(
        gutterTag = 'div',
        class = 'sidebar-heading px-3 border-top',
        innerHtml = c('Measures', "Parameters")
      )
    ),
    variable_dropdown(ns('variable_dropdown')),
    add_editable_class(inputId=ns('rename_variable'),'.pill-label'),
    uiOutput(ns("hi"))
  )
}

# Module Server
data_manager_server <- function(id,dataset) {
  #Documentation
  fdoc("Data manager module", "[invisible(NULL)]")
  #Non-reactive assertions
  id <- assert_string(id)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #Reactive assertions
    dat<- assert_reactive(x = dataset, output_type = "r6",classes='dynDataTable',
                          .m = "data_manager_server")
    #TO DO

    vals=reactiveValues(valid_formula=FALSE,set_tokens=NULL,invoke=0)
    output$measures_output=renderUI({



      c(ids,data)%<-%dat()$measure_pills
      #dvars<-dat()$measure_vars
      out<-pill_card(inputId =ns("measures"),
                             type='column',
                            `data-folder_id`='measures',
                             sort_ops = pill_shelf_sortable_ops("nav_measures"),
                             pill_shelf(ids,data,group_name = "nav_measures"))
      #print(findDependencies(out))
      #print(findDependencies(out))
      out

    })
    output$dimensions_output=renderUI({
      # di=DT(ggplot2::diamonds)
      # di[,date:=Sys.Date()]
      # self<-dynDT(di)

      c(ids,data)%<-%dat()$dimension_pills
      #dvars<-dat()$dimension_vars
      out<-pill_card(inputId =ns("dimensions"),
                     type='column',
                     `data-folder_id`='dimensions',
                     sort_ops =  pill_shelf_sortable_ops("nav_measures"),
                     pill_shelf(ids,data,group_name = "nav_dimensions"))
      #print(findDependencies(out))



    })
    observeEvent(vals$set_tokens,{
     req(vals$set_tokens)
     dems=str_as_object_name(dat()$dimension_vars$name)
     nums=str_as_object_name(dat()$measure_vars$name)
     funcs=getSDTfns()
     comps=list(functions=funcs)
     tokens = list(
       create_ace_token('function', paste0(funcs, followed_by('\\(')), escape =
                          FALSE),
       create_ace_token(
         'assignment',
         '(^|\n)\\s*([A-z][A-Za-z0-9]+|`.*?`)(?==[^=])',
         escape = FALSE
       )
     )
     if(!is_empty(nums)){
       comps$measures<-nums
       append(tokens)<-
       create_ace_token('measure',
                        nums,
                        escape = TRUE)
     }
     if(!is_empty(dems)){
       comps$dimensions<- dems
       append(tokens)<-
        create_ace_token('dimension',
                        dems,
                        escape = TRUE)
     }
     updateAceEditor(session,"calc_field_editor",
                              autoCompleters = c("static"),
                              autoCompleteList = comps
     )


    # print(tokens)
    set_ace_tokens(inputId='calc_field_editor',tokens=tokens)
    })
    observeEvent(input$calc_field_editor,{
     vals$valid_formula=FALSE

     if(is_empty(input$calc_field_editor))
       return()
     value=format_editor_value(input$calc_field_editor)
     #print(value)
     clone<- dat()$clone()
     #print(a)
     res<-clone$new_variable(name=tmp_name(),
                      I=NULL,
                      J=value,
                      by=NULL,
                      exclude_LOD=NULL,
                      include_LOD=NULL,
                      from_source = F,
                      replace_old=TRUE)

     g_print('clonemessage:{res}')
     send_validation_message(id='calc_field_editor',
                             message=res)
     if(is_error(res))
        return()
     send_validation_message(id = 'calc_field_editor',
                             message = as_valid_message('This calculation is valid'))
     vals$valid_formula=TRUE

    })
    observeEvent(input$new_variable,{
      vals$set_tokens=vals$set_tokens%or%0+1
    })

    invoke_ace_drop_events('calc_field_editor')

    observeEvent(input$variable_dropdown_action_edit,{
     edit_var=input$variable_dropdown_action_edit
     c(name,value)%<-%dat()$get_formula_edit(edit_var)
     show_calculated_field_model('calc_field',name=name,value=value)
    })
    observeEvent(input$variable_dropdown_action_rename,{
      var_id=input$variable_dropdown_action_rename
      update_editable('rename_variable',data_id=var_id,trigger_edit = TRUE)

    })

    observeEvent(input$variable_dropdown_action_create_calc_field,{

    value<-dat()$get_variable_name(input$variable_dropdown_action_create_calc_field)
    show_calculated_field_model('calc_field',name="",value=value)

    })
    observeEvent(input$variable_dropdown_action_duplicate,{
      dat=dat()
     id= dat$duplicate_variable(input$variable_dropdown_action_duplicate)
     dat$insert_new_pill(id,session)
     #update_pill_shelf()
    })
    observeEvent(vals$update_pill_shelf,{
      c(ids,data)%<-%dat()$dimension_pills
      #ord<-order(dvars$name)
      items<-pill_shelf(ids,data,group_name = "nav_dimensions")
      #g_print("dimensions:`{dvars$name%sep%','}`")
      updateSortableDiv(inputId="dimensions",content=items)

      c(ids,data)%<-%dat()$measure_pills

      #g_print("measures:`{dvars$name%sep%','}`")
      items<-pill_shelf(ids,data,group_name = "nav_measures")
      updateSortableDiv(inputId="measures",content=items,session=session)
    })
    observeEvent(input$calc_field_editor_drop_event,{
      print(input$calc_field_editor_drop_event)

      ace_clear_selection('calc_field_editor')
    })
    observeEvent(input$calc_field_var_name,{
      value=input$calc_field_var_name

      if(value=='')return()
      send_validation_message(id='calc_field_var_name',message=TRUE)
    })
    observeEvent(input$rename_variable,{
    c(id,new_name)%<-%input$rename_variable
g_print('renameid: {id} new_name: {new_name}')
   # dat()$rename_object_by_id(id,new_name)
    })
    observeEvent(input$variable_dropdown_action_create_folder,{
      id<-input$variable_dropdown_action_create_folder
      folder_id<-dat()$add_variable_to_new_folder(input$variable_dropdown_action_create_folder)
      dat()$insert_new_pill(folder_id,session)
      sortable_remove('measures',data_id=id)
      #observeEvent(input[[folder_id]],{
      update_editable('rename_variable',data_id =folder_id,trigger_edit = TRUE)
        #},once=TRUE)
    })
    observeEvent(vals$valid_formula,{
      if(vals$valid_formula){
        removeClass('calc_field_ok','disabled')
      }else{
        addClass('calc_field_ok','disabled')
      }
    })
    observeEvent(input$calc_field_ok,{
      value=input$calc_field_editor

      res = if (!is_empty(value))
        TRUE
      else
        as_error_message("Formula can't be empty")

      send_validation_message(id='calc_field_var_name',message=res)

      if(is_error(res))
        return()
      req(vals$valid_formula)

      res=TRUE
     if(input$calc_field_var_name=='')
        res=as_error_message("Missing variable name")

      send_validation_message(id='calc_field_var_name',message=res)
      if(!isTRUE(res))return()

      dat=dat()

      value=format_editor_value(value)
      name<-input$calc_field_var_name
      res<-dat$new_variable(name=name,
                              I=NULL,
                              J=value,
                              by=NULL,
                              exclude_LOD=NULL,
                              include_LOD=NULL,
                              from_source = F,
                              replace_old=TRUE)

      send_validation_message(id='calc_field_editor',
                              message=res)
      if(is_error(res)){
        g_print('data_message:{res}')
        return()
      }
     # update_pill_shelf()
      dat()$insert_new_pill(res,session)
      hide_calculated_field_model('calc_field')
    })
    observeEvent(input$pill_shelf_item_put,{

      c(var_id,folder_id)%<-%input$pill_shelf_item_put
      print(input$pill_shelf_item_put)
      dat<-dat()
      dat$add_variable_to_folder(var_id,folder_id)
      dat$session_sort_folder(folder_id)

    })
    observeEvent(input$view_data,{
      # c(var_id,folder_id)%<-%input$pill_shelf_item_put
      # print(input$pill_shelf_item_put)
      # dat<-dat()
      # dat$add_variable_to_folder(var_id,folder_id)
      # dat$session_sort_folder(folder_id)
    })

  })
}

as_valid_message=function(x){
  class(x)<-c('valid_message',class(x))
  x
}
trigger<-function(x,env=caller_env()){
  x=enexpr(x)
 eval(expr({!!x<-!!x%or%0+1}),envir = env)
}

