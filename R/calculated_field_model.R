calculated_field_model=function(inputId,top,left,class=NULL,width=NULL,...){
  MS<-function(x){
    function(y){
      paste0(x,'_',y)
    }
  }
  ms<-MS(inputId)
  draggable_model(
    class = paste("card rounded-0 bg-light p-1",class),
    inputId = inputId,
    top=top,
    left=left,
    width = width,
    div(
      class = "card-header px-2 pb-2 pt-1 bg-light border-0",
      div(
        class = "d-flex align-content-center",
        tags$input(
          type = "text",
          class = "var-btn form-control form-control-sm w-50 mr-auto rounded-0 shadow-none",
          id = ms("var_name"),
          placeholder = "Variable name",
          required = "required"
        ),
        div(class = "d-flex flex-fill drag-handle align-items-center pl-1",
        div(id = ms('var_name_validation_message'),class = "w-100 text-wrap v-none validation-message",'none')),
        draggable_model_btn(
          inputId = ms('close'),
          toggleId = inputId,
          class = "btn mdi mdi-close align-self-center new-var-btn pr-0",
          style = "font-size:16px;",
          outerTag = 'span'
        )
      )
    ),
    div(
      class = "card-body px-2 pt-0 pb-0 mb-0 border-0",
      ace_editor(
        outputId = ms('editor'),
        selectionId = ms('selection'),
        cursorId = ms('cursor'),
        ...
      ),
      div(
        id = ms('editor_validation_message'),
        class = 'w-100 text-wrap v-none validation-message',"none"),
      div(
        class = "d-flex mt-2",
      as_shiny_button(tags$button(id= ms("ok"),class = "btn btn-outline-dark btn-sm rounded-0 var-btn mr-1",
                                    'OK')),
      as_shiny_button(tags$button(id=ms("apply"),class = "btn btn-outline-dark btn-sm rounded-0 var-btn mr-auto",
                    "Apply")),
      draggable_model_btn(
      inputId = ms("cancel"),
      toggleId = inputId,
      class = "btn btn-outline-dark btn-sm rounded-0 var-btn",
      "Cancel"
      )
      )
    )
  )

}
invoke_ace_drop_events=function(id,session=getDefaultReactiveDomain()){
  observeEvent(session$input[[id]],{

    session$sendCustomMessage('invoke_ace_drop',message='')

  },once=TRUE)
}
format_editor_value=function(value){
value<-str_split(value, '\r\n|\n') %>%
  unlist()
if(l(value)>1)value=c("{",value,"}")%sep%"\n"
value
}
show_calculated_field_model=function(inputId,name=NULL,value=NULL,session=getDefaultReactiveDomain(),env=caller_env()){
  updateTextInput(session,inputId = glue('{inputId}_var_name'),value=name)
  updateAceEditor(session,editorId =glue('{inputId}_editor'),value=value)
  show_draggable_model(inputId)
  trigger(vals$set_tokens,env=env)
}
hide_calculated_field_model=function(inputId,name="",value="",session=getDefaultReactiveDomain(),env=caller_env()){
  hide_draggable_model(inputId)
  updateAceEditor(session,editorId=glue('{inputId}_editor'),value='')
  updateTextInput(session,inputId=glue('{inputId}_var_name'),value='')
  trigger(vals$set_tokens,env=env)
}

tmp_name=function(){
  'ksdfl432ljd3234fssv2j'
}


update_pill_shelf=function(env=caller_env()){
  trigger(vals$update_pill_shelf,env=env)
}
