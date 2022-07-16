#' Send a validation message to html elements with ids:
#'  id
#' '{id}_validation_message'
#' @param id [string] the element id
#' @param message [TRUE|character(message)] if TRUE, removes any message. If character, add shows the message.
#' @param type [choice] c('valid','invalid)

#' @noRd
send_validation_message=function(id,message){
  assert_string(id)
  message_id=paste0(id,'_validation_message')
  editor_id=paste0(id)


  if(is_error(message)|is(message,'valid_message')){
    #print(message)
    message_type =  if (is(message, 'valid_message'))
      'valid-message'
    else
      'invalid-message'
    r_type = if (is(message, 'valid_message'))
      'invalid-message'
    else
      'valid-message'
    code_type=paste0('code_', message_type)

    addClass(id=   editor_id,class=code_type)
    removeClass(id= message_id,
                class=r_type)
    html(id =  message_id, html = message)

    addClass(id= message_id,
             class=message_type)

    removeClass(id= message_id,
                class='v-none')

  }else{
    removeClass(id=  editor_id,class='code_valid-message')
    removeClass(id=  editor_id,class='code_invalid-message')
    addClass(id=  message_id,
             class='v-none')
    removeClass(id=  message_id,
             class='invalid-message')
    removeClass(id=  message_id,
                class='valid-message')

  }
}
