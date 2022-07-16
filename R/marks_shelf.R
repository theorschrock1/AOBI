
mark_selector=function(inputId,selected=  "automatic" ){
  select_picker(
    inputId,
    selected = "automatic",
    choices =c("automatic",
               "chart_bar",
               "chart_bell_curve",
               "chart_bubble",
               "chart_text",
               "chart_donut",
               "chart_gantt",
               "chart_line",
               "chart_pie",
               "chart-areaspline-variant",
               "chart_tree"),
    icons=c("chart-box-outline",
            'signal-cellular-outline',
            "square-outline",
            "checkbox-blank-circle-outline",
            "shape-plus",
            "format-text",
            "chart-gantt",
            "chart-line-variant",
            "chart-pie",
            "chart-areaspline-variant",
            "chart-tree"
    ),
    display_content = c("Automatic",
                        "Bar",
                        "Square",
                        "Circle",
                        "Shape",
                        'Text',
                        "Gantt",
                        "Line",
                        "Pie",
                        "Area",
                        "Tree"),
    width = '100%',
    class = 'border-block'
  )}
mark_buttons=function(id,variables=list()){
  MS<-function(x){
    function(y){
      paste0(x,'_',y)
    }
  }
  if(is.null(variables))
    variables=list()
  if( nlen0(variables))
    assert_named_list(variables,structure = list(color=string(NULL),size=string(NULL),label=string(NULL),tooltip=string(NULL),shape=string(NULL)))
  ms<-MS(id)
  out=v_arrange(`data-id`="buttons",class='mark-buttons pb-2',
                h_arrange(
                  tags$button(
                    id = ms("color"),
                    `data-mark`="color",
                    `data-variable`=variables$color,
                    class = "ao_btn ao_block-btn ao_block-btn-sm variable-dropzone",
                    type = "button",
                    tags$span(class = "mdi mdi-palette-outline ao_block-btn_icon"),
                    tags$span(class = "block-button-text", "Color")
                  ),
                  tags$button(
                    id = ms("size"),
                    `data-mark`="size",
                    `data-variable`=variables$size,
                    class = "ao_btn ao_block-btn ao_block-btn-sm variable-dropzone",
                    type = "button",
                    tags$span(class = "mdi mdi-chart-bubble ao_block-btn_icon"),
                    tags$span(class = "block-button-text", "Size")
                  ),
                  tags$button(
                    id = ms("label"),
                    `data-mark`="label",
                    `data-variable`=variables$label,
                    class = "ao_btn ao_block-btn ao_block-btn-sm variable-dropzone",
                    type = "button",
                    tags$span(class = "mdi mdi-text-recognition ao_block-btn_icon"),
                    tags$span(class = "block-button-text", "Label")
                  )
                ),
                h_arrange(
                  tags$button(
                    id = ms("detail"),
                    `data-mark`="detail",
                    class = "ao_btn ao_block-btn ao_block-btn-sm variable-dropzone",
                    type = "button",
                    tags$span(class = "mdi mdi-grain ao_block-btn_icon"),
                    tags$span(class = "block-button-text", "Detail")
                  ),
                  tags$button(
                    id = ms("tooltip"),
                    `data-mark`="tooltip",
                    `data-variable`=variables$tooltip,
                    class = "ao_btn ao_block-btn ao_block-btn-sm variable-dropzone",
                    type = "button",
                    tags$span(class = "mdi mdi-tooltip ao_block-btn_icon"),
                    tags$span(class = "block-button-text", "Tooltip")
                  ),
                  tags$button(
                    id = ms("shape"),
                    `data-mark`="shape",
                    `data-variable`=variables$shape,
                    class = "ao_btn ao_block-btn ao_block-btn-sm variable-dropzone",
                    type = "button",
                    tags$span(class = "mdi mdi-shape-outline ao_block-btn_icon"),
                    tags$span(class = "block-button-text", "Shape")
                  )
                ))
  out%>%
    attachDependencies(html_dependency_variable_dropzone())
}
marks_shelf_sortable_ops=function(group="pill_marks"){
  ns<-session_ns()

  sortable_options(
    name = group,
    pull = TRUE,
    put = c("nav_measures","nav_dimensions"),
    dragClass = 'drag',
    sort =TRUE,
    removeOnSpill=TRUE,
    emptyInsertThreshold=5,
    customGetValueFn=getPillValues(),
    onSpill='function(evt){

        var $el=$(evt.item);
        var mark_id=$el.data("mark");
       var mb= $(".mark-buttons").find(`[data-mark="${mark_id}"]`).get(0);
        $(mb).data("variable",null);
        $(mb).trigger("variableChange");
    }',
    onUnchoose='function(evt){
    let elid=$(evt.item);


    if(elid.hasClass("pill-marks")){

       var elDropped=evt.originalEvent.path[0];

    if($(elDropped).parent().hasClass("variable-dropzone")){
     var elDropped= $(elDropped).parent().get(0);
    }

    var icon="mdi mdi-grain";
    var mark=elid.data("mark");

    if($(elDropped).hasClass("variable-dropzone")){
      var sort=$(elDropped).parents(".sortable-div").get(0);
      var icon=$(elDropped).find(".mdi").attr("class");
      var mark_id=$(elDropped).attr("id");
      var mark_type=$(elDropped).data("mark");
      if(mark_type!=mark){

      //   console.log("current"+mark);
         var mb= $(".mark-buttons").find(`[data-mark="${mark}"]`).get(0);
        //console.log(mb);
        var current_var=$(elDropped).data("variable");
        $(mb).data("variable",null);
        $(mb).trigger("variableChange");
       data_id=$(elid).data("id");
       $(elDropped).data("variable",data_id);
       $(elDropped).trigger("variableChange");
        if(mark_type!="detail"){
          var current=$(sort).find(`.pill-item[data-id="${current_var}"]`).get(0);
          $(current).remove();
          }
        elid.find(".pill-data-icon .mdi").attr("class",icon);
        elid.attr("data-mark",mark_type);;

      }
    }
        }

   }',
    onAdd= cglue('function(evt){
    var elDropped=evt.originalEvent.path[0];
    if($(elDropped).parent().hasClass("variable-dropzone")){
     var elDropped= $(elDropped).parent().get(0);
    }
    var icon="mdi mdi-grain";
    var mark="detail";
    if($(elDropped).hasClass("variable-dropzone")){
     var sort=$(elDropped).parents(".sortable-div").get(0);
      var icon=$(elDropped).find(".mdi").attr("class");
      var mark_id=$(elDropped).attr("id");
       var mark_type=$(elDropped).data("mark");
        var current_var=$(elDropped).data("variable");
      if(mark_type!=mark){
        data_id=$(evt.item).data("id");
          $(elDropped).data("variable",data_id);

          $(elDropped).trigger("variableChange");
        var current=$(sort).find(`.pill-item[data-id="${current_var}"]`).get(0);
          $(current).remove();

      }
      mark= mark_type;
     }

    parent=[];
    clones=[];
    let fixlab=function(d,i){
    let elid=$(d);
    clones.push(elid.clone());
    if(i===0){
    elid.find(".pill-data-icon .mdi").attr("class",icon);
    elid.attr("data-mark",mark);
    }else{
      elid.find(".pill-data-icon .mdi").attr("class","mdi mdi-grain");
       elid.attr("data-mark","detail");
    }
    if(elid.hasClass("pill-marks")===false){
    elid.addClass("pill-marks pill-selected");

     let agg=elid.data("option_aggregation");
    let conversion=elid.data("conversion");
    let lab=elid.find(".pill-label").get(0);
     var current=$(lab).text();
     elid.data("name",current);
    if(conversion!="NA"){
       current=conversion+"("+current+")";
    }
    if(agg!=="NA"){
     current=agg+"("+current+")";
    }
    $(lab).text(current);
    let idn=  elid.attr("id")+Date.now()+i;
     elid.attr("id",idn);
    }
    parent.push(elid.data("parent"));
    }
    var items=evt.items;
    var indicies=evt.oldIndicies;
    if(items.length==0){
      items[0]=evt.item
      indicies[0]={index:evt.oldIndex};
    }
   items.map(fixlab);


   clones.map(function(d,i){

   if(indicies[i].index===0){
       $(d).removeClass(".pill-selected");
      $("#"+parent[i]).prepend(d);
      return;
   }
    let before= $("#"+parent[i]).children().get(indicies[i].index-1);

    $(d).removeClass("pill-selected");
    $(before).after($(d));
   })
    }')
  )
}
marks_shelf=function(ns,mark_selected= "automatic",pills=NULL){
  if(is.null(pills))
    pills<-mark_buttons(ns("mark"))
div(class="marks-card nav-card",
    pill_card(inputId =ns("marks_shelf"),
              type='column',
              name="Marks",
              class='nav-card',
              sortable = FALSE,
              v_arrange(mark_selector(ns("mark_type"),selected=mark_selected)

              ),
              sortableDiv(
                inputId = ns("marks_shelf"),
                class = glue("d-flex flex-column flex-fill flex-wrap bg-transparent shelf pt-2 pb-2"),
                options = marks_shelf_sortable_ops(),pills)))
}
