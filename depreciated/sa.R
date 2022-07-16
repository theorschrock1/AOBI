
mark_selector=function(inputId){
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
  class = 'border-top-0 border-right-0 border-left-0'
)}
mark_buttons=function(id){
  MS<-function(x){
    function(y){
      paste0(x,'_',y)
    }
  }
  ms<-MS(id)
  tagList(h_arrange(
    tags$button(
      id = ms("color"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-palette-outline ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Color")
    ),
    tags$button(
      id = ms("size"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-chart-bubble ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Size")
    ),
    tags$button(
      id = ms("label"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-text-recognition ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Label")
    )
  ),
  h_arrange(
    tags$button(
      id = ms("detail"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-grain ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Detail")
    ),
    tags$button(
      id = ms("tooltip"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-tooltip ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Tooltip")
    ),
    tags$button(
      id = ms("shape"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-shape-outline ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Shape")
    )
  ),html_dependency_dropzone())
}
marks_shelf_sortable_ops=function(group="pill_marks"){
  ns<-session_ns()

  sortable_options(
    name = group,
    pull = TRUE,
    put = c("nav_measures","nav_dimensions"),
    filter='folder-wrapper',
    dragClass = 'drag',
    sort =TRUE,
    removeOnSpill=TRUE,
    onAdd = cglue('function(evt){let elid=$(evt.item);elid.find(".pill-data-icon .mdi").attr("class","mdi mdi-grain");elid.addClass("pill-marks pill-selected");}')
    )
}

div(class="marks-card nav-card",
pill_card(inputId ="mark_sheft",
          type='column',
          name="Marks",
          class='nav-card',
          sortable = FALSE,
          v_arrange(mark_selector("mark_type"),
                    mark_buttons("mark")
          ),
          sortableDiv(
            inputId = "mark_shelf",
            class = glue("d-flex flex-column flex-fill flex-wrap bg-transparent shelf py-1"),
            options = marks_shelf_sortable_ops()))



mark_selector=function(inputId){
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
mark_buttons=function(id){
  MS<-function(x){
    function(y){
      paste0(x,'_',y)
    }
  }
  ms<-MS(id)
  tagList(h_arrange(
    tags$button(
      id = ms("color"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-palette-outline ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Color")
    ),
    tags$button(
      id = ms("size"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-chart-bubble ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Size")
    ),
    tags$button(
      id = ms("label"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-text-recognition ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Label")
    )
  ),
  h_arrange(
    tags$button(
      id = ms("detail"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-grain ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Detail")
    ),
    tags$button(
      id = ms("tooltip"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-tooltip ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Tooltip")
    ),
    tags$button(
      id = ms("shape"),
      class = "ao_btn ao_block-btn ao_block-btn-sm dropzone",
      type = "button",
      tags$span(class = "mdi mdi-shape-outline ao_block-btn_icon"),
      tags$span(class = "block-button-text", "Shape")
    )
  ),html_dependency_dropzone())
}
marks_shelf_sortable_ops=function(group="pill_marks"){
  ns<-session_ns()

  sortable_options(
    name = group,
    pull = TRUE,
    put = c("nav_measures","nav_dimensions"),
    filter='folder-wrapper',
    dragClass = 'drag',
    sort =TRUE,
    removeOnSpill=TRUE,
    onAdd = cglue('function(evt){
    console.log(evt.items);
    let elid=$(evt.item);elid.find(".pill-data-icon .mdi").attr("class","mdi mdi-grain");
    elid.addClass("pill-marks pill-selected");
    let agg=elid.data("option_aggregation");
    let lab=elid.find(".pill-label").get(0);
    let current=$(lab).text();
    $(lab).text("SUM("+current+")");
    }')
  )
}
flexRow(class='w-100',
        div(class='w-50 nav-card',pill_shelf_example()),
        div(class="w-50 marks-card nav-card",
            pill_card(inputId ="mark_sheft",
                      type='column',
                      name="Marks",
                      class='nav-card',
                      sortable = FALSE,
                      v_arrange(mark_selector("mark_type"),
                                mark_buttons("mark")
                      ),
                      sortableDiv(
                        inputId = "mark_shelf",
                        class = glue("d-flex flex-column flex-fill flex-wrap bg-transparent shelf py-1"),
                        options = marks_shelf_sortable_ops()))))
