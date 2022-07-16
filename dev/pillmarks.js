function(evt){
    let elid=$(evt.item);


    if(elid.hasClass("pill-marks")){

    var elDropped=evt.originalEvent.path[0];

    if($(elDropped).parent().hasClass("dropzone")){
     var elDropped= $(elDropped).parent().get(0);
    }

    var icon="mdi mdi-grain";
    var mark=elid.data("mark");

    if($(elDropped).hasClass("dropzone")){
      var sort=$(elDropped).parents(".sortable-div").get(0);
      var icon=$(elDropped).find(".mdi").attr("class");
      var id=$(elDropped).attr("id");

      if(id!=mark){
      if(id!="mark_detail"){
          data_id=$(elid).data('id');
          $(elDropped).data("variable",data_id);
          var current=$(sort).find(`.pill-item[data-mark="${id}"]`).get(0);
          $(current).remove();
          }
      }
      mark=id;
      elid.find(".pill-data-icon .mdi").attr("class",icon);
      elid.attr("data-mark",mark);
    }
        }

   }
