
    onAdd=function(evt){
    var elDropped=evt.originalEvent.path[0];
    if($(elDropped).parent().hasClass("dropzone")){
     var elDropped= $(elDropped).parent().get(0);
    }
    var icon="mdi mdi-grain";
    var mark="mark_detail";
    if($(elDropped).hasClass("dropzone")){
      var sort=$(elDropped).parents(".sortable-div").get(0);
      var icon=$(elDropped).find(".mdi").attr("class");
      var id=$(elDropped).attr("id");

      if(id!=mark){
          data_id=$(evt.item).data('id');
          $(elDropped).data("variable",data_id);
          var current=$(sort).find(`.pill-item[data-mark="${id}"]`).get(0);
          $(current).remove();

      }
      mark=id;
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
       elid.attr("data-mark","mark_detail");
    }
    if(elid.hasClass("pill-marks")===false){
    elid.addClass("pill-marks pill-selected");
    let agg=elid.data("option_aggregation");
    let lab=elid.find(".pill-label").get(0);
    let current=$(lab).text();
    $(lab).text("SUM("+current+")");
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
    }
