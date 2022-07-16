getPillValues=function(){
  'function(el){

    var out={};
    var pills=$(el).children(".pill-item");


         pills.each(function(){
         var $el=$(this);
        var tmp=$el.data()
        var type="dimension";
        if($el.hasClass("pill-measure")){
            type="measure";
        }
        tmp["type"]=type

        tmp["collapsed"]=$el.hasClass("collapsed");
        out[$el.attr("id")]=tmp
        });

        return out
    }'
}
getPillValues=function(){
  'function(el){

    var out={};
    var types=[];
    var formula=[];
    var marks=[];
    var ids=[];
    var data={};
    var pills=$(el).children(".pill-item");
    if(pills.length===0){
    return   {"types":null,
              "ids":null,
              "marks":null,
              "formula":null,
              "data":null,
              "state":null}
    }
    out.state=$(el).html();
         pills.each(function(){
         var $el=$(this);
        formula.push($($el.find(".pill-label").get(0)).text());
        var type="dimension";
        if($el.hasClass("pill-measure")){
            type="measure";
        }
        types.push(type);
        marks.push($el.attr("data-mark"));
        data[$el.attr("id")]=$el.data();
        ids.push($el.data("id"));
        });
        out.types=types;
        out.ids=ids;
        out.marks=marks;
        out.formula=formula;
        out.data=data;
        return out;
    }'
}
filter_shelf_sortable_ops=function(group="filter"){
  sortable_options(
    name = group,
    pull = TRUE,
    put = c("nav_measures","nav_dimensions"),
    dragClass = 'drag',
    sort =TRUE,
    removeOnSpill=TRUE,
    emptyInsertThreshold=5,
    customGetValueFn=getPillValues(),
    onAdd= cglue('function(evt){

let fixlab=function(d,i){
    let elid=$(d);

    elid.removeClass("pill-selected");
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
     elid.data("mark","&&group&&");
    }

 var clones=evt.items;
 if(clones.length===0){
      fixlab(evt.item,0);
 }else{

  clones.map(fixlab)
  }
   }'
  ))
}
filter_shelf=function(ns,pills=NULL){

  pill_card(inputId =ns("filter_shelf"),
            type='column',
            name="Filter",
            sortable = FALSE,
            sortableDiv(
              inputId = ns("filter_shelf"),
              class = glue("d-flex flex-column flex-fill flex-wrap bg-transparent shelf p-1 put-clone"),
              options = filter_shelf_sortable_ops(),pills ))
}

