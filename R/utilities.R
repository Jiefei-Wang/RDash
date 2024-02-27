collapsibleAwesomeCheckboxGroupInput <- 
function(inputId, label, i, choices = NULL, selected = NULL,  
          status = "primary", width = NULL){
    input <- awesomeCheckboxGroup(inputId, label, choices = choices, 
                                  selected = selected, width = width,
                                  status = status)
    checkboxes <- input[[3]][[2]][[3]][[1]]
    id_btn <- paste0(inputId, "_btn")
    id_div <- paste0(inputId, "_collapsible")
    btn <- actionBttn(id_btn, "More...", color = "primary", size = "sm", 
                      style = "minimal", icon = icon("collapse-up", lib = "glyphicon"))
    collapsible <- div(id = id_div, class = "collapse")
    collapsible$children <- checkboxes[(i+1):length(checkboxes)]
    children <- c(checkboxes[1:i], list(btn), list(collapsible))
    input[[3]][[2]][[3]][[1]] <- children
    script <- sprintf('$(document).ready(function(){
  $("#%s_btn").attr("data-target", "#%s_collapsible").attr("data-toggle", "collapse").css("margin-bottom", "11px");
  $("#%s_collapsible").on("hide.bs.collapse", function(){
    $("#%s_btn").html("<span class=\\\"glyphicon glyphicon-collapse-down\\\"></span> More...");
  });
  $("#%s_collapsible").on("show.bs.collapse", function(){
    $("#%s_btn").html("<span class=\\\"glyphicon glyphicon-collapse-up\\\"></span> Less...");
  });
});', inputId, inputId, inputId, inputId, inputId, inputId)
    tagList(input, tags$script(HTML(script)))
}

hiddenDataElement <- function(id, value){
    hidden(textInput(id, NULL, value))
}
