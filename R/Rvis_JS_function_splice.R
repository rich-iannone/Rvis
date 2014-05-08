Rvis_JS_function_splice <- function(sets){
 
  # Require statements
  require(stringr)
  
  # test
  JS_function <-
    "
    svg.append(%g%)
        .attr(%class%, %y axis%)
        .call(~yAxis~)
      .append(%text%)
        .attr(%transform%, %rotate(-90)%)
        .attr(%y%, ~6~)
        .attr(%dy%, %.71em%)
        .style(%text-anchor%, %end%)
        .text(%Price ($)%);
    "
  
  quoted_properties <- gsub("%", "", unlist(str_extract_all(JS_function, "%.+?%")))
  
  unquoted_properties <- gsub("~", "", unlist(str_extract_all(JS_function, "~.+?~")))
  
  
  
  
}