Rvis_JS_function <- function(sets){
 
  # test data
  sets <- c("svg.append", "g",
            ".attr", "class", "y axis",
            ".call", "~yAxis",
            ".append", "text",
            ".attr", "transform", "rotate(-90)",
            ".attr", "y", "~6",
            ".attr", "dy", ".71em",
            ".style", "text-anchor", "end",
            ".text", "Price ($)")
  
  for (i in 1:length(sets)){
    functions.indices <- grep("^([\\.][^0-9])", sets, value = TRUE)
    quoted.parameters <- grep("^([^\\.~].*)|^([\\.][0-9].*)", sets, value = TRUE)
    grep("^([a-zA-Z]+[\\.][a-zA-Z]*)", sets, value = TRUE)
  }
  
  
  # Final formatted output
  
  #   svg.append("g")
  #   .attr("class", "y axis")
  #   .call(yAxis)
  #   .append("text")
  #   .attr("transform", "rotate(-90)")
  #   .attr("y", 6)
  #   .attr("dy", ".71em")
  #   .style("text-anchor", "end")
  #   .text("Price ($)");
  
  
}
