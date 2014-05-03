Rvis_CSS <- function(selector, declarations){
  
  # Get the number of CSS declarations
  number_of_declarations <- length(declarations) / 2
  
  # Get a vector list of CSS properties
  for (i in seq(from = 1, to = length(declarations) - 1, by = 2)){
    if (i == 1) properties <- vector(mode = "character", length = 0)
    property <- declarations[i]
    properties <- c(properties, property)
    if (i == length(declarations) - 1) rm(property)
  }
  
  # Get a vector list of CSS property values
  for (i in seq(from = 2, to = length(declarations), by = 2)){
    if (i == 2) values <- vector(mode = "character", length = 0)
    value <- declarations[i]
    values <- c(values, value)
    if (i == length(declarations)) rm(value)
  }
  
  # Generate a formatted list of CSS declarations
  for (i in 1:number_of_declarations){
    if (i == 1) formatted_declarations <- vector(mode = "character", length = 0)
    line <- paste("\t", properties[i], ": ", values[i], ";\n", sep = '')
    formatted_declarations <- c(formatted_declarations, line)
    if (i == number_of_declarations) {
      formatted_declarations <- paste(formatted_declarations, collapse = '')
    }
  }
  
}
