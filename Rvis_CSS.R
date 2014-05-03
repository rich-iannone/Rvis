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
}
