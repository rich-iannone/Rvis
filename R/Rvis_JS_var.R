Rvis_JS_var <- function(sets){
    

  # Get the number of variables to set
  number_of_vars <- length(sets) / 2
  
  # Get a vector list of variable names
  for (i in seq(from = 1, to = length(sets) - 1, by = 2)){
    if (i == 1) var_names <- vector(mode = "character", length = 0)
    var_name <- sets[i]
    var_names <- c(var_names, var_name)
    if (i == length(sets) - 1) rm(var_name)
  }
  
}
