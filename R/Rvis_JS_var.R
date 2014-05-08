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
  
  # Get a vector list of variable values
  for (i in seq(from = 2, to = length(sets), by = 2)){
    if (i == 2) var_values <- vector(mode = "character", length = 0)
    var_value <- sets[i]
    if (grepl(",", var_value) == TRUE){
      split <- unlist(strsplit(var_value, ","))
      split <- gsub(" ", "", split)
      number <- length(split)
      for (j in seq(from = 1, to = length(split) - 1, by = 2)){
        if (j == 1) c1 <- vector(mode = "character", length = 0)
        c1_part <- split[j]
        c1 <- c(c1, c1_part)
        if (j == length(split) - 1) rm(c1_part, j)
      }
      for (j in seq(from = 2, to = length(split), by = 2)){
        if (j == 2) c2 <- vector(mode = "character", length = 0)
        c2_part <- split[j]
        c2 <- c(c2, c2_part)
        if (j == length(split)) rm(c2_part, j)
      }
      for (j in 1:(number/2)){
        if (j == 1) var_value <- vector(mode = "character", length = 0)
        var_value_part <- paste(c1[j], ": ", c2[j], sep = '')
        var_value <- c(var_value, var_value_part)
        if (j == (number/2)){
          var_value <- c(gsub("$", ", ", var_value[1:((number/2) - 1)]), var_value[number/2]) 
          var_value <- paste(var_value, collapse = '')
          rm(var_value_part, j)
        }
      }
    }
    var_values <- c(var_values, var_value)
    if (i == length(sets)) rm(var_value)
  }
  
  # Generate and return a formatted variable declaration string
  for (i in 1:number_of_vars){
    if (i == 1){
      formatted_var <- vector("numeric", length = 0)
      formatted_var_row <- paste("var ", var_names[i], " = ", var_values[i], "\n", sep = '')
    }
    if (i > 1){
      formatted_var_row <- paste("    ", var_names[i], " = ", var_values[i], "\n", sep = '')
    }
    formatted_var <- c(formatted_var, formatted_var_row)
    if (i == number_of_vars){
      formatted_var <- paste(formatted_var, collapse = '')
      rm(formatted_var_row)
    }
  }
  
  return(formatted_var)
  
}
