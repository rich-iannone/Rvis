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
  
  # Obtain general information about the JS function calls
  functions.first <- sets[1]
  functions.invocations <- grep("^([\\.][^0-9])", sets, value = TRUE)
  functions.indices <- grep("^([\\.][^0-9])", sets)
  functions.quantity <- length(functions.indices) + 1
  
  # Get information on the quoted parameters
  if (grepl("^([a-zA-Z]+[\\.][a-zA-Z]*)", sets)[1] == TRUE){
    
    quoted_parameters.values <-
      grep("^([^\\.~].*)|^([\\.][0-9].*)",
           sets, value = TRUE)[2:length(grep("^([^\\.~].*)|^([\\.][0-9].*)", sets))]
    
    quoted_parameters.indices <-
      grep("^([^\\.~].*)|^([\\.][0-9].*)",
           sets)[2:length(grep("^([^\\.~].*)|^([\\.][0-9].*)", sets))]
    
    quoted_parameters.quantity <- length(quoted_parameters.indices)
  }
  
  if (grepl("^([a-zA-Z]+[\\.][a-zA-Z]*)", sets)[1] == FALSE){
    
    quoted_parameters.values <-
      grep("^([^\\.~].*)|^([\\.][0-9].*)", sets, value = TRUE)
    
    quoted_parameters.indices <-
      grep("^([^\\.~].*)|^([\\.][0-9].*)", sets)
    
    quoted_parameters.quantity <- length(quoted_parameters.indices)
  }
  
  # Get information on the unquoted parameters
  
  unquoted_parameters.values <- grep("^~", sets, value = TRUE)
  unquoted_parameters.indices <- grep("^~", sets)
  unquoted_parameters.quantity <- length(unquoted_parameters.indices)
  
  # functions.quantity + quoted_parameters.quantity + unquoted_parameters.quantity
  
  for (i in 1:functions.quantity){
    if (i == 1){
    params <- seq(from = 1 + 1, to = functions.indices[1] - 1, by = 1)
    }
    if (i > 1 & i < functions.quantity){
      params <- seq(from = functions.indices[i - 1] + 1,
                    to = functions.indices[i] - 1,
                    by = 1)
    }
    if (i == functions.quantity){
      params <- seq(from = functions.indices[functions.quantity - 1] + 1,
                    to = quoted_parameters.indices[length(quoted_parameters.indices)],
                    by = 1)
    }
    
    for (j in params){
      if (j == params[1]) params_list <- vector(mode = "character", length = 0)
      if (is.na(match(j, quoted_parameters.indices)) == FALSE){
        param <- quoted_parameters.values[match(j, quoted_parameters.indices)]
        param <- paste("\"", param, "\"", sep = '')
      }
      if (is.na(match(j, unquoted_parameters.indices)) == FALSE){
        param <- unquoted_parameters.values[match(j, unquoted_parameters.indices)]
      }
      params_list <- c(params_list, param)
      if (j == params[length(params)]){
        params_list <- paste()
      }
    }
    
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
