Rvis_tag <- function(tag, o_c = "o"){
  
  # Create an open or closed tag
  if (o_c == "o"){
    formatted_tag <- paste("<", tag, ">\n", sep = '')
  } else if (o_c == "c"){
    formatted_tag <- paste("</", tag, ">\n", sep = '')
  }
  
  return(formatted_tag)
  
}
