Rvis_JS_matrix <- function(rows, columns, values){
  
  # Calculate the expected number of values based on the numbers of rows and columns
  expected_values <- rows * columns
  
  # Generate a matrix block from the provided dimensions and values
  if (length(values) == expected_values){
    for (i in 1:rows){
      if (i == 1) matrix_block <- vector("numeric", length = 0)
      for (j in 1:columns){
        if (j == 1) matrix_row <- vector("numeric", length = 0)  
        matrix_element <- values[((i - 1) * columns) + j]
        matrix_row <- c(matrix_row, matrix_element)
        if (j == columns){
          matrix_row <- c(gsub("$", ",", matrix_row[1:(columns-1)]), matrix_row[columns])
        }
      }
      if (i < rows){
        matrix_block <- c(matrix_block, "\t[", matrix_row, "],", "\n")
      }
      if (i == rows){
        matrix_block <- c(matrix_block, "\t[", matrix_row, "]", "\n")
        matrix_block <- paste(matrix_block, collapse = '')
      }
    }
  } else {
    stop("The number of values provided do not match the expected number.")
  }
  
  
}
