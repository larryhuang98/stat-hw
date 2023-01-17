#' Title
#'
#' @param x
#' @param y
#' @param x_row
#' @param x_col
#' @param y_row
#' @param y_col
#'
#' @return
#' @export
#'
#' @examples
matMulmat <- function(x,y, x_row=dim(x)[1], x_col=dim(x)[2], y_row=dim(y)[1], y_col=dim(y)[2] ){
  if (x_col != y_row) stop("Incorrect matrix size")
  result <- matrix(, nrow = x_row, ncol = y_col)
  for (i in 1:x_row) {
    for (j in 1:y_col) {
      my_sum = 0
      for (k in 1:x_col) {
        my_sum = my_sum + x[i,k] * y[k,j]
      }
      result[i,j] = my_sum
    }
  }
  return(result)
}

#' Title
#'
#' @param x
#' @param y
#' @param x_row
#' @param x_col
#' @param y_len
#'
#' @return
#' @export
#'
#' @examples
matMulvec <- function(x,y, x_row=dim(x)[1], x_col=dim(x)[2], y_len=length(y)){
  if (x_col != y_len) stop("Incorrect matrix size")
  result <- c(1:x_row)*0
  for (i in 1:x_row) {
    my_sum = 0
    for (k in 1:x_col) {
      my_sum = my_sum + x[i,k] * y[k]
    }
    result[i] = my_sum
  }
  return(result)
}

#' Title
#'
#' @param x
#' @param y
#' @param z
#' @param mode
#' @param x_row
#' @param x_col
#' @param y_row
#' @param y_col
#' @param z_dim
#'
#' @return
#' @export
#'
#' @examples
matmatvecMul <- function(x,y,z, mode=1, x_row=dim(x)[1], x_col=dim(x)[2], y_row=dim(y)[1], y_col=dim(y)[2],z_dim = length(z) ){
  if (mode == 1) {
    return(matMulvec(matMulmat(x,y),z))
  } else if(mode == 2){
    return(matMulvec(x,matMulvec(y,z)))
  } else{
    stop("Not a valid mode")
  }
}
