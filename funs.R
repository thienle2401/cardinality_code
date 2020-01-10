#' An ultility to check if all data.frame in a list are identical 
#'
#' @param data A list of data.frame and with length(data) > 1
#' @param cols A character vector, which specified the columns which should be used to check identicality among data.frame
#'
#' @return Logical. TRUE if all data.frame with specified columns are identical otherwise FALSE
check_all_equal <- function(data, cols){
  
  x1 <- data[[1]][, ..cols]
  x2 <- data[[2]][, ..cols]
  
  setorderv(x1, cols)
  setorderv(x2, cols)
  
  if(!all(x1 == x2)) return(FALSE) else data[[2]] <- NULL
  if(length(data) != 1) check_all_equal(data, cols)
  
  return(TRUE)
  
}

#' Calcuate objective value for a given matrix 
#'
#' @param x A named matrix
#'
#' @return Numeric vector. Computeted objective value for each row of the input matrix with defined logic 
obj_fun_5G_likeability <- function(x){
  
  # weights defined by user
  weights <- c(
    "ul_vl_amount" = 0.15,
    "dl_vl_amount" = 0.3,
    "ul_rtt_amount" = 0.5,
    "dl_rtt_amount" = 0.05
  )
  
  # scale all values between [0..1] before compute objective value
  scaled_x <- do.call(cbind, lapply(x, rescale))
  
  
  # we need to reresve the direction of RRT, so the lower is better, and then
  # contribute more to the final objective value
  scaled_x[, "dl_rtt_amount"] <- 1 - scaled_x[, "dl_rtt_amount"]
  scaled_x[, "ul_rtt_amount"] <- 1 - scaled_x[, "ul_rtt_amount"]
  
  # compute weighted value for each feature (column)
  obj_val <- scaled_x %*% diag(weights[colnames(scaled_x)])
  
  # return vector of objective values for the input on matrix
  rowSums(obj_val)
}
