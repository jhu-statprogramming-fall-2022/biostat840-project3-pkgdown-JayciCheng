#' Calculate greatest top 3
#'
#' Calculate greatest top 3 numbers in a given list
#'
#' @name top3_high
#'
#' @details This function calculates the greatest top 3 numbers in a list by bubble sort
#'
#' @param arr the target array in which we need to find the greatest 3 number
#'
#' @return high3
#'
#' @export top3_high
#'
#' @examples
#' top3_high(1:5)
#'
top3_high <- function(arr) {
  n = length(arr)
  if (n <= 3) return(arr)
  for (i in 1:3) {
    for (j in (i+1):n) {
      if(arr[i]<arr[j]){
        temp = arr[i]
        arr[i] = arr[j]
        arr[j] = temp
      }
    }
  }
  high3 <- arr[1:3]
  return(high3)
}
