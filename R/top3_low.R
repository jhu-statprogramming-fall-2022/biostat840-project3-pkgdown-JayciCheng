#' Calculate smallest top 3
#'
#' Calculate smallest top 3 numbers in a given list
#'
#' @name top3_low
#'
#' @details This function calculates the smallest top 3 numbers in a list by bubble sort
#'
#' @param arr the target array in which we need to find the smallest 3 number
#'
#' @return low3
#'
#' @export top3_low
#'
#' @examples
#' top3_low(c(1,3,5,7,9))
#'
top3_low <- function(arr) {
  n = length(arr)
  if (n <= 3) return(arr)
  for (i in 1:3) {
    for (j in (i+1):n) {
      if(arr[i]>arr[j]){
        temp = arr[i]
        arr[i] = arr[j]
        arr[j] = temp
      }
    }
  }
  low3 <- arr[1:3]
  return(low3)
}
