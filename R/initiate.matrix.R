#' Form an initial matrix for simulation
#'
#' @param n the number of nodes.
#'
#' @return
#' @export
#'
#' @examples
initiate.matrix <- function(n){
  mat <- matrix(0, n, n)
  # IDを付与する
  dimnames(mat) <- list(1:n, 1:n)
  # アウトプット
  mat
}
