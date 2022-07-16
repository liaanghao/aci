#' Attribute Attractive Scores
#' @description Attribute attractive scores between two nodes to
#' decide how likely they are going to form a tie.
#' @param beta baseline probability. Default is set to as 0.05.
#' @return
#' @export
#'
#' @examples
give.attra.score <- function(
    beta = 0.05
){
  # calculate the base probability
  Constant <- -log(1/beta-1)

  # calculate probability based on network features
  Score <- Constant

  # return
  return(Score)
}
