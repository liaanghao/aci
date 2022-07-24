#' Calculating Advocacy Coalition Index
#'
#' @param matrix An advocacy coalition network matrix.
#' @param policy.score A numeric object of belief scores.
#' @param alpha threshold uses to decide the cut off value.
#' @param print whether to print subsystem-level ACI output in Console. Default is \code{FALSE}.
#'
#' @return Advocacy Coalition Index in three levels:
#'     Subsystem-level, Coalition-level, and actor-level.
#' @export
#' @examples
aci <- function(matrix,
                policy.score,
                alpha = 0.5,
                print = FALSE){
  # impute data
  Y <- matrix
  a <- policy.score
  diag(Y) <- 0

  # number of actors
  n <- length(a)

  # transform the score into matrix
  X <- matrix(NA, n, n)
  dimnames(X) <- dimnames(Y)
  for(i in 1:length(a)){for(j in 1:length(a)){
    X[i,j] <- 1-abs(a[i]-a[j])
  }}
  diag(X) <- NA

  # distance(D)
  D <- abs(Y-X)


  # differentiate the submatrix
  D.cross <- D
  D.cross[Y<X] <- 0
  D.missing <- D
  D.missing[Y>X] <- 0

  # coalition matrix
  coalition.mat <- Y
  coalition.mat[D>alpha] <- 0
  dimnames(coalition.mat)<-dimnames(Y)

  # ACI-actor(out)
  act.cross.out <- rowSums(D.cross, na.rm = T)/rowSums(1-X, na.rm = T)
  act.homo.out  <- 1-rowSums(D.missing, na.rm = T)/rowSums(X, na.rm = T)
  act.ACI.out   <- 1-(rowSums(D, na.rm = T)/(n-1))

  # ACI-actor(in)
  act.cross.in <- colSums(D.cross, na.rm = T)/colSums(1-X, na.rm = T)
  act.homo.in  <- 1-colSums(D.missing, na.rm = T)/colSums(X, na.rm = T)
  act.ACI.in   <- 1-(colSums(D, na.rm = T)/(n-1))

  # ACI-whole
  whole.cross  <- sum(D.cross, na.rm = T)/sum(1-X, na.rm = T)
  whole.homo   <- 1-sum(D.missing, na.rm = T)/sum(X, na.rm = T)
  whole.ACI    <- 1-(sum(D, na.rm = T)/(n*(n-1)))

  ## attach the result
  act <- data.frame(act.ACI.out    = act.ACI.out,
                    act.cross.out = act.cross.out,
                    act.homo.out  = act.homo.out,
                    act.ACI.in     = act.ACI.in,
                    act.cross.in  = act.cross.in,
                    act.homo.in   = act.homo.in)
  whole <- c(whole.ACI     = whole.ACI,
             whole.cross  = whole.cross,
             whole.homo   = whole.homo,
             CCH = whole.cross / whole.homo)
  result <- list(act=act,
                 whole=whole,
                 coalition.mat = coalition.mat,
                 original.matrix.Y = Y,
                 preference.agreement.matrix.X = X)

  ## return the result
  if(print==TRUE){
    print(result$whole)
  }
  invisible(result)
}
