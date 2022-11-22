#' Assign attraction score
#'
#' @param mat A network matrix.
#' @param attribute.mat A matrix for belief score (network edges).
#' @param base.prob Base probability. the default is .05.
#' @param spatial.dist Spatial distance between nodes.
#'
#' @return
#' @export
#'
#' @examples
gas.X <- function(
    mat,
    attribute.mat,
    base.prob = 0.05,
    spatial.dist){
  # アクターの数
  n <- nrow(mat)
  # 現状のネットワークに基づいたスコア元を計算する
  X <- array(0, dim = c(n, n, 9),
             dimnames = list(rownames(mat),
                             colnames(mat),
                             c("X.intercept",
                               "X.homoph",
                               "X.recip",
                               "X.popul",
                               "X.activ",
                               "X.transiv",
                               "X.common.target",
                               "X.common.source",
                               "X.proximity")))
  # それぞれの変数を計算する
  ## 切片
  # ベースとなる確率を計算
  if(base.prob==0){
    base.prob <- 1e-100
  }
  X[ , ,"X.intercept"] <- -log(1/base.prob-1)
  ## homophily
  X[ , ,"X.homoph"] <- attribute.mat
  ## reciprocity
  X[ , ,"X.recip"]   <- t(mat)
  ## popularity
  X[ , ,"X.popul"]  <- t(colSums(mat)/sum(colSums(mat))  * matrix(1, n, n))
  ## Activity
  X[ , ,"X.activ"]  <- t(rowSums(mat)/sum(colSums(mat))  * matrix(1, n, n))
  ## Transitivity
  X[ , ,"X.transiv"]  <- (mat %*% mat)/rowSums(mat)
  ## Common target
  X[ , ,"X.common.target"]  <- (mat %*% t(mat))/rowSums(mat)
  ## Common source
  X[ , ,"X.common.source"]  <- (t(mat) %*% mat)/rowSums(mat)
  ## spatial proximity
  X[ , ,"X.proximity"] <- spatial.dist*(-1)
  # NANやInfを一括処理
  X[is.nan(X)] <- 0
  X[is.infinite(X)] <- 1
  # リターン
  return(X)
}
