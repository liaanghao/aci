#' Attribute Attractive Scores
#'
#' @param ego Ego node.
#' @param alter Alter node.
#' @param mat Network matrix.
#' @param attribute.mat Attribute matrix.
#' @param base.prob Base probability.
#' @param homoph Homophilious score.
#' @param recip Reciprocity score.
#' @param popul Popularity score.
#' @param activ Activity score.
#' @param transiv Transitivity score.
#' @param common.target Common target.
#' @param common.source Common source.
#'
#' @description Attribute attractive scores between two nodes to
#' decide how likely in forming a tie.
#' @return
#' @export
#' @import sna
#' @examples
#'
give.attra.score <- function(
    ego,
    alter,
    mat,
    attribute.mat,
    base.prob = 0,
    homoph = 0.0,
    recip = 0.0,
    popul = 0.0,
    activ = 0.0,
    transiv = 0.0,
    common.target = 0.0,
    common.source = 0.0
){

  # アクターの数
  n <- nrow(mat)

  # Beta
  Beta <- c(1, homoph, recip, popul,
            activ, transiv, common.target, common.source)

  # 現状のネットワークに基づいたスコア元を計算する
  X <- array(0, dim = c(n, n, 8),
             dimnames = list(rownames(mat),
                             colnames(mat),
                             c("X.base.prob",
                               "X.homoph",
                               "X.recip",
                               "X.popul",
                               "X.activ",
                               "X.transiv",
                               "X.common.target",
                               "X.common.source")))

  # それぞれの変数を計算する

  ## 切片
  # ベースとなる確率を計算
  if(base.prob==0){
    base.prob <- 1e-100
  }
  X[ , ,"X.base.prob"] <- -log(1/base.prob-1)

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

  # NANやInfを一括処理
  X[is.nan(X)] <- 0
  X[is.infinite(X)] <- 1

  # スコアを計算する
  Score <- sum(Beta * X[ego,alter, ])
  ego <- ego
  alter <- alter
  X.record <- X[ego,alter, ]

  # リターン
  out <- list(Score = Score,
              ego = ego,
              alter = alter,
              X.record = X.record)

  return(out)
}
