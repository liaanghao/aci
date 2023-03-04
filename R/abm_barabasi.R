#' Select Agents for ABM_Barabasi
#'
#' @param target.outdegree Target outdegree.
#' @param mat A network matrix.
#' @param actID ID.
#'
#' @return
#' @export
#'
#' @examples
select.agent.altcandid <- function(
    target.outdegree,
    mat,
    actID){
  # current.degree
  current.outdegree <- rowSums(mat)
  # まだ意図したdegreeに達していないAgentsのリスト
  egolist <- actID[current.outdegree < target.outdegree]
  # もしもすべてのエゴがTarget Degreeに達している場合NAを返す
  if(length(egolist)==0){
    out <- "END"
  }else{
    # agentを一つ選ぶ
    agent <- sample(egolist, 1)
    # 当該agentがまだ可能なaltlist
    alterlist <- colnames(mat)[names(mat[agent, ])!=agent & mat[agent,]!=1]
    # 対象となる組み合わせを返す
    out <- list(ego = agent,
                alterlist = alterlist)
  }
  # リターン
  out
}

#' Assign preference scores
#'
#' @param mat Network matrix.
#' @param attribute.mat Attribute matrix.
#' @param intercept Intercept.
#' @param spatial.dist Spatial distance among nodes.
#' @param beta Coefficients.
#'
#' @return
#' @export
#'
#' @examples
gas <- function(
    mat,
    attribute.mat,
    intercept = 1,
    spatial.dist,
    beta){
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
  X[ , ,"X.intercept"] <- intercept * beta[1]
  ## homophily
  X[ , ,"X.homoph"] <- attribute.mat * beta[2]
  ## reciprocity
  X[ , ,"X.recip"]   <- t(mat) * beta[3]
  ## popularity
  X[ , ,"X.popul"]  <- (t(colSums(mat)  * matrix(1, n, n))) * beta[4]
  ## Activity
  X[ , ,"X.activ"]  <- (t(rowSums(mat)  * matrix(1, n, n))) * beta[5]
  ## Transitivity
  X[ , ,"X.transiv"]  <- (mat %*% mat) * beta[6]
  ## Common target
  X[ , ,"X.common.target"]  <- (mat %*% t(mat)) * beta[7]
  ## Common source
  X[ , ,"X.common.source"]  <- (t(mat) %*% mat) * beta[8]
  ## spatial proximity
  X[ , ,"X.proximity"] <- (spatial.dist*(-1)) * beta[9]
  # NANやInfを一括処理
  X[is.nan(X)] <- 0
  X[is.infinite(X)] <- 1
  # リターン
  X
}
#' Network simulation: tie formation based on a revised barabasi model.
#'
#' @param mat.init Initial matrix.
#' @param belief Belief scores.
#' @param spatial.coord Spatial coordination.
#' @param target.outdegree The target outdegree.
#' @param target.density The target density.
#' @param trial.limit The upper limits of simulation.
#' @param max.time The maximum time for simulation.
#' @param intercept Intercept for calculating preference scores.
#' @param homoph Homophily beta.
#' @param recip Reciprocity beta.
#' @param popul Popularity beta.
#' @param activ Activity beta.
#' @param transiv Transitivity beta.
#' @param common.target Common target beta.
#' @param common.source Common source beta.
#' @param proximity Proximity beta.
#' @param display.time A logic for display time. Default is *FALSE*.
#' @param seed Set seed for simulation.
#' @param sparseM.output A logic for export sparse matrix. Default is *FALSE*.
#'
#' @return
#' @export
#'
#' @examples
abm_barabasi <- function(
    mat.init,
    belief = 0,
    spatial.coord = NULL,
    target.outdegree = NULL,
    target.density = NULL,
    trial.limit = NULL,
    max.time = 10000,
    intercept = 1,
    homoph = 0,
    recip = 0,
    popul = 0,
    activ = 0,
    transiv = 0,
    common.target = 0,
    common.source = 0,
    proximity = 0,
    display.time = FALSE,
    seed = NULL,
    sparseM.output = FALSE){
  #基礎設定---------------------------
  # seed
  if(is.null(seed)==FALSE){
    set.seed(seed = seed)
  }
  # アクター数
  n <- nrow(mat.init)
  # マトリクス
  mat <- mat.init
  # (元のデータにアクターIDが付与されていない場合)
  if(is.null(rownames(mat))){
    dimnames(mat) <- list(1:n, 1:n)}
  #　アクターIDを取り出す
  actID <- rownames(mat)
  # beliefがlength = 1の場合にアクター分同じ数をコピーする
  if(length(belief)==1){
    belief <- rep(belief, n)}
  # outdegreeがlength = 1の場合にアクター分同じ数をコピーする
  if(is.null(target.outdegree)){
    target.outdegree <- rep(n-1, n)
  }else if(length(target.outdegree)==1){
    target.outdegree <- rep(target.outdegree, n)
  }
  if(any(target.outdegree >= n)){
    target.outdegree[target.degree>=n] <- n-1
    warning("target outdegree exceeds the possible number of ties")
  }
  # spatial.coordの情報によってdistを計算仕分ける
  if(is.null(spatial.coord)){
    spatial.dist <- matrix(0, n, n)
  }else if(ncol(spatial.coord)<=2){
    spatial.dist <- as.matrix(dist(spatial.coord))
  }else{
    spatial.dist <- spatial.dist
  }
  # BeliefからAttribute matrixを作成
  attribute.mat <- 1-as.matrix(dist(belief, method = "euclidean"))
  # 時間を初期設定
  time <- 0

  # 選好係数---------------------------------
  beta <- c(1, homoph, recip, popul, activ,
            transiv, common.target, common.source, proximity)

  # 記録表------------------------------------
  record <- data.frame(matrix(0, 1, 3, byrow = T,
                              dimnames = list("", c("time","ego","alt"))))
  record <- record[-1, ]
  score.record <- matrix(0, 1, n, byrow = T)
  colnames(score.record) <- 1:n
  score.record <- score.record[-1, ]

  # ここからリピート///////////////////////////
  repeat{
    # 対象となるego, alterの組を計算する
    candid <- select.agent.altcandid(
      target.outdegree = target.outdegree,
      mat = mat,
      actID = actID)
    # すでにconstraint条件に達している場合にはリピート終了
    if(length(candid)==1 && candid=="END"| # すべての候補者が目標degreeに達した
       time == max.time|                   # 時間が上限を越えた
       !is.null(target.density) && sum(mat)/(n*(n-1)) >= target.density| # 密度が上限を越えた
       NROW(record)>=1 && !is.null(trial.limit) && any(table(record$ego) >= trial.limit)    # 各人の試行回数が上限を越えた
    ){
      break
    }
    # 時間の更新
    time <- time + 1
    # アクターの選好計算-------------------------
    # 現状のmatrix値をもとにgasのXを計算する
    X <- gas(mat = mat,
             attribute.mat = attribute.mat,
             intercept = intercept,
             spatial.dist = spatial.dist,
             beta = beta)
    # すべてのスコアを合計する
    base.score <- apply(X, c(1,2), sum)
    # 今回関係するagentのスコア
    altcandid.score <- base.score[candid$ego, candid$alterlist]
    # 確率
    prob <- altcandid.score/sum(altcandid.score)
    #alterの決定
    alt.decided <- sample(candid$alterlist, 1, prob = prob)
    # レコードの更新
    new.record <- c(time = time, ego = candid$ego, alt = alt.decided)
    record <- rbind(record, new.record)
    score.record <- rbind(score.record, base.score)
    # ネットワークを更新
    mat[new.record["ego"], new.record["alt"]] <- 1
    # 現在時間の表示（オプション）
    if(display.time==TRUE){
      print(paste("current time:", time))
    }
  }
  #////////////リピートここまで///////////////
  # アウトプット
  # sparce matrixをもとのマトリクスに戻すか
  if(sparseM.output == TRUE){
    mat <- as(mat, "sparseMatrix")
  }
  out <- list(mat  = mat,
              belief = belief,
              target.outdegree = target.outdegree,
              record = record,
              score.record = score.record)
  # リターン
  return(out)
}
