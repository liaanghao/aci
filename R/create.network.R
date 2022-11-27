#' Network simulation based on ABM.
#'
#' @param mat.init
#' @param belief
#' @param spatial.coord
#' @param target.outdegree
#' @param target.density
#' @param trial.limit
#' @param max.time
#' @param base.prob
#' @param homoph
#' @param recip
#' @param popul
#' @param activ
#' @param transiv
#' @param common.target
#' @param common.source
#' @param proximity
#' @param display.time
#' @param display.plot
#' @param seed
#' @param sparseM.output
#'
#' @return
#' @export
#'
#' @examples

#### VALUES ##########
# mat.init: 初期マトリクス（通常はゼロ行列）
# belief: agentの信念値（0~1の範囲）。個別に与える必要
# spatial.coord: アクターの空間的位置。デフォルトはNULL.
# target.outdegree: 目標とする出次数。デフォルトはNULL.
# target.density: 目標とする密度。デフォルトはNULL.
# trial.limit: 各アクターがトライできる最大回数。デフォルトはNULL。
# max.time: 最大時間。デフォルトは10000。
# base.prob: アクター間にエッジがランダムに引かれる確率。デフォルトは0.05.
# homoph: ホモフィリースコア。
# recip: レシプロシティスコア
# popul: 人気性スコア
# activ: アクティブ性スコア
# transiv: 推移性スコア
# common.target: 共通ターゲットスコア
# common.source: 共通ソーススコア
# proximity: 空間的近接性
# display.time: 経過時間を表示するか。デフォルトはFALSE
# display.plot: プロットをアウトプットで表示するか。デフォルトはFALSE.
# seed: ランダムシード。デフォルトはNULL。
# sparseM.output: アウトプットマトリクスを疎行列で示すか。デフォルトはFALSE.

create.network <- function(
    mat.init,
    belief = 0,
    spatial.coord = NULL,
    target.outdegree = NULL,
    target.density = NULL,
    trial.limit = NULL,
    max.time = 10000,
    base.prob = 0.05,
    homoph = 0,
    recip = 0,
    popul = 0,
    activ = 0,
    transiv = 0,
    common.target = 0,
    common.source = 0,
    proximity = 0,
    display.time = FALSE,
    display.plot = FALSE,
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
  # spatial.positの情報によってdistを計算仕分ける
  if(is.null(spatial.coord)){
    spatial.dist <- matrix(0, n, n)
  }else if(ncol(spatial.coord)<=2){
    spatial.dist <- as.matrix(dist(spatial.coord))
  }else{
    spatial.dist <- spatial.coord
  }
  # BeliefからAttribute matrixを作成
  attribute.mat <- 1-as.matrix(dist(belief, method = "euclidean"))
  # 時間を初期設定
  time <- 0

  # 選好係数---------------------------------
  beta <- c(1, homoph, recip, popul, activ,
            transiv, common.target, common.source, proximity)

  # 記録表------------------------------------
  record <- matrix(0, 1, 15, byrow = T,
                   dimnames = list("", c("time","ego","alter",
                                         "score","prob","decision",
                                         "intercept",
                                         "homoph","recip","popul",
                                         "activ","transiv",
                                         "common.target","common.source",
                                         "proximity")))
  record <- record[-1, ]
  #リピート中に使用する関数

  # ここからリピート///////////////////////////
  repeat{
    # 対象となるego, alterの組を計算する
    candid <- select.agent(
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
    X <- gas.X(mat = mat,
               attribute.mat = attribute.mat,
               base.prob = base.prob,
               spatial.dist = spatial.dist)
    # 候補となるego-alter間のスコア
    X.candid <- t(apply(candid, 1, function(t){
      X[t[1], t[2], ]
    }))

    base.score <- t(beta*t(X.candid))
    # 候補となるego-alter間のスコア
    score <- apply(base.score, 1, sum)
    # 各ego-alterがエッジを結ぶ確率
    prob <- 1/(1+exp((-1*score)))
    # 確率が１を越えた場合には１を代入
    prob[prob > 1] <- 1
    # 各agentの決定
    decision <- apply(data.frame(prob), 1, function(x){
      sample(c(1, 0), prob = c(x, 1-x), 1)})
    # レコードの更新
    new.record <- cbind(time, candid, score, prob, decision,base.score)
    record     <- rbind(record, new.record)
    new.edges   <- new.record[new.record[ ,"decision"]==1, c("ego","alter")]
    # ネットワークを更新
    if(NROW(new.edges)>0){
      for(i in 1:nrow(new.edges)){
        mat[new.edges[i,"ego"], new.edges[i, "alter"]] <- 1
      }
    }
    # 現在時間の表示（オプション）
    if(display.time==TRUE){
      print(paste("current time:", time))
    }
  }
  #////////////リピートここまで///////////////

  # 結果を時間単位でまとめる
  record.by.time <- data.frame(matrix(0, time, 15, byrow = T))
  colnames(record.by.time) <- colnames(record)
  record.by.time$time <- 1:time
  record.by.time$ego <- unlist(lapply(tapply(record$ego, record$time, unique),length))
  record.by.time$alter <- unlist(lapply(tapply(record$alter, record$time, unique),length))
  record.by.time$score <- tapply(record$score, record$time, mean)
  record.by.time$prob <- tapply(record$prob, record$time, mean)
  record.by.time$decision <- tapply(record$decision, record$time, sum)
  record.by.time$X.intercept <- tapply(record$X.intercept, record$time, mean)
  record.by.time$X.homoph <- tapply(record$X.homoph, record$time, mean)
  record.by.time$X.recip <- tapply(record$X.recip, record$time, mean)
  record.by.time$X.popul <- tapply(record$X.popul, record$time, mean)
  record.by.time$X.activ <- tapply(record$X.activ, record$time, mean)
  record.by.time$X.transiv <- tapply(record$X.transiv, record$time, mean)
  record.by.time$X.common.target <- tapply(record$X.common.target, record$time, mean)
  record.by.time$X.common.source <- tapply(record$X.common.source, record$time, mean)
  record.by.time$X.proximity <- tapply(record$X.proximity, record$time, mean)

  # report parameters


  # アウトプット
  # sparce matrixをもとのマトリクスに戻すか
  if(sparseM.output == TRUE){
    mat <- as(mat, "sparseMatrix")
  }
  out <- list(mat  = mat,
              belief = belief,
              target.outdegree = target.outdegree,
              time = time,
              record = record,
              record.by.time = record.by.time,
              parameters = c(belief = belief, spatial.coord = spatial.coord,
                             target.outdegree = target.outdegree,target.density = target.density,
                             trial.limit = trial.limit, base.prob = base.prob,homoph = homoph,
                             recip = recip, popul = popul,activ = activ, transiv = transiv,
                             common.target = common.target,common.source = common.source,
                             proximity = proximity))
  # プロット
  if(display.plot==TRUE){
    list(
      gplot(as.matrix(out$mat), vertex.col = belief,
            main = "network"),
      plot(x = record.by.time$time,
           y = record.by.time$prob,
           xlab = "time", ylab = "Prob",
           ylim = c(0,1),
           type = "l",
           main = "Ave. prob. of making ties"),
      plot(x = record.by.time$time,
           y = record.by.time$decision/sum(record.by.time$decision),
           type = "l",
           xlab = "time", ylab = "% of new ties",
           main = "% of new ties realized among all realized ties"),
      plot(x = record.by.time$time,
           y = record.by.time$X.homoph,
           type = "l",
           xlab = "time", ylab = "ave. homoph score",
           main = "Ave. homoph score"),
      plot(x = record.by.time$time,
           y = record.by.time$X.recip,
           type = "l",
           xlab = "time", ylab = "ave. recip score",
           main = "Ave. recip score"),
      plot(x = record.by.time$time,
           y = record.by.time$X.popul,
           type = "l",
           xlab = "time", ylab = "ave. recip score",
           main = "Ave. popul score"),
      plot(x = record.by.time$time,
           y = record.by.time$X.activ,
           type = "l",
           xlab = "time", ylab = "ave. activ score",
           main = "Ave. activ score"),
      plot(x = record.by.time$time,
           y = record.by.time$X.transiv,
           type = "l",
           xlab = "time", ylab = "ave. transiv score",
           main = "Ave. transiv score"),
      plot(x = record.by.time$time,
           y = record.by.time$X.common.target,
           type = "l",
           xlab = "time", ylab = "ave. common target score",
           main = "Ave. common target score"),
      plot(x = record.by.time$time,
           y = record.by.time$X.common.source,
           type = "l",
           xlab = "time", ylab = "ave. common source score",
           main = "Ave. common source score"),
      plot(x = record.by.time$time,
           y = record.by.time$X.proximity,
           type = "l",
           xlab = "time", ylab = "ave. proximity",
           main = "Ave. proximity source score")
    )
  }
  # リターン
  return(out)
}
