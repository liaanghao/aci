#' Simulation: Create Simulating Networks
#' @description Network simulation based on agent-based models (ABM).
#'
#' @param n The number of nodes.
#' @param Belief The belief scores of each nodes.
#' @param base.prob see \code{give.attra.score}.
#' @param homoph see \code{give.attra.score}.
#' @param recip see \code{give.attra.score}.
#' @param popul see \code{give.attra.score}.
#' @param activ see \code{give.attra.score}.
#' @param transiv see \code{give.attra.score}.
#' @param common.target see \code{give.attra.score}.
#' @param common.source see \code{give.attra.score}.
#' @param display.time display time for simulation, default is \code{False}.
#' @param display.plot display simulation results in plot, default is \code{False}.
#' @param max.time maximum time for simulation, default is \code{100000}.
#' @param Degree The target degree.
#' @param plotfamily Define a family of fonts for plot.
#'
#' @return \code{time}: time used for creating the network,
#'         \code{mat}: the imported network matrix,
#'         \code{Belief}: the belief scores of each nodes,
#'         \code{Degree}: the target degree.
#'         \code{Record}: the record of edge formation.
#' @export
#' @examples
#'
create.network <- function(
    n, Belief, Degree,
    base.prob = 0.05,
    homoph = 0,
    recip = 0,
    popul = 0,
    activ = 0,
    transiv = 0,
    common.target = 0,
    common.source = 0,
    display.time = FALSE,
    display.plot = FALSE,
    plotfamily = NA,
    max.time = 100000){


  #　アクターID
  Act.ID <- as.character(1:n)
  # DegreeにAct.IDを付与する
  names(Degree) <- Act.ID
  # 最初の空のマトリクスを用意する
  mat <- matrix(0, n, n)
  dimnames(mat) <- list(Act.ID, Act.ID)
  # 時間を初期設定
  time <- 0
  # BeliefからAttribute matrixを作成
  attribute.mat <- 1-as.matrix(dist(Belief, method = "euclidean"))

  # 記録表
  ego.record <- c()
  alter.record <- c()
  decision.record <- c()
  X.record <- matrix(0, 1, 8, byrow = T)
  X.record <- X.record[-1, ]
  score.record <- c()
  prob.record <- c()

  # ここからリピート///////////////////////////
  repeat{
    # 検討するego, alterの組を選ぶ
    Candid.Ego.Alter <- select.agent(Act.ID = Act.ID,
                                     Target.Degree = Degree,
                                     mat = mat)

    # すでに目標Degreeに達している場合にはリピート終了
    if(length(Candid.Ego.Alter)==1 && Candid.Ego.Alter=="END"|
       time == max.time){
      break
    }

    # アクターの選好関数
    gas.out <- give.attra.score(
      ego = Candid.Ego.Alter["ego"],
      alter = Candid.Ego.Alter["alter"],
      mat = mat,
      attribute.mat = attribute.mat,
      base.prob = base.prob,
      homoph = homoph,
      recip = recip,
      popul = popul,
      activ = activ,
      transiv = transiv,
      common.target = common.target,
      common.source = common.source)

    # Attractive
    Attractiveness <- gas.out$Score

    # egoがalterとエッジを結ぶ確率
    Prob <- 1/(1+exp((-1*Attractiveness)))

    # 確率が１を越えた場合には１を代入
    if(Prob>1){Prob <- 1}

    # Agentの決定
    Decision <- sample(c(1,0), prob = c(Prob, 1-Prob), 1)

    # ネットワークを更新
    if(Decision==1){
      mat[Candid.Ego.Alter["ego"], Candid.Ego.Alter["alter"]] <- 1
    }else{
      mat <- mat
    }

    # 時間を更新
    time <- time + 1

    # 記録を更新
    ego.record <- c(ego.record, gas.out$ego)
    alter.record <- c(alter.record, gas.out$alter)
    X.record <- rbind(X.record, gas.out$X.record)
    score.record <- c(score.record, gas.out$Score)
    prob.record <- c(prob.record, Prob)
    decision.record <- c(decision.record, Decision)

    # 現在時間の表示（オプション）
    if(display.time==TRUE){
      print(paste("current time:", time))
    }
  }

  #////////////リピートここまで///////////////

  # 記録のまとめ
  Record <- data.frame(
    time = 1:time,
    ego = ego.record,
    alter = alter.record,
    X.record,
    score = score.record,
    prob = prob.record,
    Decision = decision.record
  )
  # アウトプット
  out <- list(time = time,
              mat  = mat,
              Belief = Belief,
              Degree = Degree,
              Record = Record)

  # プロット
  if(display.plot==TRUE){
    par(family = plotfamily, mfrow=c(2,2))
      gplot(out$mat, vertex.col = Belief,
            main = "1. network")

      plot(x = Record$time,
           y = cumsum(Record$Decision)/sum(Record$Decision),
           type = "l",
           xlab = "time", ylab = "% of ties realized",
           main = "2. time")

      plot(x = Record$time,
           y = cumsum(Record$score)/seq_along(Record$score),
           xlab = "time", ylab = "score",
           type = "l",
           main = "3. Cum. ave. attract. score")

      plot(x = Record$time,
           y = Record$prob,
           xlab = "time", ylab = "Prob",
           ylim = c(0,1),
           type = "l",
           main = "4. Prob. of making ties")
  }
  # リターン
  return(out)
}
