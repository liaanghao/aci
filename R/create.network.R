#' Simulation: Create Simulating Networks
#' @description Network simulation based on agent-based models (ABM).
#' @param N The number of nodes.
#' @param Belief The belief scores of each nodes.
#' @param Degree The target degree.
#'
#' @return \code{time}: time used for creating the network,
#'         \code{mat}: the imported network matrix,
#'         \code{Belief}: the belief scores of each nodes,
#'         \code{Degree}: the target degree.
#' @export
#'
#' @examples
create.network <- function(N, Belief, Degree){
  #　アクターID
  Act.ID <- as.character(1:N)
  # DegreeにAct.IDを付与する
  names(Degree) <- Act.ID
  # 最初の空のマトリクスを用意する
  mat <- matrix(0, N, N)
  dimnames(mat) <- list(Act.ID, Act.ID)
  # 時間を初期設定
  time <- 0

  # ここからリピート///////////////////////////
  repeat{
    # 検討するego, alterの組を選ぶ
    Candid.Ego.Alter <- select.agent(Act.ID = Act.ID,
                                     Target.Degree = Degree,
                                     mat = mat)

    # すでに目標Degreeに達している場合にはリピート終了
    if(length(Candid.Ego.Alter)==1 && Candid.Ego.Alter=="END"){
      break
    }

    # アクターの選好関数
    Attractiveness <- give.attractiveness.score(Baseline.Probability = 0.2)

    # egoがalterとエッジを結ぶ確率
    Prob <- 1/(1+exp((-1*Attractiveness)))
    # 確率が１を越えたばあには１を代入
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
  }
  #////////////リピートここまで///////////////

  # アウトプット
  out <- list(time = time,
              mat  = mat,
              Belief = Belief,
              Degree = Degree)
  # リターン
  return(out)
}
