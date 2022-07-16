#' Simulation: Select agent at random
#'
#' @param Act.ID An ID list of actors.
#' @param Target.Degree The number of degree.
#' @param mat  A network matrix.
#'
#' @return
#' @export
#'
#' @examples
select.agent <- function(Act.ID = NULL, Target.Degree, mat){
  # Target.Degreeとmatの長さがあっているかをチェック
  if(length(Target.Degree)!=nrow(mat)){
    stop("The length of Target degree differs from the matrix")
  }
  # アクターのIDリストを作成する（未入力の場合）
  if(is.null(Act.ID)){
    Act.ID <- as.character(1:nrow(mat))
    names(Target.Degree) <- Act.ID
  }
  # まだ意図したdegreeに達していないAgentsのリスト
  Current.degree <- rowSums(mat)
  ego.candid <- Act.ID[Current.degree < Target.Degree]

  # もしもすべてのエゴがTarget Degreeに達している場合NAを返す
  if(length(ego.candid)==0){
    out <- "END"
  }else{
    # 対象となるエゴを一つ選ぶ
    ego <- sample(ego.candid, 1)
    # 対象となるalterを選ぶ
    alter <- sample(Act.ID[Act.ID!=ego], 1)
    # 対象となる組み合わせを返す
    out <- c(ego = ego, alter = alter)
  }

  # リターン
  return(out)
}
