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
select.agent <- function(Target.Degree, mat, Act.ID){

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
    alter <- sample(Act.ID[Act.ID!=ego & mat[ego, ]!=1], 1)
    # 対象となる組み合わせを返す
    out <- c(ego = ego, alter = alter)
  }
  # リターン
  return(out)
}
