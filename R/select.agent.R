#' Randomly select a pair of ego and alter for network simulation
#'
#' @param target.outdegree The outdegree for selected ego.
#' @param mat A network matrix.
#' @param actID the ID of actors.
#'
#' @return
#' @export
#'
#' @examples
select.agent <- function(
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
    # 対象となるalterをそれぞれのegoごとに選ぶ
    alterlist <- apply(data.frame(egolist), 1, function(x){
      sample(actID[actID != x & mat[x, ] != 1], 1)})
    # 対象となる組み合わせを返す
    out <- data.frame(ego = egolist, alter = alterlist)
  }
  # リターン
  return(out)
}
