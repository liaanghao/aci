#' Add a random noise to edges
#'
#' @param mat an adjacency matrix.
#' @param noise_type a character: "add", "delete", "rewire", or "add.or.delete" (default)
#' @param n_noise_edge a integer that indicates the number of edges to be "noised". Default is NULL.
#' @param noise_prop proportion of the noised edge calculated from the input matrix.
#' @param directed logical indicating if the graph is treated as directed network (default is TRUE)
#' @param FUN_rounding Function for the rounding of the number of n_noise_edge. Default is "round", but it can be replaced with other functions.
#' @param seed an integer for the seed (Default is NULL).
#' @return an adjacency matrix with some noise of edges added.
#' @examples
#' mat <- matrix(0, 5, 5)
#' mat <- add_edge_noise(mat = mat, n_noise_edge = 5)
#' add_edge_noise(mat = mat, noise_prop = 0.05,
#'                noise_type = FUN_rounding = round)


add_edge_noise <- function(
  mat = mat,
  noise_type = "add.or.delete",
  n_noise_edge = NULL,
  noise_prop = 0.05,
  directed = TRUE,
  FUN_rounding = round,
  seed = NULL
){
  # seed
  if(!is.NULL(seed)){
    set.seed(seed = seed)
  }
  # ノード数
  n <- nrow(mat)
  # 初期のマトリクスをコピーしておく
  init.mat <- mat
  # n_noise_edge判定
  if(is.null(n_noise_edge)){
    if(directed==TRUE){
      n_noise_edge <- FUN_rounding(sum(mat)*noise_prop)
    }else if(directed==FALSE){
      n_noise_edge <- FUN_rounding(sum(mat)/2*noise_prop)
    }
  }

  # vertex ID
  vnames <- 1:n

  # ノイズを加える
  if(directed == TRUE){
    if(noise_type == "add"){
      mat[sample(which(mat < 1), n_noise_edge)] <- 1
    }else if(noise_type == "delete"){
      mat[sample(which(mat == 1), n_noise_edge)] <- 0
    }else if(noise_type == "rewire"){
      mat[sample(which(mat == 1), n_noise_edge)] <- 0
      mat[sample(which(mat < 1), n_noise_edge)] <- 1
    }else if(noise_type == "add.or.delete"){
      n_add <- sample(1:n_noise_edge, 1)
      n_delete <- n_noise_edge - n_add
      mat[sample(which(mat < 1), n_add)] <- 1
      mat[sample(which(mat == 1), n_delete)] <- 0
    }
  }else if(directed==FALSE){
    if(noise_type == "add"){
      mat[sample(which(mat < 1), n_noise_edge)] <- 1
      mat <- symmetrize(mat)
    }else if(noise_type == "delete"){
      mat[sample(which(mat == 1), n_noise_edge)] <- 0
      mat <- symmetrize(mat)
    }else if(noise_type == "rewire"){
      mat[sample(which(mat == 1), n_noise_edge)] <- 0
      mat <- symmetrize(mat)
      mat[sample(which(mat < 1), n_noise_edge)] <- 1
      mat <- symmetrize(mat)
    }else if(noise_type == "add.or.delete"){
      n_add <- sample(1:n_noise_edge, 1)
      n_delete <- n_noise_edge - n_add
      mat[sample(which(mat < 1), n_add)] <- 1
      mat <- symmetrize(mat)
      mat[sample(which(mat == 1), n_delete)] <- 0
      mat <- symmetrize(mat)
    }
  }

  # チェック（入力と同じでないか）
  if(all(mat==init.mat)){
    warnings("the output matrix remains the same as the imput")
  }
  # アウトプット
  mat
  }


