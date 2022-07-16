#' Calculating average degree from density, vise versa
#'
#' @param N The number of the network nodes.
#' @param Density Density of the network.
#' @param Ave.Degree Average degree of the network.
#' @param mode Mode of the network object, Default = \code{digraph}.
#' @import statnet
#' @return
#' @export
#'
#' @examples
avedig.dens.switch <- function(
    N, Density = NULL, Ave.Degree = NULL, mode = "digraph"){

  # 何を求めるのかを判定する
  if(is.null(Density) && !is.null(Ave.Degree)){
    target <- "density"
  }else if(is.null(Ave.Degree) && !is.null(Density)){
    target <- "degree"
  }else{
    stop(message="Supply density OR average degree, not both")
  }

  # 有向グラフの場合
  if(mode == "digraph"){
    # target = "density"の場合
    if(target == "density"){
      L <- Ave.Degree*N
      out <- L/(N*(N-1))
      # target == "degree"の場合
    }else if(target == "degree"){
      L <- Density*N*(N-1)
      out <- L/N
    }

    # 無向グラフの場合
  }else if(mode == "graph"){
    # target = "density"の場合
    if(target == "density"){
      L <- Ave.Degree*N/2
      out <- 2*L/(N*(N-1))
      # target == "degree"の場合
    }else if(target == "degree"){
      L <- Density*N*(N-1)/2
      out <- 2*L/N
    }
  }
  # アウトプット
  return(out)}
