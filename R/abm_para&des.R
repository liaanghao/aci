#' Generate multiple random networks
#'
#' @param mat.init Initial network matrix.
#' @param belief Belief scores.
#' @param spatial.coord Spatial coordination.
#' @param target.outdegree Target outdegree to end simulation.
#' @param target.density Target density to end simulation.
#' @param trial.limit The upper limits for trials.
#' @param max.time The maximum time to simulate one network.
#' @param intercept The intercept for prefernece score. Default is 1.
#' @param homoph Homophily beta.
#' @param recip Reciprocity beta.
#' @param popul Popularity beta.
#' @param activ Activity beta.
#' @param transiv Transitivity beta.
#' @param common.target Common target beta.
#' @param common.source Common source beta.
#' @param proximity Proximity beta.
#' @param display.time Display time. Default is *FALSE*.
#' @param seed Seed for random simulation.
#' @param sparseM.output Export sparse matrix. Default is *FALSE*.
#' @param cores The number of cores for parallel comuptation. Default is the *maximum-1*.
#' @param n.sim Number of simulation.
#'
#' @return
#' @export
#'
#' @examples
netsim.single <- function(
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
    sparseM.output = FALSE,
    cores = 2,
    n.sim = 1
){
  # 空のオブジェクトを作成する
  out <- vector(mode = "list", length = n.sim)
  # パラレルクラスターをレジストリする
  if(cores == 0){
    cores <- detectCores()
    cl <- makeCluster(cores-1)
  }
  if(cores != 0){
    cl <- makeCluster(cores)
    }
  registerDoParallel(cl)
  # 計算する
  out <- foreach(i = 1:n.sim, .export="abm_barabasi") %dopar% {
    abm_barabasi(
      mat.init = mat.init,
      belief = belief,
      spatial.coord = spatial.coord,
      target.outdegree = target.outdegree,
      target.density = target.density,
      trial.limit = trial.limit,
      max.time = max.time,
      intercept = intercept,
      homoph = homoph,
      recip = recip,
      popul = popul,
      activ = activ,
      transiv = transiv,
      common.target = common.target,
      common.source = common.source,
      proximity = proximity,
      display.time = display.time,
      seed = seed,
      sparseM.output = sparseM.output)
  }
  # パラレルクラスターをリリースする
  stopCluster(cl)
  # アウトプット
  out
}

#' Description regarding simulated network.
#'
#' @param sim.out The simulated network.
#'
#' @return
#' @export
#'
#' @examples
netdesc <- function(sim.out){
  # オブジェクトの長さを計算する
  n.sim <- length(sim.out)
  # 空のオブジェクトを作成する
  deg.gini <- c()
  deg.sd   <- c()
  mat.clust.coef <- c()
  mat.transitivity <- c()
  mat.ei   <- c()

  # 入次数のみまとめる
  deg <- sna::degree(sim.out[[1]]$mat, cmode = "indegree")
  for(i in 2:n.sim){
    deg <- rbind(deg,
                 sna::degree(sim.out[[i]]$mat, cmode = "indegree"))
  }

  # 記述統計
  for(i in 1:n.sim){
    # indegree
    deg.gini[i] <- gini.wtd(deg[i, ])
    deg.sd[i]   <- sd(deg[i, ])
    # average local clustering coefficient
    mat_i <- graph_from_adjacency_matrix(sim.out[[i]]$mat)
    mat.clust.coef[i] <- transitivity(mat_i, type = "localaverage")
    mat.transitivity[i] <- sna::gtrans(sim.out[[i]]$mat)
    # EI index
    mat.ei[i] <- EI.index(sim.out[[i]]$mat, attr = belief)
  }

  desc.dat <- data.frame(
    m = 1:n.sim,
    deg.gini = deg.gini,
    deg.sd = deg.sd,
    mat.clust.coef = mat.clust.coef,
    mat.transitivity = mat.transitivity,
    mat.ei = mat.ei
  )

  # アウトプット
  out <- list(desc = describe(desc.dat),
              desc.dat = desc.dat,
              deg = deg)
  out
}
