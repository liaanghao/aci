#' Agent based Network simulation using multicore
#'
#' @param n.sim Number of simulation.
#' @param n Number of nodes.
#' @param Belief see \code{give.attra.score}.
#' @param Degree see \code{give.attra.score}.
#' @param base.prob see \code{give.attra.score}.
#' @param homoph see \code{give.attra.score}.
#' @param recip see \code{give.attra.score}.
#' @param popul see \code{give.attra.score}.
#' @param activ see \code{give.attra.score}.
#' @param transiv see \code{give.attra.score}.
#' @param common.target see \code{give.attra.score}.
#' @param common.source see \code{give.attra.score}.
#' @param plotfamily Define a family of fonts for plot.
#' @param display.time see \code{give.attra.score}.
#' @param core The number of core for simulation.
#'
#' @return
#' @export
#' @import foreach
#' @import doParallel
#'
#' @examples
sim.aci.para <- function(n.sim, n, Belief, Degree, base.prob, core,
                         homoph = 0, recip = 0, popul = 0, activ = 0, transiv = 0,
                         common.target = 0, common.source = 0,
                         display.time = FALSE, plotfamily = NA){
  #set the number of core
  cl <- makeCluster(core)
  registerDoParallel(cl)

  ##network formation
  Sim_Matrix  <- array(0, dim = c(n, n, n.sim))
  Sim_Belief <- matrix(0, n.sim, length(Belief), byrow = T)
  Sim_Time   <- rep(0, n.sim)
  Sim_X.record  <- vector(mode ="list", length =  n.sim)

  #network simulation using multicore
  foreach1 <- foreach(m = 1:n.sim, .packages = 'aci') %dopar% {
    out <- create.network(n= n, Belief = Belief, Degree = Degree,
                          base.prob = base.prob, homoph = homoph, recip = recip,
                          popul = popul, activ = activ, common.target = common.target,
                          common.source = common.source, display.time = display.time)
  }

  #assign network properties
  for(m in 1:n.sim){
    Sim_Matrix[ , ,m] <- foreach1[[m]]$mat
    Sim_Belief[m, ] <- foreach1[[m]]$Belief
    Sim_Time[m]     <- foreach1[[m]]$time
    Sim_X.record[[m]] <- foreach1[[m]]$Record
  }

  # network evaluation
  ACI.Out <- vector(mode = "list", length = n.sim)
  Whole.ACI <- rep(0, n.sim)
  Whole.cross <- rep(0, n.sim)
  Whole.homo <- rep(0, n.sim)
  CCH <- rep(0, n.sim)
  time <- rep(0, n.sim)

  for(m in 1:n.sim){
    ACI.Out[[m]] <- list(Parameters = c(n = n, n.sim = n.sim, base.prob = base.prob,
                                        homoph = homoph, recip = recip, popul = popul,
                                        activ = activ, transiv = transiv,
                                        common.target = common.target,
                                        common.source = common.source),
                         ACI = aci(matrix = Sim_Matrix[,,m],
                                   policy.score = Sim_Belief[m, ],
                                   alpha = 0.5),
                         Sim_Matrix = Sim_Matrix[ , ,m],
                         Sim_Belief = Sim_Belief[m, ],
                         Sim_Time = Sim_Time[m],
                         Sim_X.record = Sim_X.record[[m]])


    Whole.ACI[m] <- ACI.Out[[m]]$ACI$whole["whole.ACI"]
    Whole.cross[m] <- ACI.Out[[m]]$ACI$whole["whole.cross"]
    Whole.homo[m] <- ACI.Out[[m]]$ACI$whole["whole.homo"]
    CCH[m] <- ACI.Out[[m]]$ACI$whole["CCH"]
    time[m] <- ACI.Out[[m]]$Sim_Time
  }

  #stop parallel
  stopImplicitCluster()
  stopCluster(cl)

  par(family = plotfamily, mfrow=c(2,3))
  hist(Whole.ACI)
  hist(Whole.cross)
  hist(Whole.homo)
  hist(CCH)
  hist(time)
  invisible(ACI.Out)
}
