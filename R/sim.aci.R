#' ACI simulation: single
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
#' @param display.time see \code{give.attra.score}.
#' @param display.plot see \code{give.attra.score}.
#' @param display.sim.time see \code{give.attra.score}.
#'
#' @return
#' @export
#'
#' @examples
sim.aci <- function(n.sim, n, Belief, Degree, base.prob,
                    homoph, recip, popul, activ, transiv,
                    common.target, common.source,
                    display.time = FALSE, display.plot = TRUE, display.sim.time = TRUE){

  # network formation
  Sim_Matrix  <- array(0, dim = c(n, n, n.Sim))
  Sim_Belief <- matrix(0, n.sim, length(Belief), byrow = T)
  Sim_Time   <- rep(0, n.sim)
  Sim_X.record  <- vector(mode ="list", length = n.sim)

  for(m in 1:N.Sim){
    out <- create.network(
      n = n, Belief = Belief, Degree = Degree,
      base.prob = base.prob, homoph = homoph, recip = recip,
      popul = popul, activ = activ, common.target = common.target,
      common.source = common.source, display.time = display.time,
      display.plot = display.plot)

    Sim_Matrix[ , ,m] <- out$mat
    Sim_Belief[m, ] <- out$Belief
    Sim_Time[m]     <- out$time
    Sim_X.record[[m]] <- out$Record

    if(display.sim.time == TRUE){
      print(paste0("current simulation: ", m, " (",round(m/N.Sim*100,1),"%)"))
    }
  }

  # network evaluation
  ACI.Out <- vector(mode = "list", length = n.sim)
  Whole.ACI <- rep(0, n.sim)
  for(m in 1:n.sim){
    ACI.Out[[m]] <- aci(matrix = Sim_Matrix[,,m],
                        policy.score = Sim_Belief[m, ],
                        alpha = 0.5)
    Whole.ACI[m] <- ACI.Out[[m]]$whole["whole.ACI"]
  }

  hist(Whole.ACI)

}
