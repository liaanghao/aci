#' ACI simulation for multiple parameters at once.
#'
#' @param n.sim
#' @param n
#' @param Belief
#' @param Degree
#' @param base.prob
#' @param homoph
#' @param recip
#' @param popul
#' @param activ
#' @param transiv
#' @param common.target
#' @param common.source
#' @param display.time
#' @param display.plot
#' @param display.sim.time
#'
#' @return
#' @export
#'
#' @examples
sim.aci.multi <- function(n.sim, n, Belief, Degree, base.prob,
                    homoph = 0, recip = 0, popul = 0, activ = 0, transiv = 0,
                    common.target = 0, common.source = 0,
                    display.time = FALSE, display.plot = FALSE, display.sim.time = FALSE){

  if(class(Degree) != "list"){
    stop(message = "Provide a list of degrees for multiple simulations. OR use sim.aci for single simulation")
  }

  # network formation
  Sim_X.record  <- vector(mode ="list", length =  n.sim)
  Sim_Matrix  <- list()
  Sim_Belief <- list()
  Sim_Time   <- c()
  Sim_Degree   <- c()

  for(n in 1:length(Degree)){
    for(m in 1: n.sim){
      out <- create.network(
        n=n, Belief = Belief, Degree = Degree[[n]],
        base.prob = base.prob, homoph = homoph, recip = recip,
        popul = popul, activ = activ, common.target = common.target,
        common.source = common.source, display.time = display.time,
        display.plot = display.plot)

      Sim_Matrix[[n.sim*(n-1) + m]] <- out$mat
      Sim_Belief[[n.sim*(n-1) + m]] <- out$Belief
      Sim_Degree[[n.sim*(n-1) + m]] <- out$Degree
      Sim_Time[n.sim*(n-1) + m]     <- out$time
      Sim_X.record[[n.sim*(n-1) + m]] <- out$Record

      if(display.sim.time == TRUE){
        print(paste0("current simulation: ", m, " (",round(m/N.Sim*100,1),"%)"))
      }
    }
  }

  #network evaluation
  ACI.Out <- list()
  Whole.ACI <- c()
  Average.Degree <- c()
  for(m in 1:length(Sim_Matrix)){
    Average.Degree[m] <-  mean(rowSums(Sim_Matrix[[m]]))
    ACI.Out[[m]] <- aci(matrix = Sim_Matrix[[m]],
                        policy.score = Sim_Belief[[m]],
                        cut.value = 0.5)

    Whole.ACI[m] <- ACI.Out[[m]]$whole["whole.ACI"]
    Whole.cross[m] <- ACI.Out[[m]]$ACI$whole["whole.cross"]
    Whole.homo[m] <- ACI.Out[[m]]$ACI$whole["whole.homo"]
    CCH[m] <- ACI.Out[[m]]$ACI$whole["CCH"]
    time[m] <- ACI.Out[[m]]$Sim_Time

  }

  eval_dat <- data.frame(
    avedeg = Average.Degree,
    ACI      = Whole.ACI,
    cross = Whole.cross,
    homo = Whole.homo,
    cch = CCH,
    time = time
  )

  plotmulti(eval_dat)
  invisible(eval_dat)
}
