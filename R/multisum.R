#' summary of multiple simulation results
#'
#' @param list A list of simulated ACI objects.
#'
#' @return An evaluation dataframe with ACI parameters and variables used for simulation.
#' @export
#'
#' @examples
multisum <- function(list){

  #summary the simulation list
  sa <- c(list)

  #output variables
    whole.aci <- c()
    whole.cross <- c()
    whole.homo <- c()
    CCH <- c()
    avedeg <- c()
    time <- c()
    homo <- c()
    homoph <- c()
    base.prob <- c()
    recip <- c()
    popul <- c()
    activ <- c()
    transiv <- c()
    common.target <- c()
    common.source <- c()
    n <- c()
    n.sim <- c()

    for (m in 1:length(sa)) {
      avedeg[m] <- mean(rowSums(sa[[m]]$Simulated_Network))
      time[m] <- sa[[m]]$Sim_Time
      whole.aci[m] <- sa[[m]]$ACI$whole["whole.ACI"]
      whole.cross[m] <- sa[[m]]$ACI$whole["whole.cross"]
      whole.homo[m] <- sa[[m]]$ACI$whole["whole.homo"]
      CCH[m] <- sa[[m]]$ACI$whole["CCH"]
      homoph[m] <- sa[[m]]$simulation_result$parameters["homoph"]
      base.prob[m] <- sa[[m]]$simulation_result$parameters["base.prob"]
      recip[m] <- sa[[m]]$simulation_result$parameters["recip"]
      popul[m] <- sa[[m]]$simulation_result$parameters["popul"]
      activ[m] <- sa[[m]]$simulation_result$parameters["activ"]
      transiv[m] <- sa[[m]]$simulation_result$parameters["transiv"]
      common.target[m] <- sa[[m]]$simulation_result$parameters["common.target"]
      common.source[m] <- sa[[m]]$simulation_result$parameters["common.source"]
      n[m] <- sa[[m]]$n
      n.sim[m] <- sa[[m]]$n.sim
    }
    eval_dat <- data.frame(n = n, n.sim = n.sim, avedeg = avedeg,
                           ACI = whole.aci, cross = whole.cross, homo = whole.homo,
                           CCH = CCH, time = time, homoph = homoph, base.prob = base.prob,
                           recip = recip, popul = popul, activ = activ, transiv = transiv,
                           common.target = common.target, common.source = common.source)
    invisible(eval_dat)
  }
