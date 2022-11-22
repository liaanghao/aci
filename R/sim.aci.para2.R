#' ACI simulation test 2
#'
#' @param n.sim Number of simulation.
#' @param core
#' @param seed
#' @param n
#' @param belief
#' @param spatial.coord
#' @param target.outdegree
#' @param target.density
#' @param trial.limit
#' @param max.time
#' @param base.prob
#' @param homoph
#' @param recip
#' @param popul
#' @param activ
#' @param transiv
#' @param common.target
#' @param common.source
#' @param proximity
#' @param display.time
#' @param display.plot
#' @param sparseM.output
#'
#' @return
#' @export
#'
#' @examples
sim.aci.para2 <- function(n.sim,core, seed = NULL,
                         n,belief = 0,spatial.coord = NULL,
                         target.outdegree = NULL,target.density = NULL,
                         trial.limit = NULL,max.time = 10000,
                         base.prob = 0.05,homoph = 0,recip = 0,popul = 0,
                         activ = 0,transiv = 0,
                         common.target = 0,common.source = 0,proximity = 0,
                         display.time = FALSE,display.plot = FALSE,sparseM.output = FALSE){
  #set.seed
  if(is.null(seed)==FALSE){
    set.seed(seed = seed)
  }

  #set the number of core
  cl <- makeCluster(core)
  registerDoParallel(cl)

  #network simulation
  f1 <- foreach(i = 1:n.sim, .packages = 'aci') %dopar%{
    a <- create.network(mat.init=initiate.matrix(n),belief = belief,spatial.coord = spatial.coord,
                        target.outdegree = target.outdegree,target.density = target.density,
                        trial.limit = trial.limit, max.time = max.time,
                        base.prob = base.prob,homoph = homoph,recip = recip,popul = popul,
                        activ = activ,transiv = transiv,
                        common.target = common.target,common.source = common.source,proximity = proximity,
                        display.time = display.time,display.plot = display.plot,sparseM.output = sparseM.output)
  }

  #calculate ACI based on simulated network
  # network evaluation
  #list for final output
  ACI.Out <- vector(mode = "list", length = n.sim)
  #vectors for plots
  Whole.ACI <- rep(0, n.sim)
  Whole.cross <- rep(0, n.sim)
  Whole.homo <- rep(0, n.sim)
  CCH <- rep(0, n.sim)
  time <- rep(0, n.sim)

  for(m in 1:n.sim){
    ACI.Out[[m]] <- list(Simulated_Network = f1[[m]]$mat,
                         Parameters = f1[[m]],
                         ACI = aci(matrix = f1[[m]]$mat,
                                   policy.score = f1[[m]]$belief,
                                   alpha = 0.5),
                         Sim_Belief = f1[[m]]$belief,
                         Sim_Time = f1[[m]]$time)


    Whole.ACI[m] <- ACI.Out[[m]]$ACI$whole["whole.ACI"]
    Whole.cross[m] <- ACI.Out[[m]]$ACI$whole["whole.cross"]
    Whole.homo[m] <- ACI.Out[[m]]$ACI$whole["whole.homo"]
    CCH[m] <- ACI.Out[[m]]$ACI$whole["CCH"]
    time[m] <- ACI.Out[[m]]$Sim_Time
  }
  #stop parallel
  stopImplicitCluster()
  stopCluster(cl)

  par(family = "", mfrow=c(2,3))
  hist(Whole.ACI)
  hist(Whole.cross)
  hist(Whole.homo)
  hist(CCH)
  hist(time)
  return(ACI.Out)
}
