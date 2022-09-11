#' Summary
#'
#' @param list a list of object
#' @param x see \code{plotmulti}.
#' @param xlab see \code{plotmulti}.
#'
#' @return
#' @export
#'
#' @examples
multisum <- function(list, x, xlab){

  sa <- c(list)

  whole.aci <- c()
  whole.cross <- c()
  whole.homo <- c()
  CCH <- c()
  avedeg <- c()
  time <- c()
  homo <- c()
  homoph <- c()
  base.prob <- c()
  recip  <- c()
  popul  <- c()
  activ  <- c()
  transiv  <- c()
  common.target <- c()
  common.source <- c()
  n <- c()
  n.sim <- c()

    for(m in 1:length(sa)){
      avedeg[m] <- mean(rowSums(sa[[m]]$Sim_Matrix))
      time[m] <- sa[[m]]$Sim_Time
      whole.aci[m] <- sa[[m]]$ACI$whole["whole.ACI"]
      whole.cross[m] <- sa[[m]]$ACI$whole["whole.cross"]
      whole.homo[m] <- sa[[m]]$ACI$whole["whole.homo"]
      CCH[m] <- sa[[m]]$ACI$whole["CCH"]
      homoph[m] <- sa[[m]]$Parameters["homoph"]
      base.prob[m] <- sa[[m]]$Parameters["base.prob"]
      recip[m]  <- sa[[m]]$Parameters["recip"]
      popul[m]  <- sa[[m]]$Parameters["popul"]
      activ[m] <- sa[[m]]$Parameters["activ"]
      transiv[m]  <- sa[[m]]$Parameters["transiv"]
      common.target[m] <- sa[[m]]$Parameters["common.target"]
      common.source[m] <- sa[[m]]$Parameters["common.source"]
      n[m] <- sa[[m]]$Parameters["n"]
      n.sim[m] <- sa[[m]]$Parameters["n.sim"]
    }


  eval_dat <- data.frame(
    n = n,
    n.sim = n.sim,
    avedeg = avedeg,
    ACI = whole.aci,
    cross = whole.cross,
    homo = whole.homo,
    CCH = CCH,
    time = time,
    homoph = homoph,
    base.prob = base.prob,
    recip  = recip,
    popul  = popul,
    activ  = activ,
    transiv  = transiv,
    common.target = common.target,
    common.source = common.source
  )
  invisible(eval_dat)
}
