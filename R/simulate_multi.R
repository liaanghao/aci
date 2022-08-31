#' Summary
#'
#' @param list a list of object
#'
#' @return
#' @export
#'
#' @examples
multisum <- function(list){

  sa <- c(list)

  whole.aci <- c()
  whole.cross <- c()
  whole.homo <- c()
  CCH <- c()
  avedeg <- c()
  time <- c()

    for(m in 1:length(sa)){
      avedeg[m] <- mean(rowSums(sa[[m]]$Sim_Matrix))
      time[m] <- sa[[m]]$Sim_Time
      whole.aci[m] <- sa[[m]]$ACI$whole["whole.ACI"]
      whole.cross[m] <- sa[[m]]$ACI$whole["whole.cross"]
      whole.homo[m] <- sa[[m]]$ACI$whole["whole.homo"]
      CCH[m] <- sa[[m]]$ACI$whole["CCH"]
    }


  eval_dat <- data.frame(
    avedeg = avedeg,
    ACI = whole.aci,
    cross = whole.cross,
    homo = whole.homo,
    CCH = CCH,
    time = time
  )

  plotmulti(eval_dat)
  invisible(eval_dat)
}
