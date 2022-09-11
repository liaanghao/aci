#' Visual comparison among multiple simulation
#'
#' @param eval_dat evaluation data
#' @param x x index
#' @param xlab xlab
#'
#' @return
#' @export
#'
#' @examples
plotmulti <- function(eval_dat, x, xlab){

  par(family = "HiraKakuPro-W3", mfrow=c(2,3))
  boxplot(eval_dat$ACI ~ x,
          ylab = "ACI", xlab = xlab,
          main = paste0(xlab, "ごとのACIのバラつき"))
  boxplot(eval_dat$cross ~ x,
          ylab = "Cross tie score", xlab = xlab,
          main = paste0(xlab, "ごとのCross tie scoreのバラつき"))
  boxplot(eval_dat$homo ~ x,
          ylab = "Homo tie score", xlab = xlab,
          main = paste0(xlab,"ごとのHomo tie scoreのバラつき"))
  boxplot(eval_dat$CCH ~ x,
          ylab = "CCH", xlab = xlab,
          main = paste0(xlab,"ごとのCCHのバラつき"))
  boxplot(eval_dat$time ~ x,
          ylab = "Time", xlab = xlab,
          main = paste0(xlab, "ごとのTimeのバラつき"))
}
