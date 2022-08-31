#' Visual comparison among multiple simulation
#'
#' @param eval_dat evaluation data
#'
#' @return
#' @export
#'
#' @examples
plotmulti <- function(eval_dat){

  par(family = "HiraKakuPro-W3", mfrow=c(2,3))
  boxplot(eval_dat$ACI ~ eval_dat$avedeg,
          ylab = "ACI", xlab = "Average degree",
          main = "平均出次数ごとのACIのバラつき")
  boxplot(eval_dat$cross ~ eval_dat$avedeg,
          ylab = "Cross tie score", xlab = "Average degree",
          main = "平均出次数ごとのCross tie scoreのバラつき")
  boxplot(eval_dat$homo ~ eval_dat$avedeg,
          ylab = "Homo tie score", xlab = "Average degree",
          main = "平均出次数ごとのHomo tie scoreのバラつき")
  boxplot(eval_dat$CCH ~ eval_dat$avedeg,
          ylab = "CCH", xlab = "Average degree",
          main = "平均出次数ごとのCCHのバラつき")
  boxplot(eval_dat$time ~ eval_dat$avedeg,
          ylab = "Time", xlab = "Average degree",
          main = "平均出次数ごとのTimeのバラつき")
}
