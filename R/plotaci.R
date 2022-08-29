aci_plot <- function(a, x, xlab, summary = FALSE){

  if(summary == TRUE){
    if(xlab != "ave.deg" | xlab != "density"){
      stop("xlab should be either average degree (ave.deg) or density (density)")
    }
    par(family = "HiraKakuPro-W3", mfrow=c(2,2))
    if(xlab == "ave.deg"){
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

    }else if (xlab == "density"){

      boxplot(eval_dat$ACI ~ eval_dat$density,
              ylab = "ACI", xlab = "Average degree",
              main = "密度ごとのACIのバラつき")
      boxplot(eval_dat$cross ~ eval_dat$density,
              ylab = "Cross tie score", xlab = "Average degree",
              main = "密度ごとのCross tie scoreのバラつき")
      boxplot(eval_dat$homo ~ eval_dat$density,
              ylab = "Homo tie score", xlab = "Average degree",
              main = "密度ごとのHomo tie scoreのバラつき")
      boxplot(eval_dat$CCH ~ eval_dat$density,
              ylab = "CCH", xlab = "Average degree",
              main = "密度ごとのCCHのバラつき")
    }
  }

}
