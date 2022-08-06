#' Summary of multinational object
#'
#' @param out      An output object of multinom in nnet library.
#' @param rounding If the result is rounded (Default: TRUE).
#' @param digits   digit used for rounding.
#' @param se.in.parenthesis If the standard error is shown in parenthesis.
#'
#' @return Reference Reference category in dependent variable.
#' @return coef Result with the regression coefficients, standard errors and the p-value indicator.
#' @return fit  Fit measures of the regression.
#' @example
#' library(nnet)
#' library(MASS)
#' data(birthwt)
#' birthwt$race <- factor(birthwt$race, labels = c("white", "black", "other"))
#' out <- multinom(race ~ ., birthwt)
#' multinom.summary(out)

multinom.summary <- function(out,
                             rounding = TRUE,
                             digits = 2,
                             se.in.parenthesis = FALSE){
  # define the p.eval function:
  p.eval <- function(Z){
    Z.abs <- abs(Z)
    out <-
      ifelse(Z.abs > qnorm(0.999+0.001/2,0,1),
             "***",
             ifelse(Z.abs > qnorm(0.99+0.01/2,0,1),
                    "**",
                    ifelse(Z.abs > qnorm(0.95+0.05/2,0,1),
                           "*", ifelse(Z.abs > qnorm(0.90+0.10/2,0,1),
                                       ".",
                                       ""))))
    return(out)
  }

  # values
  Coef <- t(coef(out))
  SE   <- sqrt(diag(vcov(out)))
  Z    <- Coef/SE
  Z.eval <- matrix(NA, nrow(Z), ncol(Z), byrow = T)
  for(i in 1:nrow(Z)){for(j in 1:ncol(Z)){
    Z.eval[i,j] <- p.eval(Z[i,j])
  }}
  Ref <- out$lev[1]

  # calculate the margin
  y.level <- length(out$lev)-1
  n.param <- nrow(Coef)
  coef.col <- seq(1,2*y.level, by = 2)
  p.col <- seq(2,2*y.level, by = 2)
  coef.row <- seq(1, n.param*2, by = 2)
  se.row   <- seq(2, n.param*2, by = 2)

  # change the expression as per settings
  if(rounding == T){
    Coef <- round(Coef, digits = digits)
    SE   <- round(SE, digits = digits)
  }
  if(se.in.parenthesis==T){
    SE <- paste0("(", format(SE, digits = digits), ")")
  }

  # summarize the values into the table
  result.table <- matrix(NA, n.param*2, y.level*2, byrow = T)
  result.table <- as.data.frame(result.table)

  result.table[coef.row, coef.col] <- Coef
  result.table[coef.row, p.col] <- Z.eval
  result.table[se.row, coef.col] <- SE
  result.table[is.na(result.table)] <- ""

  # put row and colnames
  colnames(result.table)[coef.col] <- colnames(Coef)
  colnames(result.table)[p.col] <- ""

  # add a column with variable names
  valnames <- rep(NA, 2*n.param)
  valnames[coef.row] <- rownames(Coef)
  valnames[se.row] <- ""
  result.table2 <- cbind.data.frame(valnames, result.table)
  colnames(result.table2) <- c("",colnames(result.table))

  # Fit measures
  AIC <- out$AIC
  Deviance <- out$deviance
  LL <- Deviance*(-1/2)
  Obs <- length(out$fitted.values)/(y.level+1)
  K   <- y.level+1
  fit <- data.frame(AIC = AIC,
                    Deviance = Deviance,
                    LL = LL,
                    Obs = Obs,
                    K = K)

  # リターン
  output <- list(Reference = Ref,
                 coef = result.table2,
                 fit = t(fit))
  return(output)
}
