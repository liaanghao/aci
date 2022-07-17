#' Simulation: ACI simulation based on random level of average degree.
#'
#' @param nsim The number of simulation.
#' @param actor The number of nodes.
#' @param belief The belief score.
#' @param degree A \code{numeric} object of average degree used or
#' a \code{list} object of multiple average degrees used.
#' @param plot Only applies when \code{degree} is a \code{list} object.
#'
#' @return
#' @export
#'
#' @examples
#' @author Hao Liang \url{hl893@cornell.edu},
#' Keiichi Satoh \url{keiichi.satoh@r.hit-u.ac.jp}
sim.degree <- function(nsim, actor, belief, degree, plot = FALSE){

  # set seed
  set.seed(666)

  #network formation
  Sim_Matrix  <- list()
  Sim_Belief <- list()
  Sim_Time   <- c()

  # check
  if(class(degree) == "numeric"){

    for(m in 1:nsim){
      out <- create.network(N = actor, Belief = belief, Degree = degree)
      Sim_Matrix[[m]] <- out$mat
      Sim_Belief[[m]] <- out$Belief
      Sim_Time[m]     <- out$time
    }

    #network evaluation
    ACI.Out <- list()
    Whole.ACI <- c()
    for(m in 1:N.Sim){
      ACI.Out[[m]] <- aci(matrix = Sim_Matrix[[m]],
                          policy.score = Sim_Belief[[m]],
                          alpha = 0.5)
      Whole.ACI[m] <- ACI.Out[[m]]$whole["whole.ACI"]
    }

    #return
    invisible(ACI.Out)
    return(head(Whole.ACI))
    hist(Whole.ACI)

  }else if(class(degree) == "list"){

    #network formation for lists
    Degree.pattern <- length(degree)
    Sim_Degree   <- c()

    for(n in 1:Degree.pattern){
      for(m in 1:nsim){
        out <- create.network(N = actor, Belief = belief, Degree = degree[[n]])
        Sim_Matrix[[nsim*(n-1) + m]] <- out$mat
        Sim_Belief[[nsim*(n-1) + m]] <- out$Belief
        Sim_Degree[[nsim*(n-1) + m]] <- out$Degree
        Sim_Time[nsim*(n-1) + m]     <- out$time
      }
    }

    # network evaluation
    ACI.Out <- list()
    Whole.ACI <- c()
    Average.Degree <- c()
    whole.cross <- c()
    whole.homo <- c()
    CCH <- c()
    time <- c()
    for(m in 1:length(Sim_Matrix)){
      Average.Degree[m] <-  mean(rowSums(Sim_Matrix[[m]]))
      ACI.Out[[m]] <- aci(matrix = Sim_Matrix[[m]],
                          policy.score = Sim_Belief[[m]],
                          alpha = 0.5)
      Whole.ACI[m] <- ACI.Out[[m]]$whole["whole.ACI"]
      whole.cross[m] <- ACI.Out[[m]]$whole["whole.cross"]
      whole.homo[m] <- ACI.Out[[m]]$whole["whole.homo"]
      CCH[m] <- ACI.Out[[m]]$whole["CCH"]
      time[m] <- Sim_Time[[m]]
    }

    eval_dat <- data.frame(
      Average.Degree = Average.Degree,
      ACI = Whole.ACI,
      cross = whole.cross,
      homo = whole.homo,
      CCH = CCH,
      time = time
    )
    m <- 1
    ACI.Out[[m]]$whole
    eval_dat$ACI

    if(plot == TRUE){
      # plot
      par(family = "HiraKakuPro-W3", mfrow=c(2,3))
      boxplot(eval_dat$ACI ~ eval_dat$Average.Degree,
              ylab = "ACI", xlab = "Average degree",
              main = "平均出次数ごとのACIのバラつき")
      boxplot(eval_dat$cross ~ eval_dat$Average.Degree,
              ylab = "ACI", xlab = "Average degree",
              main = "平均出次数ごとのCross tie scoreのバラつき")
      boxplot(eval_dat$homo ~ eval_dat$Average.Degree,
              ylab = "ACI", xlab = "Average degree",
              main = "平均出次数ごとのHomo tie scoreのバラつき")
      boxplot(eval_dat$CCH ~ eval_dat$Average.Degree,
              ylab = "ACI", xlab = "Average degree",
              main = "平均出次数ごとのCCHのバラつき")
      boxplot(eval_dat$time ~ eval_dat$Average.Degree,
              ylab = "ACI", xlab = "Average degree",
              main = "平均出次数ごとのTimeのバラつき")
    }
    #return
    return(eval_dat)
  }}
