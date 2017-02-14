#' Calculate paugmented
#'
#' This function calculates paugmented, a statistic discussed in Sagarin, Ambler, and Lee's (2014, Perspectives on Psychological Science, 9, 293-304) "An Ethical Approach to Peeking at Data".
#' @param ns refers to a vector of sample sizes that indicate the initial number of participants run (N1) followed by the number of participants in the first round of augmentation (N2), the number of participants in the second round of augmentation (N3), etc.
#' @param plargest refers to the largest p-value obtained in the initial or augmented subsamples. For example, if the p-value for the initial N1 participants was .15, the p-value for the combined N1+N2 participants was .18, and the p-value for the combined N1+N2+N3 participants was .04, plargest would be .18.
#' @param pfinal refers to the p-value obtained in the final sample that includes all participants.
#' @param pcrit refers to the value for determining statistical significance (typically .05).
#' @param slices refers to the number of slices to divide the probability distribution (higher numbers of slices will make the calculations slower but more precise).
#' @param tails refers to whether the tests are one- or two-tailed.
#' @keywords 
#' @export
#' @examples 
#' paugmented(c(40,40,20), plargest = .18, pfinal = .04, pcrit = .05, slices = 1000, tails = 2)
#' paugmented()

paugmented <- function(ns, plargest, pfinal, pcrit=.05, slices=1000, tails=2) {
  slicewidth <- 1/slices
  pslices <- seq(.5*(1/slices),1-.5*(1/slices),length=slices)
  lowboundoldweights <- rep(slicewidth,times=slices)
  highboundoldweights <- rep(slicewidth,times=slices)
  naccumulator <- ns[1]
  lowboundpaccumulator <- 0
  highboundpaccumulator <- 0
  if (length(ns)>2) {
    for (i in 2:(length(ns)-1)) {
      lowboundnewweights <- rep(0,times=slices)
      highboundnewweights <- rep(0,times=slices)
      cat("Calculating augmentation round ", i-1, " of ", length(ns)-1, ":", sep = "")
      for (j in 1:slices) {
        if ((j-1)*10/slices==trunc((j-1)*10/slices)) {
          cat("", 10-(j-1)*10/slices)
        }
        if (((tails==1)&(pslices[j] > (1-pcrit)))|
            ((tails==2)&((pslices[j] > (1-pcrit/2))|(pslices[j] < (pcrit/2))))) {
          lowboundpaccumulator <- lowboundpaccumulator + lowboundoldweights[j]
          highboundpaccumulator <- highboundpaccumulator + highboundoldweights[j]
        } else if (((tails==1)&(pslices[j] > (1-plargest)))|
                   ((tails==2)&((pslices[j] > (1-plargest/2))|(pslices[j] < (plargest/2))))) {
          for (k in 1:slices) {
            pcombined <- pnorm((naccumulator^.5*qnorm(pslices[j])+ns[i]^.5*qnorm(pslices[k]))/(naccumulator+ns[i])^.5)
            lowboundnewweights[pcombined*slices+1] <- lowboundnewweights[pcombined*slices+1] + lowboundoldweights[j]*slicewidth
            highboundnewweights[pcombined*slices+1] <- highboundnewweights[pcombined*slices+1] + highboundoldweights[j]*slicewidth
          }
        } else {
          for (k in 1:slices) {
            pcombined <- pnorm((naccumulator^.5*qnorm(pslices[j])+ns[i]^.5*qnorm(pslices[k]))/(naccumulator+ns[i])^.5)
            highboundnewweights[pcombined*slices+1] <- highboundnewweights[pcombined*slices+1] + highboundoldweights[j]*slicewidth
          }
        }
      }
      naccumulator <- naccumulator + ns[i]
      lowboundoldweights <- lowboundnewweights
      highboundoldweights <- highboundnewweights
      cat("\n")
    }
    cat("Calculating augmentation round ", length(ns)-1, " of ", length(ns)-1, ": 10 9 8 7 6 5 4 3 2 1\n", sep = "")
  }
  if (tails==1) {
    lowboundpaccumulator <- lowboundpaccumulator + sum(lowboundoldweights*((pslices>(1-pcrit))+
                                                                             (pslices>(1-plargest))*(pslices<=(1-pcrit))*
                                                                             (1-pnorm((qnorm(1-pfinal)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5))))
    highboundpaccumulator <- highboundpaccumulator + sum(highboundoldweights*((pslices>(1-pcrit))+
                                                                                (pslices>(1-1))*(pslices<=(1-pcrit))*
                                                                                (1-pnorm((qnorm(1-pfinal)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5))))
  } else {
    lowboundpaccumulator <- lowboundpaccumulator + sum(lowboundoldweights*((pslices>(1-pcrit/2))+(pslices<(pcrit/2))+
                                                                             ((pslices>(1-plargest/2))*(pslices<=(1-pcrit/2))+(pslices<(plargest/2))*(pslices>=(pcrit/2)))*
                                                                             (1-pnorm((qnorm(1-pfinal/2)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5)+
                                                                                pnorm((-qnorm(1-pfinal/2)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5))))
    highboundpaccumulator <- highboundpaccumulator + sum(highboundoldweights*((pslices>(1-pcrit/2))+(pslices<(pcrit/2))+
                                                                                ((pslices>(1-1/2))*(pslices<=(1-pcrit/2))+(pslices<(1/2))*(pslices>=(pcrit/2)))*
                                                                                (1-pnorm((qnorm(1-pfinal/2)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5)+
                                                                                   pnorm((-qnorm(1-pfinal/2)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5))))
  }
  return(paste("[",lowboundpaccumulator,", ",highboundpaccumulator,"]", sep=""))
}