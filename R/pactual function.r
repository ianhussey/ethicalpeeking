#' Calculate pactual
#'
#' This function calculates pactual, a statistic discussed in Sagarin, Ambler, and Lee's (2014, Perspectives on Psychological Science, 9, 293-304) "An Ethical Approach to Peeking at Data".
#' @param ns ns refers to a vector of sample sizes that indicate the initial number of participants run (N1) followed by the number of participants in the first round of augmentation (N2), the number of participants in the second round of augmentation (N3), etc.
#' @param pmax refers to the maximum p-value in the data collected thusfar such that the researcher would augment the sample with additional participants.
#' @param pcrit refers to the value for determining statistical significance (typically .05).
#' @param slices refers to the number of slices to divide the probability distribution (higher numbers of slices will make the calculations slower but more precise).
#' @param tails refers to whether the tests are one- or two-tailed.
#' @param indent is used to format the countdown when this function is called from pcrit.
#' @keywords 
#' @export
#' @examples 
#' pactual(c(100,50,25), pmax = .1, pcrit = .05, slices = 1000, tails = 1)
#' pactual()

pactual <- function(ns,pmax=1,pcrit=.05,slices=1000,tails=2,indent="") {
  slicewidth <- 1/slices
  pslices <- seq(.5*(1/slices),1-.5*(1/slices),length=slices)
  oldweights <- rep(slicewidth,times=slices)
  naccumulator <- ns[1]
  paccumulator <- 0
  if (length(ns)>2) {
    for (i in 2:(length(ns)-1)) {
      newweights <- rep(0,times=slices)
      cat(indent, "Calculating augmentation round ", i-1, " of ", length(ns)-1, ":", sep = "")
      for (j in 1:slices) {
        if ((j-1)*10/slices==trunc((j-1)*10/slices)) {
          cat("", 10-(j-1)*10/slices)
        }
        if (((tails==1)&(pslices[j] > (1-pcrit)))|
            ((tails==2)&((pslices[j] > (1-pcrit/2))|(pslices[j] < (pcrit/2))))) {
          paccumulator <- paccumulator + oldweights[j]
        } else if (((tails==1)&(pslices[j] > (1-pmax)))|
                   ((tails==2)&((pslices[j] > (1-pmax/2))|(pslices[j] < (pmax/2))))) {
          for (k in 1:slices) {
            pcombined <- pnorm((naccumulator^.5*qnorm(pslices[j])+ns[i]^.5*qnorm(pslices[k]))/(naccumulator+ns[i])^.5)
            newweights[pcombined*slices+1] <- newweights[pcombined*slices+1] + oldweights[j]*slicewidth
          }
        }
      }
      naccumulator <- naccumulator + ns[i]
      oldweights <- newweights
      cat("\n")
    }
    cat(indent, "Calculating augmentation round ", length(ns)-1, " of ", length(ns)-1, ": 10 9 8 7 6 5 4 3 2 1\n", sep = "")
  }
  if (tails==1) {
    paccumulator <- paccumulator + sum(oldweights*((pslices>(1-pcrit))+
                                                     (pslices>(1-pmax))*(pslices<=(1-pcrit))*
                                                     (1-pnorm((qnorm(1-pcrit)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5))))
  } else {
    paccumulator <- paccumulator + sum(oldweights*((pslices>(1-pcrit/2))+(pslices<(pcrit/2))+
                                                     ((pslices>(1-pmax/2))*(pslices<=(1-pcrit/2))+(pslices<(pmax/2))*(pslices>=(pcrit/2)))*
                                                     (1-pnorm((qnorm(1-pcrit/2)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5)+
                                                        pnorm((-qnorm(1-pcrit/2)*(naccumulator+ns[length(ns)])^.5-naccumulator^.5*qnorm(pslices))/ns[length(ns)]^.5))))
  }
  return(paccumulator)
}