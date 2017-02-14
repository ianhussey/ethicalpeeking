#' Calculate pcrit
#'
#' This function calculates pcrit, a statistic discussed in Sagarin, Ambler, and Lee's (2014, Perspectives on Psychological Science, 9, 293-304) "An Ethical Approach to Peeking at Data".
#' @param ns refers to a vector of sample sizes that indicate the initial number of participants run (N1) followed by the number of participants in the first round of augmentation (N2), the number of participants in the second round of augmentation (N3), etc.
#' @param pmax refers to the maximum p-value in the data collected thusfar such that the researcher would augment the sample with additional participants.
#' @param pdesired refers to the desired Type I error rate (typically .05).
#' @param slices refers to the number of slices to divide the probability distribution (higher numbers of slices will make the calculations slower but more precise).
#' @param tails refers to whether the tests are one- or two-tailed.
#' @keywords 
#' @export
#' @examples 
#' pcrit(c(200,150,50), pmax = .2, pdesired = .05, slices = 1000, tails = 2)
#' pcrit()

pcrit <- function(ns,pmax=1,pdesired=.05,slices=1000,tails=2) {
  pcrit <- pdesired/2
  modification <- pcrit/2
  for(i in 1:15) {
    cat("Calculating adjustment iteration ", i, " of ", 15, "\n", sep = "")
    if (pactual(ns,pmax,pcrit,slices,tails,"  ")<pdesired) {
      pcrit <- pcrit+modification
    } else {
      pcrit <- pcrit-modification
    }
    modification <- modification/2
  }
  return(pcrit)
}