# ethicalpeeking

This library provides an easy way to use the statistics discussed in Sagarin, Ambler, and Lee's (2014, *Perspectives on Psychological Science, 9*, 293-304) "An Ethical Approach to Peeking at Data". See http://www.paugmented.com/

## License

GPLv3+

## Author

Library by Ian Hussey (ian.hussey@ugent.be)

All the real, hard work (conceptualisation of paugmented, writing of functions, original article) by Brad J. Sagarin (bsagarin@niu.edu), James K. Ambler, and Ellen M. Lee.

## Abstract from the original article

"When data analyses produce encouraging but nonsignificant results, researchers often respond by collecting more data. This may transform a disappointing dataset into a publishable study, but it does so at the cost of increasing the Type I error rate. How big of a problem is this, and what can we do about it? To answer the first question, we estimate the Type I error inflation based on the initial sample size, the number of participants used to augment the dataset, the critical value for determining significance (typically .05), and the maximum p value within the initial sample such that the dataset would be augmented. With one round of augmentation, Type I error inflation maximizes at .0975 with typical values from .0564 to .0883. To answer the second question, we review methods of adjusting the critical value to allow augmentation while maintaining p < .05, but we note that such methods must be applied a priori. For the common occurrence of post-hoc dataset augmentation, we develop a new statistic, paugmented, that represents the magnitude of the resulting Type I error inflation. We argue that the disclosure of post-hoc dataset augmentation via p augmented elevates such augmentation from a questionable research practice to an ethical research decision."

## Install guide for R/Rstudio

```R
# devtools allows you to install packages from github
install.packages("devtools")
library("devtools")

# install package
install_github("ianhussey/ethicalpeeking")
library("ethicalpeeking")

# view documentation
?paugmented
```