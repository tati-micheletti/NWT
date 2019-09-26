tTestMeansSD <- function(mean1, mean2, sd1, sd2, 
                          sampleSize1, sampleSize2, m0 = 0, 
                          equal.variance = FALSE){
  # FROM Macro: https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
  # mean1, mean2: the sample means
  # sd1, sd2: the sample standard deviations
  # sampleSize1, sampleSize2: the same sizes
  # m0: the null value for the difference in means to be tested for. Default is 0. 
  # equal.variance: whether or not to assume equal variance. Default is FALSE. 
  
  if(equal.variance == FALSE ){
    se <- sqrt( (sd1^2/sampleSize1) + (sd2^2/sampleSize2) )
    # welch-satterthwaite df
    df <- ( (sd1^2/sampleSize1 + sd2^2/sampleSize2)^2 )/( (sd1^2/sampleSize1)^2/(sampleSize1-1) + (sd2^2/sampleSize2)^2/(sampleSize2-1) )
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/sampleSize1 + 1/sampleSize2) * ((sampleSize1-1)*sd1^2 + (sampleSize2-1)*sd2^2)/(sampleSize1+sampleSize2-2) ) 
    df <- sampleSize1 + sampleSize2 - 2
  }      
  t <- (mean1 - mean2 - m0)/se 
  dat <- c(mean1-mean2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("DifferenceOfMeans", "stdError", "t", "pValue")
  return(dat) 
}