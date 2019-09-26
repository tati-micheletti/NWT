calculatePvalueOfRasters <- function(dtForTest, pixelBased = FALSE, sampleSize, species, redFactorTimes, bird){
  if (pixelBased){
    if (!is.null(sampleSize)){
      if (all(sampleSize == "auto", bird == species[[1]])){ # NEEDS TO HAPPEN ONLY FOR THE FIRST BIRD SPECIES AND STAY
        # COMPUTE IDEAL SIZE SAMPLE FOR SAMPLING USING Cohen's D And Hedges G Effect Size
        reproducible::Require("effsize")
        library("effsize")
        treatment <- dtForTest[[2]][!is.na(dtForTest[[2]])]
        control <- dtForTest[[1]][!is.na(dtForTest[[1]])]
        
        ## data and factor
        cohen <- cohen.d(treatment, control, paired = TRUE, 
                         conf.level = 0.01, hedges.correction = TRUE)
        limit <- 0.2 
        # Effect size	d	Reference
        # Very small	0.01	Sawilowsky, 2009
        # Small	      0.20	Cohen, 1988
        # Medium	    0.50	Cohen, 1988
        # Large	      0.80	Cohen, 1988
        # Very large  1.20	Sawilowsky, 2009
        # Huge	      2.0	Sawilowsky, 2009
        vectorSize <- length(treatment)
        message(crayon::yellow("Calculating ideal sample size using Cohen's D and Hedges'g effect size statistics..."))
        while (cohen$estimate < limit){
          newSample <- sample(x = 1:NROW(treatment), 
                              size = vectorSize, 
                              replace = FALSE)
          cohen <- cohen.d(treatment[newSample], control[newSample], paired = TRUE, 
                           conf.level = 0.01, hedges.correction = TRUE)
          vectorSize <- vectorSize - round(vectorSize/redFactorTimes, 0)
        }
        sampleSize <- length(newSample)
        message(crayon::green(paste0("Ideal sample size calculated as ", 
                                     sampleSize, "\n with sample size effect calcutated as")))
        print(cohen)
      }
      # SAMPLE FROM TABLE AND RUN THE TEST
      dtForTest$ID <- 1:NROW(dtForTest)
      env <- environment()
      if (!exists("sbsetID")){
        if (tryCatch({
          pryr::where(name = "sbsetID", env = env) 
          return(FALSE)}, 
          error = function(e) return(TRUE))){
          assign(x = "sbsetID", value = sample(x = dtForTest[!is.na(dtForTest)[ID]]$ID, size = sampleSize, replace = FALSE), 
                 envir = env)
        } else {
          sbsetID <- get("sbsetID", envir = pryr::where(name = "sbsetID", env = env))
        }
      }
      dtForTest <- dtForTest[sbsetID,]
    }
    test <- wilcox.test(x = dtForTest[[1]], y = dtForTest[[2]], paired = TRUE, alternative = "two.sided")
    p.value <- test$p.value
    mean1 <- mean(dtForTest[[1]], na.rm = TRUE)
    mean2 <- mean(dtForTest[[2]], na.rm = TRUE)
  } else {
    mean1 <- mean(dtForTest[[1]], na.rm = TRUE)
    sd1 <- sd(dtForTest[[1]], na.rm = TRUE)
    sampleSize1 <- sum(!is.na(dtForTest[[1]]))
    mean2 <- mean(dtForTest[[2]], na.rm = TRUE)
    sd2 <- sd(dtForTest[[2]], na.rm = TRUE)
    sampleSize2 <- sum(!is.na(dtForTest[[2]]))
    source('/mnt/data/Micheletti/NWT/functions/tTestMeansSD.R') # add to usefun [ FIX ]
    test <- tTestMeansSD(mean1 = mean1, mean2 = mean2, sd1 = sd1, sd2 = sd2, 
                         sampleSize1 = sampleSize1, sampleSize2 = sampleSize2)
    
    p.value <- test["pValue"]
  }
  return(list(mean1 = mean1, mean2 = mean2, p.value = p.value))
}