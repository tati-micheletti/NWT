waitingTime <- function(experimentName, 
                        mins = 30, 
                        calcul = TRUE){
  wt <- switch(EXPR = experimentName,
         I = 0,
         II = 1,
         III = 2,
         IV = 3,
         V = 4,
         VI = 5,
         VII = 6,
         VIII = 7,
         IX = 8,
         X = 9,
         XI = 10,
         XII = 11
  )
  print(paste0("Waking up in ",
               wt*mins, " minutes..."))
  if (calcul){
    Sys.sleep(wt*60*mins)
  } else {
    Sys.sleep(60*mins)
  }
}
