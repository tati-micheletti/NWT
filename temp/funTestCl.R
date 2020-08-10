funTestCl <- function(bird){
  print(ls())
  machine <- capture.output(system("hostname", wait = TRUE, intern = TRUE))
  paste0("Machine ", machine," PID ", paste0(Sys.getpid()), " bird: ", bird)
  return(NULL) # The predicted value is NOT multiplied by 1000! For that, need to change fitModel!
}