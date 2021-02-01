getAnnualClimateZipURL <- function(scenario){
avail <-  c(
  # "CCSM4_85", 
  "CCSM4_RCP85",
  # "CCSM4_45", 
    "ACCESS1-0_RCP85", 
    "CanESM2_RCP85",
    "CSIRO-Mk3-6-0_RCP85",
    "INM-CM4_RCP85",
    "CNRM-CM5_RCP85")
  library("data.table")
if (!scenario %in% avail)
  stop(paste0("Climate scenario ", scenario, " is not available"))
  DT <- data.table(climateScenario = c("CCSM4_RCP85",
                                       # "CCSM4_85", 
                                       # "CCSM4_45", 
                                       "ACCESS1-0_RCP85", 
                                       "CanESM2_RCP85",
                                       "CSIRO-Mk3-6-0_RCP85",
                                       "INM-CM4_RCP85",
                                       "CNRM-CM5_RCP85"),
                   URL = c("https://drive.google.com/file/d/14-OdREbU8lukbhbhr8WtA3eZk_Us_zi7",
                           # "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8", 
                           # "https://drive.google.com/open?id=1U0TuYNMC75sQCkZs7c4EcBRLcjqeVO6N", 
                           "https://drive.google.com/file/d/1koskmRuguL4lwMvwhZ5Zdhed8fbZU7FI", 
                           "https://drive.google.com/file/d/1XaPVWzyrXAV1xMaJPsfOo6etTDxcJ8D1",
                           "https://drive.google.com/file/d/1nRds9kp5R3Eo03OctGoStw4qax2pitn4",
                           "https://drive.google.com/file/d/1M1wHxP7DxrM5mLH5yLu21pl9KrmXdGk5",
                           "https://drive.google.com/file/d/1QApZ0yMy-SHkhjm19saa8salhAkq15bv"))
  selected <- DT[climateScenario == scenario, URL]
  return(selected)
}
