
# TO MAKE THE PLOT WITH THE DIVISIONS BY TYPE OF "BIRD GROUP"
# Needs:
# netChangeTable (plotsPaper_NotFun.R)
# Species: bird species
if(FALSE){
  netChangeTableUpdated <- rbindlist(lapply(Species, function(sp){
    DT <- netChangeTable[species == sp, ]
    rule1 <- all(DT[effect == "vegetation", netChange] < 0,
                 DT[effect == "climate", netChange] < 0,
                 DT[effect == "fire", netChange] < 0)
    rule2 <- all(DT[effect == "vegetation", netChange] < 0,
                 DT[effect == "climate", netChange] < 0,
                 DT[effect == "fire", netChange] > 0)
    rule3 <- all(DT[effect == "vegetation", netChange] < 0,
                 DT[effect == "climate", netChange] > 0,
                 DT[effect == "fire", netChange] < 0)
    rule4 <- all(DT[effect == "vegetation", netChange] < 0,
                 DT[effect == "climate", netChange] > 0,
                 DT[effect == "fire", netChange] > 0)
    rule5 <- all(DT[effect == "vegetation", netChange] > 0,
                 DT[effect == "climate", netChange] < 0,
                 DT[effect == "fire", netChange] < 0)
    rule6 <- all(DT[effect == "vegetation", netChange] > 0,
                 DT[effect == "climate", netChange] < 0,
                 DT[effect == "fire", netChange] > 0)
    rule7 <- all(DT[effect == "vegetation", netChange] > 0,
                 DT[effect == "climate", netChange] > 0,
                 DT[effect == "fire", netChange] < 0)
    rule8 <- all(DT[effect == "vegetation", netChange] > 0,
                 DT[effect == "climate", netChange] > 0,
                 DT[effect == "fire", netChange] > 0)
    allRules <- c(rule1, 
                  rule2,
                  rule3, 
                  rule4,
                  rule5, 
                  rule6,
                  rule7, 
                  rule8)
    replacementNames <- c("Climate Effects \nPromoting Extirpation", "Climate Effects on Fire \nPromoting Colonization", 
                          "Direct Climate Effects \nPromoting Colonization", "Climate Effects on Vegetation \nPromoting Extirpation",
                          "Climate Effects on Vegetation \nPromoting Colonization", "Direct Climate Effects \nPromoting Extirpation", 
                          "Climate Effects on Fire \nPromoting Extirpation", "Climate Effects \nPromoting Colonization")
    DT[, group := replacementNames[which(allRules)]]
    return(DT)
  }))
}