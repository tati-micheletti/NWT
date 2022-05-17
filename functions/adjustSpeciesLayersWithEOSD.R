adjustSpeciesLayersWithEOSD <- function(rstLCC, 
                                        speciesLayers,   
                                        coniferLeadingClasses = c(1, 6, 8),
                                        coniferSpecies = c("Pice_Gla", "Pice_Mar", "Pinu_Ban"),
                                        decidousLeadingClasses = c(2, 11),
                                        mixedClasses = c(3, 13),
                                        deciduousSpecies = c("Betu_Pap", "Lari_Lar", "Popu_Tre")){

  stkLCC <- data.table(raster::getValues(raster::stack(rstLCC, speciesLayers2011)))
  stkLCC[, pixelID := 1:ncell(rstLCC)]
  stkLCC <- na.omit(object = stkLCC)
  
  # Make sum of each percentage
  stkLCC[, coniferTotal := eval(parse(text = paste(coniferSpecies, collapse = "+")))]
  stkLCC[, deciduousTotal := eval(parse(text = paste(deciduousSpecies, collapse = "+")))]
  stkLCC[, totalBiomass := eval(parse(text = paste(c(coniferSpecies, deciduousSpecies), collapse = "+")))]
  stkLCC[, c("percConifers", "percDeciduous") := list(coniferTotal/totalBiomass,
                                                      deciduousTotal/totalBiomass)]
  
  # Calculate the percentage that each species contributes to the total of the group (conifer or deciduous)
  for (index in seq_along(coniferSpecies)){
    stkLCC[, c(paste0("perc", coniferSpecies[index])) := eval(parse(text = coniferSpecies[index]))/coniferTotal]
  }
  for (index in seq_along(deciduousSpecies)){
    stkLCC[, c(paste0("perc", deciduousSpecies[index])) := eval(parse(text = deciduousSpecies[index]))/deciduousTotal]
  }
  
  # Which rows need updates? 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~ STEP 1: get neighbor cell values for those that are 0 in KNN when leading
  # in EOSD
  
  # ############# CONIFERS
  
  # EOSD says the leading is conifer, but deciduousTotal is higher than coniferTotal: 213,203
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
           deciduousTotal > coniferTotal, ]
  
  # Adjacent cells for cases where we don't have the species we need matching the landcover type:
  pix <- stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
                  deciduousTotal > coniferTotal & 
                  Pice_Mar == 0 & 
                  Pice_Gla == 0 & 
                  Pinu_Ban == 0, pixelID]
  
  adjDT <- data.table(adjacent(speciesLayers2011, pix, directions = 8, 
                               pairs = TRUE, id = TRUE, sort = TRUE))
  
  percNeighb <- rbindlist(lapply(pix, function(pixID){
    pixelToCheck <- adjDT[from == pixID, to]
    totalSp <- stkLCC[pixelID %in% pixelToCheck, colSums(.SD), .SDcols = coniferSpecies]
    propSp <- totalSp/sum(totalSp)
    names(propSp) <- paste0("nPerc", names(propSp))
    propSp <- data.table(t(propSp))
    propSp[, pixelID := pixID]
    return(propSp)
  }))
  stkLCC <- merge(stkLCC, percNeighb, by = "pixelID", all.x = TRUE)
  
  # In the case where nPerc is NaN (i.e. none of the surrounding cells actually have data, make Pice_Mar 100%)
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
           deciduousTotal > coniferTotal &
           Pice_Mar == 0 & 
           Pice_Gla == 0 & 
           Pinu_Ban == 0 &
           is.na(nPercPice_Gla) &
           is.na(nPercPice_Mar) &
           is.na(nPercPinu_Ban), 
         c("nPercPice_Gla", "nPercPice_Mar", "nPercPinu_Ban") := list(0,1,0)]
  
  # Updating the percSp
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
           deciduousTotal > coniferTotal &
           is.na(percPice_Gla), percPice_Gla := nPercPice_Gla]
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
           deciduousTotal > coniferTotal &
           is.na(percPice_Mar), percPice_Mar := nPercPice_Mar]
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
           deciduousTotal > coniferTotal &
           is.na(percPinu_Ban), percPinu_Ban := nPercPinu_Ban]
  
  # After updating I can remove the newPercentage based on neighbours
  stkLCC[, c("nPercPinu_Ban", "nPercPice_Mar", "nPercPice_Gla") := NULL]
  
  
  # ############# DECIDUOUS ############# #
  
  
  # EOSD says the leading is deciduous, but coniferTotal is higher than deciduousTotal: 186,165
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
           deciduousTotal < coniferTotal, ]
  
  # Adjacent cells for cases where we don't have the species we need matching the landcover type:
  pix <- stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
                  deciduousTotal < coniferTotal & 
                  Betu_Pap == 0 & 
                  Lari_Lar == 0 & 
                  Popu_Tre == 0, pixelID]
  
  adjDT <- data.table(adjacent(speciesLayers2011, pix, directions = 8, 
                               pairs = TRUE, id = TRUE, sort = TRUE))
  
  percNeighb <- rbindlist(lapply(pix, function(pixID){
    pixelToCheck <- adjDT[from == pixID, to]
    totalSp <- stkLCC[pixelID %in% pixelToCheck, colSums(.SD), .SDcols = deciduousSpecies]
    propSp <- totalSp/sum(totalSp)
    names(propSp) <- paste0("nPerc", names(propSp))
    propSp <- data.table(t(propSp))
    propSp[, pixelID := pixID]
    return(propSp)
  }))
  stkLCC <- merge(stkLCC, percNeighb, by = "pixelID", all.x = TRUE)
  
  # In the case where nPerc is NaN (i.e. none of the surrounding cells actually have data, make Popu_Tre 100%)
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
           deciduousTotal < coniferTotal &
           Betu_Pap == 0 & 
           Lari_Lar == 0 & 
           Popu_Tre == 0 &
           is.na(nPercBetu_Pap) &
           is.na(nPercLari_Lar) &
           is.na(nPercPopu_Tre), 
         c("nPercBetu_Pap", "nPercLari_Lar", "nPercPopu_Tre") := list(0,0,1)]
  
  # Updating the percSp
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
           deciduousTotal < coniferTotal &
           is.na(percBetu_Pap), percBetu_Pap := nPercBetu_Pap]
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
           deciduousTotal < coniferTotal &
           is.na(percLari_Lar), percLari_Lar := nPercLari_Lar]
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
           deciduousTotal < coniferTotal &
           is.na(percPopu_Tre), percPopu_Tre := nPercPopu_Tre]
  
  # After updating I can remove the newPercentage based on neighbours
  stkLCC[, c("nPercBetu_Pap", "nPercLari_Lar", "nPercPopu_Tre") := NULL]
  
  # Now replace the proportions for:
  # EOSD in Conifer leading, coniferSpecies = 0 
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
           deciduousTotal > coniferTotal & 
           Pice_Mar == 0 & 
           Pice_Gla == 0 & 
           Pinu_Ban == 0, 
         c("Pice_Gla", "Pice_Mar", "Pinu_Ban") := 
           list(round(100*percPice_Gla, digits = 0),
                round(100*percPice_Mar, digits = 0),
                round(100*percPinu_Ban, digits = 0))]
  
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
           deciduousTotal < coniferTotal & 
           Betu_Pap == 0 & 
           Lari_Lar == 0 & 
           Popu_Tre == 0, 
         c("Betu_Pap", "Lari_Lar", "Popu_Tre") := 
           list(round(100*percBetu_Pap, digits = 0),
                round(100*percLari_Lar, digits = 0),
                round(100*percPopu_Tre, digits = 0))]
  
  toKeep <- names(stkLCC)[!grepl(x = names(stkLCC), pattern = "perc")]
  stkLCC <- stkLCC[, ..toKeep]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~ STEP 2: Fixing the proportion 
  
  # Update the total
  stkLCC[, coniferTotal := eval(parse(text = paste(coniferSpecies, collapse = "+")))]
  stkLCC[, deciduousTotal := eval(parse(text = paste(deciduousSpecies, collapse = "+")))]
  stkLCC[, totalBiomass := eval(parse(text = paste(c(coniferSpecies, deciduousSpecies), collapse = "+")))]
  stkLCC[, c("percConifers", "percDeciduous") := list(coniferTotal/totalBiomass,
                                                      deciduousTotal/totalBiomass)]
  
  adjust <- function(indivProportion, adjustFactor, totalProportion) 
    (indivProportion * adjustFactor)/totalProportion
  
  # Check the CONIFER LEADING: 647,700
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
           percConifers < 0.75, `:=` (
             Betu_Pap = adjust(Betu_Pap, 25, deciduousTotal),
             Lari_Lar = adjust(Lari_Lar, 25, deciduousTotal),
             Pice_Gla = adjust(Pice_Gla, 75, coniferTotal),
             Pice_Mar = adjust(Pice_Mar, 75, coniferTotal),
             Pinu_Ban = adjust(Pinu_Ban, 75, coniferTotal),
             Popu_Tre = adjust(Popu_Tre, 25, deciduousTotal)
           )]
  
  # Check the DECIDUOUS LEADING: 227,676
  stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses & 
           percDeciduous < 0.75, `:=` (
             Betu_Pap = adjust(Betu_Pap, 75, deciduousTotal),
             Lari_Lar = adjust(Lari_Lar, 75, deciduousTotal),
             Pice_Gla = adjust(Pice_Gla, 25, coniferTotal),
             Pice_Mar = adjust(Pice_Mar, 25, coniferTotal),
             Pinu_Ban = adjust(Pinu_Ban, 25, coniferTotal),
             Popu_Tre = adjust(Popu_Tre, 75, deciduousTotal)
           )]
  
  # Check I don't have any more mismatch
  stkLCC[, coniferTotal := eval(parse(text = paste(coniferSpecies, collapse = "+")))]
  stkLCC[, deciduousTotal := eval(parse(text = paste(deciduousSpecies, collapse = "+")))]
  stkLCC[, totalBiomass := eval(parse(text = paste(c(coniferSpecies, deciduousSpecies), collapse = "+")))]
  stkLCC[, c("percConifers", "percDeciduous") := list(coniferTotal/totalBiomass,
                                                      deciduousTotal/totalBiomass)]
  a <- stkLCC[EOSD_NT1_BCR6_JAN21 %in% decidousLeadingClasses &
                percDeciduous < 0.74999999999,]
  testthat::expect_true(NROW(a) == 0)
  
  b <- stkLCC[EOSD_NT1_BCR6_JAN21 %in% coniferLeadingClasses & 
                percConifers < 0.74999999999,]
  testthat::expect_true(NROW(b) == 0)
  
  toKeep <- c("pixelID", names(speciesLayers2011)) 
  
  stkLCC <- stkLCC[, ..toKeep]
  fillUp <- data.table(pixelID = 1:ncell(speciesLayers2011))
  stkLCC <- merge(fillUp, stkLCC, all = TRUE, by = "pixelID")
  stkLCC[, pixelID := NULL]
  mstkLCC <- as.matrix(stkLCC)
  newSpeciesLayers <- raster::stack(setValues(x = speciesLayers2011, values = mstkLCC))
  return(newSpeciesLayers)
}
