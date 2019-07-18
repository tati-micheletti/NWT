reviseSpeciesTraits <- function(speciesTable = NULL, updatedTable = NULL,
                                destinationPath = tempdir(),
                                returnTable = FALSE, 
                                species = c("BETU.PAP","LARI.LAR","PICE.GLA",
                                            "PICE.MAR","PINU.BAN","POPU.TRE", 
                                            "PINU.CON")){
  if (returnTable){
    message(crayon::blue(paste0("This is the ORIGINAL species table traits (from LANDIS-II).",
                               "\nTo check the table to be used in the simulation, save the object \n", 
                               crayon::yellow("sim$species"), " in your next run.")))
    tb <- LandR::getSpeciesTable(dPath = destinationPath)
    return(tb[LandisCode %in% species])
  } else {
    if (is.null(speciesTable))
      stop("speciesTable can only be NULL is returnTable == TRUE")
    if (is.null(updatedTable)){ 
      #  TOADD: 
      #  CDF Long = 800, Mort = 15, GrowthCurv = 0,
      #  IDF Long = 500, Mort = 15, GrowthCurv = 0.1
      updatedTable <- structure(list(species = c("Abie_Bal", "Betu_Pap", 
                                                 "Lari_Lar", "Pice_Gla", 
                                                 "Pice_Mar", "Pinu_Ban", 
                                                 "Popu_Tre", "Pinu_Con",
                                                 "Abie_Las", "Pice_Eng"), 
                                     longevity = c(200L, 250L, 
                                                   350L, 500L, 
                                                   250L, 250L, 
                                                   250L, 300L,
                                                   400L, 500L),
                                     mortalityshape = c(25L, 8L, 
                                                        15L, 11L, 
                                                        20L, 12L, 
                                                        9L, 15L, 
                                                        16L, 13L),
                                     growthcurve = c(0, 0.1, 
                                                     0.5, 0.5, 
                                                     0.8, 0.5, 
                                                     0.1, 0.1, 
                                                     0.8, 0.5), 
                                     speciesCode = structure(1:10, .Label = c("Abie_Bal", "Betu_Pap", 
                                                                             "Lari_Lar", "Pice_Gla", 
                                                                             "Pice_Mar", "Pinu_Ban", 
                                                                             "Popu_Tre", "Pinu_Con",
                                                                             "Abie_Las", "Pice_Eng"), 
                                                             class = "factor")), 
                                class = c("data.table", "data.frame"), 
                                row.names = c(NA, -10L), 
                                sorted = "speciesCode")
      message(crayon::yellow(paste0("No updated species table trait was provided. Using default:")))
      print(updatedTable)
    }
paramsToChange <- names(updatedTable)[!names(updatedTable) %in% c("species", "speciesCode")]
speciesToChange <- unique(speciesTable$species)
 invisible(lapply(X = speciesToChange, function(sp){
   lapply(X = paramsToChange, FUN = function(param){
     speciesTable[species == sp, (param) := updatedTable[species == sp, 
                                                         eval(parse(text = param))]]
   })
 })
 )
 message("Species trait table was updated: ")
 print(speciesTable)
  }
}
