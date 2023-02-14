outPath <- "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS"
uploadFolder <- "1ffWDDurVMVmK1Ezlgzg4Yoos_F_l01B2"
autoUpload <- TRUE

allAvailableMaps <- rbindlist(lapply(bibis, function(BIRD){
  DT <- rbindlist(lapply(yearsWanted, function(y){
   DT <- rbindlist(lapply(scens, function(Scen){
      print(paste0("Running ", BIRD, " for ", y, " for ", Scen))
      figName <- paste0(BIRD, "_", 
                        strsplit(as.character(Scen), 
                                 split = ".", fixed = TRUE)[[1]][2], 
                        "_", y, ".png")
      fullNameFig <- file.path(outPath, figName)
      return(data.table(Species = BIRD,
                        Scenario = Scen,
                        Year = y,
                        Exists = file.exists(fullNameFig)))
    }))
  }))
}))

allFls <- drive_ls(as_id(uploadFolder))
allAvailableMapsOnline <- rbindlist(lapply(bibis, function(BIRD){
  DT <- rbindlist(lapply(yearsWanted, function(y){
    DT <- rbindlist(lapply(scens, function(Scen){
      outPath <- "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS"
      figName <- paste0(BIRD, "_", 
                        strsplit(as.character(Scen), 
                                 split = ".", fixed = TRUE)[[1]][2], 
                        "_", y, ".png")
      isThere <- any(grepl(pattern = figName, 
                           x = allFls$name))
      if (autoUpload){
        if (!isThere){
          fullNameFig <- file.path(outPath, figName)
          suppressMessages(drive_upload(fullNameFig, as_id(uploadFolder)))
          message(paste0(figName, " uploaded!"))
          isThere <- TRUE
        } else message(paste0(figName, " already available!"))
      }
      return(data.table(Species = BIRD,
                        Scenario = Scen,
                        Year = y,
                        Exists = isThere))
    }))
  }))
}))
