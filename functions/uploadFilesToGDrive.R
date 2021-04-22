uploadFilesToGDrive <- function(resultsFolderPath, 
                                filePatterns, 
                                UnwantedPatterns, 
                                Recursive,
                                Gfolder){
  Require("googledrive")
  Require("usefulFuns")
  Error <- TRUE
  while (Error){
    Error <- tryCatch({
      # Check which files are up, which need moving up.
      # If all is good, return no error
      # Confirm all files are uploaded
      # 1. Check all files in the cloud
      filesInG <- drive_ls(path = as_id("1tNCmQEJqGJp9s9PDbNHaa2Gfy1Jql9_b"))
      filesInG <- filesInG[["name"]]
      # 2. Check all files in the computer
      fls <- basename(grepMulti(list.files(path = resultsFolderPath, 
                        recursive = Recursive, 
                        full.names = FALSE),
                       patterns = filePatterns,
                       unwanted = UnwantedPatterns))
      missing <- setdiff(fls, filesInG)
      
      # TOO SLOW...
      flsToUp <- list.files(path = resultsFolderPath,
                            recursive = Recursive,
                            full.names = TRUE)
      flsToUp <- flsToUp[grepl(x = flsToUp, pattern = paste(missing, 
                                                            collapse = "|"))]
      lapply(flsToUp, drive_upload, path = as_id(Gfolder))
      FALSE
    }, error = function(e){
      message(paste0(e, "Retrying to upload files..."))
      Sys.sleep(5)
      return(TRUE)
    })
  }
  print(paste0("All files uploaded correctly to https://drive.google.com/drive/folders/", Gfolder))
}
