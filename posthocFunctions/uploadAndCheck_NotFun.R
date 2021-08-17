library("googledrive")
allFiles <- list.files("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS", 
                       pattern = "_LandR.CS_fSpredicted", 
                       recursive = TRUE, full.names = TRUE)

foldID <- "1O34zQIem_RUxxCDOZMGEUPIkUtCWsS_c"

# notAllFilesGone <- TRUE
# while (notAllFilesGone){
  # 1. Check the files
  whichFilesUploaded <- drive_ls(as_id(foldID))
  # 2. Compare to what you have
  stillToGo <- which(basename(allFiles) %in% setdiff(basename(allFiles), whichFilesUploaded$name))
  if (length(stillToGo) == 0){
    assert <- nrow(whichFilesUploaded) == 6030
    if (!assert) stop(paste0("Something weird happened. There is no difference between the files",
                      " locally and in google drive, but the number of files is different.",
                      " Please debug."))
    message(crayon::green("All Files Uploaded Successfully!"))
    # notAllFilesGone <- FALSE
  }
  # if (notAllFilesGone){
    # 3. If there are still files, upload them 
    filesStillToGo <- allFiles[stillToGo]
    lapply(filesStillToGo, drive_upload, path = as_id(foldID))
  # }
# }


