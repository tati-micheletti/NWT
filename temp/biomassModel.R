library("googledrive")

tryCatch(googledrive::drive_download(file = googledrive::as_id("1EetOiGxAq-QTZCVU9Q6Y3I26tqjZm3Oi"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
tryCatch(googledrive::drive_download(file = googledrive::as_id("11CrK9PfNJzkU5cZg9-JYYzyr-VP23W0x"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})

inputs <- data.frame(
  files = c(file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds"), 
            file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds")),
  functions = c("base::readRDS", "base::readRDS"),
  stringsAsFactors = FALSE
)


modelCoverAgain <- Cache(statsModel,
                    modelFn = P(sim)$coverModel,
                    # modelFn = cm,
                    uniqueEcoregionGroup = .sortDotsUnderscoreFirst(unique(cohortDataShort$ecoregionGroup)),
                    sumResponse = sum(cohortDataShort$coverPres, cohortDataShort$coverNum, na.rm = TRUE),
                    .specialData = cds,
                    useCloud = useCloud,
                    cloudFolderID = sim$cloudFolderID,
                    # useCache = "overwrite",
                    showSimilar = getOption("reproducible.showSimilar", FALSE),
                    userTags = c(cacheTags, "modelCover"),
                    omitArgs = c("showSimilar", "useCache", ".specialData", "useCloud", "cloudFolderID"))




modelBiomass2 <- Cache(
  statsModel,
  modelFn = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                               (1 | ecoregionGroup))),
  uniqueEcoregionGroup = .sortDotsUnderscoreFirst(unique(cohortDataNo34to36NoBiomass$ecoregionGroup)),
  sumResponse = sum(cohortDataShort$B, na.rm = TRUE),
  .specialData = cohortDataNo34to36NoBiomass,
  useCloud = useCloud,
  # useCache = "overwrite",
  cloudFolderID = sim$cloudFolderID,
  showSimilar = getOption("reproducible.showSimilar", FALSE),
  userTags = c(cacheTags, "modelBiomass", paste0("subsetSize:", P(sim)$subsetDataBiomassModel)),
  omitArgs = c("showSimilar", ".specialData", "useCloud", "cloudFolderID", "useCache")
)
