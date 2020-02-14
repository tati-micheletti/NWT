#' updateSpeciesEcoregionAttributes
#'
#' TODO: description and title needed
#'
#' @param speciesEcoregion TODO: description needed
#' @param time TODO: description needed
#' @param cohortData \code{data.table} TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
#' @importFrom LandR speciesEcoregionLatestYear
updateSpeciesEcoregionAttributes <- function(speciesEcoregion, time, cohortData) {
  # the following codes were for updating cohortdata using speciesecoregion data at current simulation year
  # to assign maxB, maxANPP and maxB_eco to cohortData
  specieseco_current <- speciesEcoregionLatestYear(speciesEcoregion, time)

  #specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[, .(speciesCode, maxANPP,
                                                      maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco := max(maxB), by = ecoregionGroup]

  cohortData <- specieseco_current[cohortData, on = c("speciesCode", "ecoregionGroup"),
                                   nomatch = NA]#[specieseco_current, nomatch = NA]
  return(cohortData)
}

#' updateSpeciesAttributes
#'
#' TODO: description and title needed
#'
#' @param species TODO: description needed
#' @param cohortData \code{data.table} TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
updateSpeciesAttributes <- function(species, cohortData) {
  # to assign longevity, mortalityshape, growthcurve to cohortData
  species_temp <- setkey(species[, .(speciesCode, longevity, mortalityshape,
                                     growthcurve)], speciesCode)
  setkey(cohortData, speciesCode)
  cohortData <- cohortData[species_temp, nomatch = 0]
  return(cohortData)
}

#' Calculate total biomass
#'
#' TODO: description needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param lastReg TODO: description needed
#' @param simuTime TODO: description needed -- rename this to 'time' to match others
#' @param successionTimestep TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table rbindlist setkey
calculateSumB <- function(cohortData, lastReg, simuTime, successionTimestep) {
  # this function is used to calculate total stand biomass that does not include the new cohorts
  # the new cohorts are defined as the age younger than simulation time step
  # reset sumB
  pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup),
                            temID = 1:length(unique(cohortData$pixelGroup)))
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = 10^4), max(pixelGroups$temID))))
  if (length(cutpoints) == 1) {cutpoints <- c(cutpoints, cutpoints + 1)}
  pixelGroups[, groups := cut(temID, breaks = cutpoints,
                              labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
                              include.lowest = TRUE)]
  for (subgroup in paste("Group",  1:(length(cutpoints) - 1), sep = "")) {
    subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    set(subCohortData, NULL, "sumB", 0L)
    if (simuTime == lastReg + successionTimestep - 2) {
      sumBtable <- subCohortData[age > successionTimestep,
                                 .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
    } else {
      sumBtable <- subCohortData[age >= successionTimestep,
                                 .(tempsumB = as.integer(sum(B, na.rm=TRUE))), by = pixelGroup]
    }
    subCohortData <- merge(subCohortData, sumBtable, by = "pixelGroup", all.x = TRUE)
    subCohortData[is.na(tempsumB), tempsumB := as.integer(0L)][, ':='(sumB = tempsumB, tempsumB = NULL)]
    if (subgroup == "Group1") {
      newcohortData <- subCohortData
    } else {
      newcohortData <- rbindlist(list(newcohortData, subCohortData))
    }
    rm(subCohortData, sumBtable)
  }
  rm(cohortData, pixelGroups, cutpoints)
  return(newcohortData)
}

#' calculateAgeMortality
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#' @param spinupMortalityfraction TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table setkey
calculateAgeMortality <- function(cohortData, stage = "nonSpinup", spinupMortalityfraction) {
  # for age-related mortality calculation
  if (stage == "spinup") {
    cohortData[age > 0, mAge := B*(exp((age) / longevity*mortalityshape) / exp(mortalityshape))]
    cohortData[age > 0, mAge := mAge+B*spinupMortalityfraction]
    cohortData[age > 0, mAge := pmin(B, mAge)]
  } else {
    set(cohortData, NULL, "mAge",
        cohortData$B * (exp((cohortData$age) / cohortData$longevity * cohortData$mortalityshape) /
                          exp(cohortData$mortalityshape)))
    set(cohortData, NULL, "mAge",
        pmin(cohortData$B,cohortData$mAge))
  }
  return(cohortData)
}

#' calculateANPP
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table set
calculateANPP <- function(cohortData, stage = "nonSpinup") {
  if (stage == "spinup") {
    cohortData[age > 0, aNPPAct := maxANPP * exp(1) * (bAP^growthcurve) *
                 exp(-(bAP^growthcurve)) * bPM]
    cohortData[age > 0, aNPPAct := pmin(maxANPP * bPM, aNPPAct)]
  } else {
    set(cohortData, NULL, "aNPPAct",
        cohortData$maxANPP * exp(1) * (cohortData$bAP^cohortData$growthcurve) *
          exp(-(cohortData$bAP^cohortData$growthcurve)) * cohortData$bPM)
    set(cohortData, NULL, "aNPPAct",
        pmin(cohortData$maxANPP*cohortData$bPM,cohortData$aNPPAct))
  }
  return(cohortData)
}

#' calculateGrowthMortality
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table set
#' @importFrom fpCompare %>>% %<=%
calculateGrowthMortality <- function(cohortData, stage = "nonSpinup") {
  if (stage == "spinup") {
    cohortData[age > 0 & bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[age > 0 & bAP %<=% 1.0, mBio := maxANPP*(2*bAP) / (1 + bAP)*bPM]
    cohortData[age > 0, mBio := pmin(B, mBio)]
    cohortData[age > 0, mBio := pmin(maxANPP*bPM, mBio)]
  } else {
    cohortData[bAP %>>% 1.0, mBio := maxANPP*bPM]
    cohortData[bAP %<=% 1.0, mBio := maxANPP*(2*bAP)/(1 + bAP)*bPM]
    set(cohortData, NULL, "mBio",
        pmin(cohortData$B, cohortData$mBio))
    set(cohortData, NULL, "mBio",
        pmin(cohortData$maxANPP*cohortData$bPM, cohortData$mBio))
  }
  return(cohortData)
}

#' calculateCompetition
#'
#' TODO: description and title needed
#'
#' @param cohortData \code{data.table} TODO: description needed
#' @param stage TODO: description needed
#'
#' @return updated cohort \code{data.table}
#'
#' @export
#' @importFrom data.table set
calculateCompetition <- function(cohortData, stage = "nonSpinup") {
  # two competition indics are calculated bAP and bPM
  if (stage == "spinup") {
    cohortData[age > 0, bPot := pmax(1, maxB - sumB + B)]
    cohortData[age > 0, bAP := B/bPot]
    set(cohortData, NULL, "bPot", NULL)
    cohortData[, cMultiplier := pmax(as.numeric(B^0.95), 1)]
    cohortData[age > 0, cMultTotal := sum(cMultiplier), by = pixelGroup]
    cohortData[age > 0, bPM := cMultiplier / cMultTotal]
    set(cohortData, NULL, c("cMultiplier", "cMultTotal"), NULL)
  } else {
    set(cohortData, NULL, "bPot", pmax(1, cohortData$maxB - cohortData$sumB + cohortData$B))
    set(cohortData, NULL, "bAP", cohortData$B/cohortData$bPot)
    set(cohortData, NULL, "bPot", NULL)
    set(cohortData, NULL, "cMultiplier", pmax(as.numeric(cohortData$B^0.95), 1))
    cohortData[, cMultTotal := sum(cMultiplier), by = pixelGroup]
    set(cohortData, NULL, "bPM", cohortData$cMultiplier / cohortData$cMultTotal)
    set(cohortData, NULL, c("cMultiplier", "cMultTotal"), NULL)
  }
  return(cohortData)
}
