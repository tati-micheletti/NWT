#' Designed as a helper to create caribou models based on 2011 ECCC report on population demographics and RSF.
#' This function uses \code{usefun::createEquation()} internally
#'
#' @param caribouCoefTable data.table of the coefficient values for all models available. If not provided,
#'                         will use as default the one from ECCC 2011 for population demographics.
#'
#' @param modelsToUse Character string of the model to be used. Currently only "M3" and "M7" (demograohics) 
#'                    and "TaigaPlains" (RSF) have been implemented. Default is "M3".
#'                
#' @return names list of the quote of the model that can be parsed to generate the simulated data bootstrapped 
#'         using the model coefficients.
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table data.table 
#' @include createEquation
#' @rdname createModels
#' 
#' @example 
#' m3 <- createModels()
#' DH_Total <- 50
#' calculatingModel <- eval(parse(text = m3$M3))

createModels <- function(caribouCoefTable = NULL,
                         modelsToUse = "M3"){
  message("Using model ", crayon::magenta(modelsToUse)," for caribou...")
  if (is.null(caribouCoefTable)){
    message(crayon::yellow("caribouCoefTable was not supplied. Using defauilt from ECCC2011 for demographics"))
    caribouCoefTable <- structure(list(ResponseVar = c("Rec", "Rec", "Rec", "Rec", "Rec", 
                                   "Rec", "Rec", "Rec", "Rec", "Rec", "Rec", "Rec", "Rec", "Rec"
    ), ModelNum = c("M3", "M3", "M7", "M7", "M7", "M8", "M8", "M8", 
                    "M9", "M9", "M9", "M12", "M12", "M12"), 
    Type = c("National",
             "National", "National", "National", "National", "National", "National",
             "National", "National", "National", "National", "National", "National",
             "National"), Coefficient = c("Intercept", "DH_Total", "Intercept",
                                          "DH_Anthro", "DH_Fire", "Intercept", "DH_Total", "fire_prop_dist",
                                          "Intercept", "DH_Total", "ln_nn", "Intercept", "DH_Total", "hqh"), 
    Value = c(44.265, -0.429, 43.702, -0.432, -0.373, 43.551,
              -0.425, 1.211, 42.833, -0.425, 0.204, 45.191, -0.432, -2.425), 
    StdErr = c(2.942, 0.061, 3.263, 0.062, 0.143, 4.496, 0.066, 
               5.664, 7.216, 0.065, 0.932, 3.931, 0.062, 6.65)), 
    class = c("spec_tbl_df",
              "tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -14L), spec = structure(list(
      cols = list(ResponseVar = structure(list(), class = c("collector_character",
                                                            "collector")), 
                  ModelNum = structure(list(), class = c("collector_character",
                                                         "collector")), 
                  Type = structure(list(), class = c("collector_character",
                                                     "collector")), 
                  Coefficient = structure(list(), class = c("collector_character",
                                                            "collector")), 
                  Value = structure(list(), class = c("collector_double",
                                                      "collector")), 
                  StdErr = structure(list(), class = c("collector_double",
                                                       "collector"))), 
      default = structure(list(), class = c("collector_guess",
                                            "collector")), skip = 1), 
      class = "col_spec"))
    caribouCoefTable <- data.table(caribouCoefTable)
  }
  if (length(modelsToUse)>1)
    stop("Please provide only one model to be used")
  if (!modelsToUse %in% c("M3", "M7", "TaigaPlains"))
    stop("Only models M3 and M7 (for population growth) and TaigaPlains (for RSF) have been implemented so far")
  modelCoeff <- caribouCoefTable[ModelNum == modelsToUse & !is.na(Value)]
  
  equation <- createEquation(model = modelCoeff)
  modList <- list(equation)
  names(modList) <- modelsToUse
  return(modList)
}
