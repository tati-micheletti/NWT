#' changeTraits provides an easy way of modifying species trait's table in LandR by 
#' multiplication of original traits by a specified factor  
#'
#' @param speciesTable speciesTable data.table from LandR. Contains different traits in columns and 
#'                     a row for each species.
#'
#' @param param Character vector of the trait to be modified.
#' 
#' @param facMult numeric. Factor by which the specified trait should be multiplied.
#' 
#' @param species character string of the Species for which the trait should be modified
#'                
#' @return Returns a data.table updated with the new parameters.
#' @export
#' @author Tati Micheletti
#' 
#' @rdname changeTraits


changeTraits <- function(speciesTable, param, facMult, species){
  spTb <- lapply(X = species, FUN = function(sp){
    speciesTable[species == sp, (param) := eval(parse(text = param))*facMult]
  })
  return(spTb[[1]])
}
