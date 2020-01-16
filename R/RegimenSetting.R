#' regimenSetting
#' 
#' @param connectionDetails
#' @param connection
#' @param vocaDatabaseSchema
#' @param regimenConceptId
#' @param indexDrugConceptId
#' @keywords regimenSetting
#' @return drugList
#' @import SqlRender
#' @import DatabaseConnector
#' @examples
#' parameterSetting(targetRegimenConceptIds=targetRegimenConceptIds)

#' @export
readJson <- function(jsonName = "regimenDrugSettingJsonForm.json"){
  pathToRjson <-system.file("Json", jsonName, package = "treatmentCycleExtraction")
  regimenLists <-rjson::fromJSON(file = pathToRjson)
  
  class(regimenLists) = "regimenLists"
  return(regimenLists)

}

#' @export
regimenListParameterSetting <- function(regimenLists, targetRegimenConceptId){
  
  if (class(regimenLists)!="regimenLists") stop ("regimenLists should be of type regimenLists")
  filteredRegimenLists<- regimenLists[unlist(lapply(regimenLists,`[`,"conceptId"))  %in% targetRegimenConceptId][[1]]
      
  index <- !(names(filteredRegimenLists) %in% c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod'))
    
    roleIndex<-lapply(filteredRegimenLists[index],`[`,"role")
    
    primaryConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "primary"],`[`,"conceptId")
    secondaryConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "secondary"],`[`,"conceptId")
    excludingConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "excluded"],`[`,"conceptId")
    
    regimenConceptId <- filteredRegimenLists$conceptId
    regimenName <- filteredRegimenLists$regimenName
    includeDescendant <- filteredRegimenLists$includeDescendant
    outofCohortPeriod <- filteredRegimenLists$outofCohortPeriod
    drugInspectionDate <- filteredRegimenLists$drugInspectionDate
    gapDateBetweenCycle <- filteredRegimenLists$gapDateBetweenCycle
    gapDateAfter <- filteredRegimenLists$gapDateAfter  
    gapDateBefore <- filteredRegimenLists$gapDateBefore
    
    parameters <- list(regimenConceptId,regimenName,includeDescendant,outofCohortPeriod,drugInspectionDate,gapDateBetweenCycle,gapDateAfter,gapDateBefore,primaryConceptIdList,secondaryConceptIdList,excludingConceptIdList)
    
    names(parameters) <- c('regimenConceptId','regimenName','includeDescendant','outofCohortPeriod','drugInspectionDate','gapDateBetweenCycle','gapDateAfter','gapDateBefore','primaryConceptIdList','secondaryConceptIdList','excludingConceptIdList')
    class(parameters)= "regimenLists"
    return(parameters)
}

#' @export parameterSetting
parameterSetting <- function(jsonName= "regimenDrugSettingJsonForm.json",targetRegimenConceptIds){
  regimenLists <-readJson(jsonName = jsonName)
  if(is.null(targetRegimenConceptIds)){targetRegimenConceptIds <- sapply(regimenLists,`[`,"conceptId")}
  listFormRegimen<-lapply(targetRegimenConceptIds,regimenListParameterSetting,regimenLists=regimenLists)
  return(listFormRegimen)
}
