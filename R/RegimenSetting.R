#' regimenSetting
#' 
#' @param connectionDetails
#' @param connection
#' @param vocaDatabaseSchema
#' @param regimenConceptId
#' @param indexDrugConceptId
#' @keywords regimenSetting
#' @return drugList
#' @export
#' @import SqlRender
#' @import DatabaseConnector
#' @examples
#' regimenSetting(connectionDetails = connectionDetails,
#' connection = connection,
#' vocaDatabaseSchema = connectionDetails$schema,
#' regimenConceptId = regimenConceptId)
#' @export
parameterSetting <- function(jsonName= "regimenDrugSettingJsonForm.json",targetRegimenConceptIds){
  regimenLists <-readJson(jsonName = jsonName)
  filteredRegimenLists<-regimenListFilter(regimenLists=regimenLists,targetRegimenConceptIds)
  parameterList <- targetRegimenParameterSetting(filteredRegimenLists,regimenLists, targetRegimenConceptIds)
  for(i in 1:length(parameterList)){class(parameterList[[i]])= "regimenLists"}
  
  return(parameterList)
}

#' @export
readJson <- function(jsonName = "regimenDrugSettingJsonForm.json"){
  pathToRjson <-system.file("Json", jsonName, package = "treatmentCycleExtraction")
  data <-rjson::fromJSON(file = pathToRjson)
  
  class(data) = "regimenLists"
  return(data)
}

#' @export
regimenListFilter <- function(regimenLists, targetRegimenConceptIds){
  
  if (class(regimenLists)!="regimenLists") stop ("regimenLists should be of type regimenLists")
  filteredRegimenLists<- regimenLists[unlist(lapply(regimenLists,`[`,"conceptId"))  %in% targetRegimenConceptIds]
  
  class(filteredRegimenLists) = "regimenLists"
  return(filteredRegimenLists)
}

#' @export
targetRegimenParameterSetting <- function(filteredRegimenLists,regimenLists, targetRegimenConceptIds){
  
  filteredRegimenLists <- regimenListFilter(regimenLists=regimenLists, targetRegimenConceptIds=targetRegimenConceptIds)
  
  i <- 1:length(filteredRegimenLists)
  
  parameters<-lapply(i,function(i){
  singleR<- filteredRegimenLists[[i]]
  regimenConceptId <- singleR$conceptId
  regimenName <- singleR$regimenName
  includeDescendant <- singleR$includeDescendant
  outofCohortPeriod <- singleR$outofCohortPeriod
  drugInspectionDate <- singleR$drugInspectionDate
  gapDateBetweenCycle <- singleR$gapDateBetweenCycle
  gapDateAfter<- singleR$gapDateAfter  #+
  gapDateBefore<- singleR$gapDateBefore
  primaryConceptIdList <- singleR[[12]]$conceptId
  
  secondaryConceptIdList <- list()
  excludingConceptIdList <- list()
  
  if(length(singleR)>12){
    for(i in 13:length(singleR)){
      if(singleR[[i]]$role == "secondary"){secondaryConceptIdList<-append(secondaryConceptIdList,singleR[[i]]$conceptId)}else
      {excludingConceptIdList<-append(excludingConceptIdList,singleR[[i]]$conceptId)}}
  }
  
  parameters <- list(regimenConceptId,regimenName,includeDescendant,outofCohortPeriod,drugInspectionDate,gapDateBetweenCycle
                     ,gapDateAfter,gapDateBefore,primaryConceptIdList,secondaryConceptIdList,excludingConceptIdList)
  names(parameters) <- c('regimenConceptId','regimenName','includeDescendant','outofCohortPeriod','drugInspectionDate','gapDateBetweenCycle','gapDateAfter','gapDateBefore','primaryConceptIdList','secondaryConceptIdList','excludingConceptIdList')
  return(parameters)}
)
  return(parameters)
}
