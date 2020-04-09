# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CancerTxPathway
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Parameter setting for algorithm.

#' @param regimenInstance
#' @param targetRegimenConceptIds
#' @param targetRegimenConceptId
#' @keywords parameter
#' @return parameters
#' @import SqlRender
#' @import DatabaseConnector
#' @examples parameterSetting(targetRegimenConceptIds=targetRegimenConceptIds)

#' @export
listForm <- function(regimenInstance,
                     targetRegimenConceptId
){

  if (class(regimenInstance) != "regimenLists") stop ("regimenLists should be of type regimenLists")

  # Select target regimen for transform
  filteredRegimenLists <- regimenInstance[unlist(lapply(regimenInstance,`[`,"conceptId"))  %in% targetRegimenConceptId][[1]]

  # Transform JSON parameters to list form
  index <- !(names(filteredRegimenLists) %in% c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','combinationCriteria','outofCohortPeriod','origin'))
  roleIndex <- lapply(filteredRegimenLists[index],`[`,"role")

  primaryConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "primary"],`[`,"conceptId")
  secondaryConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "secondary"],`[`,"conceptId")
  excludingConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "excluded"],`[`,"conceptId")

  regimenConceptId <- filteredRegimenLists$conceptId
  regimenName <- filteredRegimenLists$regimenName
  includeDescendant <- filteredRegimenLists$includeDescendant
  outofCohortPeriod <- filteredRegimenLists$outofCohortPeriod
  combinationCriteria <- filteredRegimenLists$combinationCriteria
  gapDateBetweenCycle <- filteredRegimenLists$gapDateBetweenCycle
  gapDateAfter <- filteredRegimenLists$gapDateAfter
  gapDateBefore <- filteredRegimenLists$gapDateBefore

  if(filteredRegimenLists$invalidReason != "")stop("Target concept id has an invalid reason")

  parameters <- list(regimenConceptId,regimenName,includeDescendant,outofCohortPeriod,combinationCriteria,gapDateBetweenCycle,gapDateAfter,gapDateBefore,primaryConceptIdList,secondaryConceptIdList,excludingConceptIdList)

  names(parameters) <- c('regimenConceptId','regimenName','includeDescendant','outofCohortPeriod','combinationCriteria','gapDateBetweenCycle','gapDateAfter','gapDateBefore','primaryConceptIdList','secondaryConceptIdList','excludingConceptIdList')
  class(parameters)= "regimenLists"

  return(parameters)
}

#' @export
parameterSetting <- function(targetRegimenConceptIds){

  # Load JSON File
  jsonFile <- "RegimenParameters.json"
  pathToRjson <- system.file("Json", jsonFile, package = "CancerTxPathway")
  regimenInstance <- rjson::fromJSON(file = pathToRjson)
  class(regimenInstance) = "regimenLists"

  # If there in no specific target regimen, target is whole regimen
  if(is.null(targetRegimenConceptIds)){
    targetRegimenConceptIds <- sapply(regimenInstance,`[`,"conceptId")
  }

  # Target regimen parameters
  listFormRegimen <- lapply(targetRegimenConceptIds,
                            listForm,
                            regimenInstance = regimenInstance)
  class(listFormRegimen) = "regimenLists"

  return(listFormRegimen)
}
