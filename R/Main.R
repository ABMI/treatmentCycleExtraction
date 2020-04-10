# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of treatmentCycleExtraction
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

#' Episode / Episode_event table generation for chemotherapy
#' @param connectionDetails
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param vocaDatabaseSchema
#' @param cohortDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param cohortTable
#' @param episodeTable
#' @param episodeEventTable
#' @param includeConceptIdSetDescendant
#' @param maxCores
#' @param cohortTableCreation
#' @param episodeTableCreation
#' @param generateTargetCohort
#' @return Target Cohort, Episode, Episode Event
#' @examples

#' @export executeExtraction
executeExtraction <- function(connectionDetails,
                              oracleTempSchema = NULL,
                              cdmDatabaseSchema,
                              vocaDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              oncologyDatabaseSchema,
                              cohortTable,
                              episodeTable,
                              episodeEventTable,
                              includeConceptIdSetDescendant = TRUE,
                              maxCores,
                              cohortTableCreation = FALSE,
                              episodeTableCreation = FALSE,
                              generateTargetCohort = FALSE
){

  # DB connection_1
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  ###################
  ## Target Cohort ##
  ###################

  # Create cohort table
  if(cohortTableCreation){
    createCohortTable(connection,
                      oracleTempSchema,
                      cohortDatabaseSchema,
                      cohortTable)
  }

  # Generate target cohort
  if(generateTargetCohort){
    TargetCohortGeneration(connection,
                           oracleTempSchema,
                           cdmDatabaseSchema,
                           vocaDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           includeConceptIdSetDescendant)
  }

  # DB disconnection_1
  DatabaseConnector::disconnect(connection)

  ###################
  ## Episode Table ##
  ###################

  # DB connection_2
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  # Create Episode table, Episode_event
  if(episodeTableCreation){
    createEpisodeTable(connection,
                       oracleTempSchema,
                       oncologyDatabaseSchema,
                       episodeTable,
                       episodeEventTable)
  }

  # DB disconnection_2
  DatabaseConnector::disconnect(connection)

  # Load regimen Concept_Id for Target Cohort (Cohort_Definition_Id)
  pathToCsv <- system.file("csv", "Info_TargetRegimen.csv", package = "treatmentCycleExtraction")
  regimenInfo <- read.csv(pathToCsv, header = TRUE, stringsAsFactors = F)

  # Extract Episode / Episode_event table
  for(i in 1:nrow(regimenInfo)){

    # Load target Cohort_Definition_Id, List for Regimen_Concept_Id
    targetCohortId <- regimenInfo$targetCohortId[i]
    targetRegimenConceptIds <- strsplit(as.character(regimenInfo$regimenConceptIds),';')[[i]]

    # JSON parameters to List form
    parameters <- parameterSetting(targetRegimenConceptIds)

    # Generate Episode / Episode_event table
    episodes <- generateEpisode(parameters,
                                connectionDetails,
                                cohortTable,
                                cdmDatabaseSchema,
                                cohortDatabaseSchema,
                                targetCohortId,
                                maxCores)

    # Insert Episode / Episode_event table to DB
    insertEpisode(connectionDetails,
                  oncologyDatabaseSchema,
                  episodeTable,
                  episodeEventTable,
                  episodes)
  }
}
