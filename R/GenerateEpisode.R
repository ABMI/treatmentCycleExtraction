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

#' Generate chemotherapy records in episode table and insert to database.

#' @param parameters
#' @param connection
#' @param cohortTable
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param targetCohortId
#' @param maxCores Number of cores using in clusterApply
#' @keywords Extraction, Episode
#' @return Episode / Episode_event table
#' @export
generateEpisode <- function(parameters,
                            connection,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortDatabaseSchema,
                            targetCohortId,
                            maxCores){

  ParallelLogger::logInfo("Episode / Episode_event extraction start")

  # Extract target chemotherapy records
  chemotherapyRecords <- lapply(1:length(parameters),function(i){
    targetParameter <- parameters[[i]]
    chemotherapyRecordsExtraction(targetParameter = targetParameter,
                                  connection,
                                  cohortTable = cohortTable,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  targetCohortId = targetCohortId,
                                  maxCores = maxCores)

    # Logger for target cohort (1/4)
    if(round(i/length(parameters)*100,2) >= 25 && round((i-1)/length(parameters)*100,2) < 25){
      ParallelLogger::logInfo(cat(paste0('target cohort : ',targetCohortId),
                                  '\n',
                                  round(i/length(parameters)*100,2),'%',
                                  '\n',
                                  paste0(Sys.time())
      )
      )
    }
    # Logger for target cohort (2/4)
    if(round(i/length(parameters)*100,2) >= 50 && round((i-1)/length(parameters)*100,2) < 50){
      ParallelLogger::logInfo(cat(paste0('target cohort : ',targetCohortId),
                                  '\n',
                                  round(i/length(parameters)*100,2),'%',
                                  '\n',
                                  paste0(Sys.time())
      )
      )
    }
    # Logger for target cohort (3/4)
    if(round(i/length(parameters)*100,2) >= 75 && round((i-1)/length(parameters)*100,2) < 75){
      ParallelLogger::logInfo(cat(paste0('target cohort : ',targetCohortId),
                                  '\n',
                                  round(i/length(parameters)*100,2),'%',
                                  '\n',
                                  paste0(Sys.time())
      )
      )
    }
  }
  )
  chemotherapyRecords <- data.table::rbindlist(chemotherapyRecords)

  # Transform chemotherapy records to Episode
  if(nrow(chemotherapyRecords) == 0){episodeRecords <- list()}else{
    episodeRecords <- chemotherapyToEpisode(chemotherapyRecords)}

  ParallelLogger::logInfo("Episode / Episode_event extraction finished")

  return(episodeRecords)
}

#' @export
insertEpisode <- function(connection,
                          oncologyDatabaseSchema,
                          episodeTable,
                          episodeEventTable,
                          episodes){

  episode <- episodeAndEpisodeEvent[[1]]
  episodeEvent <- episodeAndEpisodeEvent[[2]]

  # Find last episode_Id
  sql <- 'SELECT max(EPISODE_ID) FROM @oncology_database_schema.@episode_table'
  sql <- SqlRender::render(sql,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  lastEpisodeId <- DatabaseConnector::querySql(connection, sql)
  lastEpisodeId <- as.numeric(lastEpisodeId[,1])
  if(is.na(lastEpisodeId)){lastEpisodeId <- 0}

  # Episode_id update
  episode$episode_id <- as.numeric(episode$episode_id)+lastEpisodeId
  episodeEvent$episode_id <- as.numeric(episodeEvent$episode_id)+lastEpisodeId
  episodeEvent <- as.data.frame(apply(episodeEvent,2,as.numeric))

  # Insert Episode records
  DatabaseConnector::insertTable(connection,
                                 paste0(oncologyDatabaseSchema,'.',episodeTable),
                                 episode,
                                 dropTableIfExists = FALSE,
                                 createTable = FALSE,
                                 progressBar = TRUE)

  # Insert Episode records
  DatabaseConnector::insertTable(connection,
                                 paste0(oncologyDatabaseSchema,'.',episodeEventTable),
                                 episodeEvent,
                                 dropTableIfExists = FALSE,
                                 createTable = FALSE,
                                 progressBar = TRUE )
}
