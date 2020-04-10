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

#' @param targetParameter
#' @param connection
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param targetCohortId
#' @param maxCores Number of cores in clusterApply
#' @param parameters
#' @param chemotherapyRecords
#' @param cohortTable
#' @param episodeTable
#' @param episodeEventTable
#' @import dplyr
#' @import tidyr

#' @export
# 1. Target chemotherapy records extraction tool
chemotherapyRecordsExtraction <- function(targetParameter,
                                          connection,
                                          cohortTable,
                                          cdmDatabaseSchema,
                                          cohortDatabaseSchema,
                                          targetCohortId,
                                          maxCores
)
{
  if (class(targetParameter)!="regimenLists") stop ("parameters should be regimenLists type")
  
  # Define parameters
  regimenConceptId <- parameters$regimenConceptId
  regimenName <- parameters$regimenName
  includeDescendant <- parameters$includeDescendant
  outofCohortPeriod <- parameters$outofCohortPeriod
  combinationCriteria <- parameters$combinationCriteria
  gapDateBetweenCycle <- parameters$gapDateBetweenCycle
  gapDateAfter <- parameters$gapDateAfter
  gapDateBefore <- parameters$gapDateBefore
  primaryConceptIdList <- parameters$primaryConceptIdList
  secondaryConceptIdList <- parameters$secondaryConceptIdList
  excludingConceptIdList <- parameters$excludingConceptIdList
  
  # Primary records
  primaryConceptRecords <- DrugExposureInCohort(connection,
                                                cohortTable,
                                                includeDescendant,
                                                outofCohortPeriod,
                                                cdmDatabaseSchema,
                                                cohortDatabaseSchema,
                                                targetConceptIds = primaryConceptIdList,
                                                targetCohortId)
  
  # Secondary records
  cluster <- ParallelLogger::makeCluster(numberOfThreads = maxCores)
  secondaryConceptRecords <- ParallelLogger::clusterApply(cluster,
                                                          secondaryConceptIdList,
                                                          DrugExposureInCohort,
                                                          connection,
                                                          cohortTable,
                                                          includeDescendant = TRUE,
                                                          outofCohortPeriod = TRUE,
                                                          cdmDatabaseSchema,
                                                          cohortDatabaseSchema,
                                                          targetCohortId)
  ParallelLogger::stopCluster(cluster)
  
  # Exclude records
  if(length(excludingConceptIdList)==0){excludingConceptRecords <- NULL}else{
    excludingConceptRecords <- DrugExposureInCohort(connection,
                                                    cohortTable,
                                                    includeDescendant,
                                                    outofCohortPeriod,
                                                    cdmDatabaseSchema,
                                                    cohortDatabaseSchema,
                                                    targetConceptIds = excludingConceptIdList,
                                                    targetCohortId)
  }
  
  # Extraction
  data <- lapply(unique(primaryConceptRecords$subjectId),function(x){
    try(gapDateExamination(x,
                           primaryConceptRecords,
                           secondaryConceptRecords,
                           excludingConceptRecords,
                           combinationCriteria,
                           secondaryConceptIdList,
                           excludingConceptIdList,
                           gapDateBetweenCycle,
                           gapDateBefore,
                           gapDateAfter,
                           regimenConceptId)
    )
  }
  )
  
  data <- na.omit(data.table::rbindlist(data))
  
  return(data)
}

# 2. Chemotherapy records transform to episode table form
#' @export
chemotherapyToEpisode<- function(chemotherapyRecords){
  # Chemotherapy records to Episode
  classIndex <- class(chemotherapyRecords)
  chemotherapyRecords$CYCLE_START_DATE<-as.Date(chemotherapyRecords$CYCLE_START_DATE,origin="1970-01-01")
  chemotherapyRecords$CYCLE_END_DATE<-as.Date(chemotherapyRecords$CYCLE_END_DATE,origin="1970-01-01")
  chemotherapyRecords$episode_type_concept_id <-32545
  chemotherapyRecords$episode_concept_id <-32532
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_concept_id <- 32531
  chemotherapyRecords$episode_parent_id <-NA
  chemotherapyRecords$episode_object_concept_id <-32525
  chemotherapyRecords$episode_id <- seq(nrow(chemotherapyRecords))
  chemotherapyRecords$episode_source_value <-NA
  class(chemotherapyRecords$episode_parent_id)<- 'integer'
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_parent_id <- chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_id
  chemotherapyRecords<-chemotherapyRecords %>% group_by(SUBJECT_ID) %>% fill(episode_parent_id,.direction = c("up"))
  class(chemotherapyRecords) <- classIndex
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_parent_id <- NA
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$CYCLE_NUM <- NA
  
  # Colnames in Episode
  names(chemotherapyRecords) <- c('person_id',
                                  'episode_start_datetime',
                                  'episode_number',
                                  'episode_source_concept_id',
                                  'episode_end_datetime',
                                  'episode_item',
                                  'episode_type_concept_id',
                                  'episode_concept_id',
                                  'episode_parent_id',
                                  'episode_object_concept_id',
                                  'episode_id',
                                  'episode_source_value')
  
  chemotherapyRecords <- data.frame(chemotherapyRecords)
  episodeRecords <- chemotherapyRecords[,c(11,1,8,2,5,9,3,10,7,12,4,6)]
  episode <- episodeRecords[,c(1:11)]
  ParallelLogger::logInfo("Episode generated")
  # Episode_event generation
  episodeEventRecords <- data.table::data.table()
  
  episodeRecords <- episodeRecords %>% subset(episode_concept_id == 32532)
  episodeDrug <- episodeRecords[,c(1,12)]
  
  n <- nrow(episodeDrug)
  
  for (i in 1:n){
    
    nameIndex <- as.character(episodeDrug[i, 1])
    itemIndex <- as.character(episodeDrug[i, 2])
    
    itemIndexSplitTemp <- data.frame(strsplit(itemIndex, split = '_'))
    episodeDrugTemp <- data.frame(cbind(nameIndex, itemIndexSplitTemp))
    
    names(episodeDrugTemp) <- c("episode_id", "drug_exposure_id")
    
    episodeEventRecords <- rbind(episodeEventRecords, episodeDrugTemp)
  }
  
  episodeEventRecords$visit_occurrence_id <- NA
  episodeEventRecords$condition_occurrence_id <- NA
  episodeEventRecords$procedure_occurrence_id <- NA
  episodeEventRecords$device_exposure_id <- NA
  episodeEventRecords$measurement_id <- NA
  episodeEventRecords$specimen_id <- NA
  episodeEventRecords$observation_id <- NA
  episodeEventRecords$note_id <- NA
  episodeEventRecords$cost_id <- NA
  
  episodeEvent <- episodeEventRecords[,c(1,3,4,5,2,6,7,8,9,10,11)]
  ParallelLogger::logInfo("Episode_event generated")
  
  return(list(episode,episodeEvent))
}

# 3. Using chemotherapy extraction tool and episode transformation tool
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

# 4. Create Episode / Episode_event table in database
#' @export
createEpisodeTable <- function(connection,
                               oracleTempSchema,
                               oncologyDatabaseSchema,
                               episodeTable,
                               episodeEventTable
){
  # Create Episode
  ParallelLogger::logInfo("Create Episode table")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateEpisodeTable.sql",
                                           packageName = "CancerTxPathway",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           oncology_database_schema = oncologyDatabaseSchema,
                                           episode_table = episodeTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
  
  # Create Episode_event
  ParallelLogger::logInfo("Create Episode_event table")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateEpisodeEventTable.sql",
                                           packageName = "CancerTxPathway",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           oncology_database_schema = oncologyDatabaseSchema,
                                           episode_event_table = episodeEventTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
}

# 5. Insert your episode records into database
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
