#' Populating chemotherapy regimens
#' @param connectionDetails An object of type \code{connectionDetails} as created using the
#'                          \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                          DatabaseConnector package.


#' Excute
#'
#' Excute the Study
#' @param connectionDetails An object of type \code{connectionDetails} as created using the
#'                          \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                          DatabaseConnector package.
#' @param cohortTable The name of the table that will be created in the work database schema.
#'                    This table will hold the exposure and outcome cohorts used in this
#'                    study.
#' @param targetRegimenConceptIds The concept id list of the regimen  
#' @param connectionDetails
#' @param cohortTable
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param eliminatoryDrugList
#' @param targetCohortId
#' @param maxCores
#' @keywords generateEpisodeTable
#' @return Generated episode table 
#' @export generateEpisodeTable
#' @examples
#' generateEpisodeTable(targetRegimenConceptIds,
#'                     connectionDetails,
#'                     cohortTable,
#'                     cdmDatabaseSchema,
#'                     cohortDatabaseSchema,
#'                     targetCohortId,
#'                     maxCores)

# parameter setting from json
#connectionDetails,
#cohortDatabaseSchema,
#cdmDatabaseSchema,
#vocabularyDatabaseSchema = cdmDatabaseSchema,
#oncologyDatabaseSchema = cdmDatabaseSchema,
#cohortTable = "cohort",
#targetCohortId = targetCohortId,
#targetRegimenConceptIds = targetRegimenConceptIds
#targetCohortId = ,
#oracleTempTable = NULL,
#returnData = T,
#insertOncologyDatabase=F,
#customEpisodeTableName = c("EPISODE"),
#customEpisodeEventTableName = c("EPISODE_EVENT"),
#deleteExistingRecordsInTable = F,
#' @export
extractTargetRegimen<-function(parameters,
                               connectionDetails,
                               cohortTable,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema,
                               targetCohortId,
                               maxCores
)
{   
 
  if (class(parameters)!="regimenLists") stop ("regimenLists should be of type regimenLists")
  
  regimenConceptId<-parameters$regimenConceptId
  regimenName<-parameters$regimenName
  includeDescendant<-parameters$includeDescendant
  outofCohortPeriod<-parameters$outofCohortPeriod
  drugInspectionDate<-parameters$drugInspectionDate
  gapDateBetweenCycle<-parameters$gapDateBetweenCycle
  gapDateAfter<-parameters$gapDateAfter
  gapDateBefore<-parameters$gapDateBefore
  primaryConceptIdList<-parameters$primaryConceptIdList
  secondaryConceptIdList<-parameters$secondaryConceptIdList
  excludingConceptIdList<-parameters$excludingConceptIdList
  
  # Exposure concept calling
  ##primary
  primaryConceptRecords <- DrugExposureInCohort(connectionDetails,
                                                cohortTable = cohortTable,
                                                includeDescendant = includeDescendant,
                                                outofCohortPeriod = outofCohortPeriod,
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                cohortDatabaseSchema = cohortDatabaseSchema,
                                                targetConceptIds = primaryConceptIdList,
                                                targetCohortId = targetCohortId)
  
  ParallelLogger::logInfo("Primary Drug Exposure records are loaded")
  
  ##secondary
  cluster <- ParallelLogger::makeCluster(numberOfThreads = maxCores)
  secondaryConceptRecords <- ParallelLogger::clusterApply(cluster,secondaryConceptIdList,DrugExposureInCohort,
                                                          connectionDetails,
                                                          cohortTable,
                                                          includeDescendant = TRUE,
                                                          outofCohortPeriod = TRUE,
                                                          cdmDatabaseSchema,
                                                          cohortDatabaseSchema,
                                                          targetCohortId)
  ParallelLogger::stopCluster(cluster)
  
  ParallelLogger::logInfo("Secondary Drug Exposure records are loaded")
  
  ##excluding
  excludingConceptRecords <-  DrugExposureInCohort(connectionDetails,
                                                   cohortTable = cohortTable,
                                                   includeDescendant = includeDescendant,
                                                   outofCohortPeriod = outofCohortPeriod,
                                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   targetConceptIds = excludingConceptIdList,
                                                   targetCohortId = targetCohortId)
  ParallelLogger::logInfo("Excluding Drug Exposure records are loaded")
  
  
  # drug & cycle condition check (It will take some time)
  
  data<-lapply(unique(primaryConceptRecords$subjectId),function(x){try(gapDateExamination(x,
                                                                    primaryConceptRecords=primaryConceptRecords,
                                                                    secondaryConceptRecords=secondaryConceptRecords,
                                                                    excludingConceptRecords=excludingConceptRecords,
                                                                    drugInspectionDate=drugInspectionDate,
                                                                    secondaryConceptIdList=secondaryConceptIdList,
                                                                    excludingConceptIdList=excludingConceptIdList,
                                                                    gapDateBetweenCycle=gapDateBetweenCycle,
                                                                    gapDateBefore=gapDateBefore,
                                                                    gapDateAfter=gapDateAfter))})
  
         
#  cluster <- ParallelLogger::makeCluster(numberOfThreads = maxCores)
#  data <- ParallelLogger::clusterApply(cluster, unique(primaryConceptRecords$subjectId), gapDateExamination,
#                                     primaryConceptRecords=primaryConceptRecords,
#                                      secondaryConceptRecords=secondaryConceptRecords,
#                                     excludingConceptRecords=excludingConceptRecords,
#                                     drugInspectionDate=drugInspectionDate,
#                                     secondaryDrugConceptIdList=secondaryDrugConceptIdList,
#                                     excludingDrugConceptIdList=excludingDrugConceptIdList,
#                                     gapDateBetweenCycle=gapDateBetweenCycle,
#                                     gapDateBefore=gapDateBefore,
#                                     gapDateAfter=gapDateAfter)
#  ParallelLogger::stopCluster(cluster)
  ParallelLogger::logInfo("extraction finish")
  data <- na.omit(data.table::rbindlist(data))
  data<-recordsInEpisodeTableForm(data,regimenConceptId)
  return(data)
}
#' @export
recordsInEpisodeTableForm<- function(regimenRecords,regimenConceptId){

  regimenRecords$CYCLE_START_DATE<-as.Date(regimenRecords$CYCLE_START_DATE,origin="1970-01-01")
  regimenRecords$CYCLE_END_DATE<-as.Date(regimenRecords$CYCLE_END_DATE,origin="1970-01-01")
  regimenRecords$episode_type_concept_id <-32545
  regimenRecords$episode_concept_id <-32532
  regimenRecords$episode_parent_id <-NA
  regimenRecords$episode_object_concept_id <-32525
  regimenRecords$episode_id <-1
  regimenRecords$episode_source_value <-NA
  regimenRecords$episode_source_concept_id <-regimenConceptId
  
  names(regimenRecords) <- c('person_id',
                                'episode_start_datetime',
                                'episode_number',
                                'episode_end_datetime',
                                'episode_type_concept_id',
                                'episode_concept_id',
                                'episode_parent_id',
                                'episode_object_concept_id',
                                'episode_id',
                                'episode_source_value',
                                'episode_source_concept_id')
  cycleList <- data.frame(regimenRecords)
  regimenRecords <- cycleList[,c(9,1,6,2,4,7,3,8,5,10,11)]
  return(regimenRecords)
}
#' @export
generateEpisodeTable <- function(targetRegimenConceptIds,
                                 connectionDetails,
                                 cohortTable,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 targetCohortId,
                                 maxCores){
  
parameters <- parameterSetting(targetRegimenConceptIds=targetRegimenConceptIds)
ParallelLogger::logInfo("parameter loaded")

regimenEpisodeRecords <- lapply(1:length(parameters),function(i){extractTargetRegimen(parameters =parameters[[i]],
                                     connectionDetails=connectionDetails,
                                     cohortTable=cohortTable,
                                     cdmDatabaseSchema=cdmDatabaseSchema,
                                     cohortDatabaseSchema=cohortDatabaseSchema,
                                     targetCohortId=targetCohortId,
                                     maxCores=maxCores)})

episodeTable <- data.table::rbindlist(regimenEpisodeRecords)

return(episodeTable)}
