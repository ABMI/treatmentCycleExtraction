#' generate regimen records for target regimen
#' @param targetRegimenConceptIds regimen concept ids 
#' @param connectionDetails An object of type \code{connectionDetails} as created using the
#'                          \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                          DatabaseConnector package.
#' @param cohortTable The name of the table that will be created in the work database schema.
#'                    This table will hold the exposure and outcome cohorts used in this
#'                    study.
#' @param cdmDatabaseSchema 
#' @param cohortDatabaseSchema
#' @param targetCohortId
#' @param maxCores Number of cores using in clusterApply
#' @keywords target regimen, records
#' @return records of the target single regimen 
#' @export extractTargetRegimen
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
  secondaryConceptIdList <- parameters$secondaryConceptIdList
  excludingConceptIdList <- parameters$excludingConceptIdList
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
  if(length(excludingConceptIdList)==0){excludingConceptRecords <- NULL}else{
    excludingConceptRecords <-  DrugExposureInCohort(connectionDetails,
                                                     cohortTable = cohortTable,
                                                     includeDescendant = includeDescendant,
                                                     outofCohortPeriod = outofCohortPeriod,
                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                                     targetConceptIds = excludingConceptIdList,
                                                     targetCohortId = targetCohortId)
  }
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
                                                                                          gapDateAfter=gapDateAfter,
                                                                                          regimenConceptId=regimenConceptId)
                                                                                                                        )})
  
  ParallelLogger::logInfo("extraction finish")
  data <- na.omit(data.table::rbindlist(data))
  
  return(data)
}
