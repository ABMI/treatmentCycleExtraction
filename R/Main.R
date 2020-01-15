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
#' @export 
#' @examples

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
  
  targetRegimenRecordsList <- lapply(1:length(parameters),function(i){
    
    extractTargetRegimen(parameters =parameters[[i]],
                         connectionDetails=connectionDetails,
                         cohortTable=cohortTable,
                         cdmDatabaseSchema=cdmDatabaseSchema,
                         cohortDatabaseSchema=cohortDatabaseSchema,
                         targetCohortId=targetCohortId,
                         maxCores=maxCores)
    })
  
  targetRegimenRecords <- data.table::rbindlist(targetRegimenRecordsList)
  if(nrow(targetRegimenRecords) == 0){episodeAndEventTable <-list()}else{
  episodeAndEventTable<-recordsInEpisodeTableForm(targetRegimenRecords)}
  return(episodeAndEventTable)}

