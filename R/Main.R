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
#' @examples

#' @export generateEpisodeTable
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

#' @export
insertEpisodeToDatabase <- function(connectionDetails,oncologyDatabaseSchema,episodeTable,episodeEventTable,createEpisodeAndEventTable,episodeAndEpisodeEvent){
  conn <- DatabaseConnector::connect(connectionDetails)
  
  episodeRecordsTable <- episodeAndEpisodeEvent[[1]]
  episodeEventRecordsTable <- episodeAndEpisodeEvent[[2]]
  if(createEpisodeAndEventTable == FALSE){lastEpisodeId<-findEpisodeIdlength(connectionDetails,
                                                                             oncologyDatabaseSchema,
                                                                             episodeTable)
  lastEpisodeId<-lastEpisodeId[[1]]
  episodeRecordsTable$episode_id <- episodeRecordsTable$episode_id+lastEpisodeId
  episodeEventRecordsTable$episode_id <- episodeEventRecordsTable$episode_id+lastEpisodeId}else{}
  
  
  DatabaseConnector::insertTable(conn, episodeTable, episodeRecordsTable, createTable = createEpisodeAndEventTable, progressBar = TRUE )
  
  DatabaseConnector::insertTable(conn, episodeEventTable, episodeEventRecordsTable, createTable = createEpisodeAndEventTable, progressBar = TRUE )
  
  DatabaseConnector::disconnect(conn)
} 

#' @export 


findEpisodeIdlength <-function(connectionDetails,
                               oncologyDatabaseSchema,
                               episodeTable){
  
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'SELECT max(episode_id) FROM @oncology_database_schema.@episode_table'
  sql <- SqlRender::render(sql,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable,
  )
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  
  DatabaseConnector::disconnect(connection)
  return(result)}