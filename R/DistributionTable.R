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
#' @export
distributionTable <- function(episodeTable,episodeSourceConceptId){
  
  episode <- episodeTable %>% filter(episode_source_concept_id == episodeSourceConceptId)
  maxCycleNumberPerPerson<-aggregate(episode$episode_number,by = list(episode$person_id), max)
  colnames(maxCycleNumberPerPerson) <- c('person_id','Cycle_num')
  
  # Total count
  totalCount<-length(unique(maxCycleNumberPerPerson$person_id))
  
  # Count the number of patients in the value of each cycle number
  countCycle<-as.data.frame(maxCycleNumberPerPerson %>% group_by(Cycle_num) %>% summarise(n = n()))
  countCycle$'%'<-round(prop.table(table(maxCycleNumberPerPerson$Cycle_num))*100, digits = 1)
  sum<- sum(countCycle$n)
  sumName<- paste0('N','(','total=',sum,')')
  names(countCycle) <- c('Treatment cycle',sumName,'%')
  return(countCycle)}
