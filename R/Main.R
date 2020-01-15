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
                                                                                          gapDateAfter=gapDateAfter,
                                                                                          regimenConceptId=regimenConceptId))})

  ParallelLogger::logInfo("extraction finish")
  data <- na.omit(data.table::rbindlist(data))
  return(data)
}
#' @export
recordsInEpisodeTableForm<- function(regimenRecords){
  
  regimenRecords$CYCLE_START_DATE<-as.Date(regimenRecords$CYCLE_START_DATE,origin="1970-01-01")
  regimenRecords$CYCLE_END_DATE<-as.Date(regimenRecords$CYCLE_END_DATE,origin="1970-01-01")
  regimenRecords$episode_type_concept_id <-32545
  regimenRecords$episode_concept_id <-32532
  regimenRecords$episode_parent_id <-NA
  regimenRecords$episode_object_concept_id <-32525
  regimenRecords$episode_id <-seq(nrow(regimenRecords))
  regimenRecords$episode_source_value <-NA

  
  names(regimenRecords) <- c('person_id',
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
                             'episode_source_value'
                             )
  cycleList <- data.frame(regimenRecords)
  regimenRecords <- cycleList[,c(11,1,8,2,5,9,3,10,7,12,4,6)]
  episodeTable <- regimenRecords[,c(1:11)]
  episodeItemTable <- regimenRecords[,c(1,12)]
 
  
  ## event table
  EventTable <- data.table::data.table()
  
  n <- nrow(episodeItemTable) # total number of rows

  for (i in 1:n){

    name_index <- as.character(episodeItemTable[i, 1])
    item_index <- as.character(episodeItemTable[i, 2])
    
    item_index_split_temp <- data.frame(strsplit(item_index, split = '_'))
    episodeItemTable_temp <- data.frame(cbind(name_index, item_index_split_temp))
    
    names(episodeItemTable_temp) <- c("episode_id", "drug_exposure_id")
    
    EventTable <- rbind(EventTable, episodeItemTable_temp)
  }
  ParallelLogger::logInfo("Episode event table generated")
EventTable$visit_ccurrence_id <- NA
EventTable$condition_occurrence_id <- NA
EventTable$procedure_occurrence_id <- NA
EventTable$device_exposure_id <- NA
EventTable$measurement_id <- NA
EventTable$specimen_id <- NA
EventTable$observation_id <- NA
EventTable$note_id <- NA
EventTable$cost_id <- NA

episodeEventTable <- EventTable[,c(1,3,4,5,2,6,7,8,9,10,11)]

  return(list(episodeTable,episodeEventTable))
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
  
  targetRegimenRecordsList <- lapply(1:length(parameters),function(i){
    parameters<-parameters[[i]]
    extractTargetRegimen(parameters =parameters,
                         connectionDetails=connectionDetails,
                         cohortTable=cohortTable,
                         cdmDatabaseSchema=cdmDatabaseSchema,
                         cohortDatabaseSchema=cohortDatabaseSchema,
                         targetCohortId=targetCohortId,
                         maxCores=maxCores)})
  
  targetRegimenRecords <- data.table::rbindlist(targetRegimenRecordsList)
  episodeAndEventTable<-recordsInEpisodeTableForm(targetRegimenRecords)
  return(episodeAndEventTable)}

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
