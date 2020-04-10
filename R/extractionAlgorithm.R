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
#' Algorithm for chemotherapy record extraction
#' @param targetConceptIds
#' @param targetSubjectId
#' @param primaryConceptRecords
#' @param secondaryConceptRecords
#' @param excludingConceptRecords
#' @param drugInspectionDate
#' @param secondaryConceptIdList
#' @param excludingConceptIdList
#' @param gapDateBetweenCycle
#' @param gapDateBefore
#' @param gapDateAfter
#' @param regimenConceptId
#' @param connection
#' @param cohortTable
#' @param includeDescendant
#' @param outofCohortPeriod
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param targetCohortId

#' @export
DrugExposureInCohort <- function(targetConceptIds,
                                 connection,
                                 cohortTable,
                                 includeDescendant = TRUE,
                                 outofCohortPeriod = TRUE,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 targetCohortId){
  pathToSql <- system.file("sql/sql_server", "DrugExposureInCohort.sql", package = "CancerTxPathway")
  
  sql <- SqlRender::readSql(pathToSql)
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           cohort_table = cohortTable,
                           include_descendant = includeDescendant,
                           out_of_cohort_period = outofCohortPeriod,
                           result_database_schema = cohortDatabaseSchema,
                           target_concept_Ids = targetConceptIds,
                           target_cohort_Id = targetCohortId)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  
  return(result)
}
##########################
## Combination criteria ##
##########################
#' @export
drugRecordExamination<-function(targetSubjectId,
                                primaryConceptRecords,
                                secondaryConceptRecords,
                                excludingConceptRecords,
                                drugInspectionDate,
                                secondaryConceptIdList,
                                excludingConceptIdList
){
  ## Dispense date of primary drug is index date
  ## Generate index date list in one person
  if (!nrow(primaryConceptRecords)==0){
    
    indexDateList <- primaryConceptRecords %>% filter(subjectId == targetSubjectId)
    ## Checking all drug condition
    ### The drug observation period is from the index date to the date as long as drug Observation Date.
    ### Secondary drug should be in the range of drug observation period and eliminatory drug should not be in.
    
    
    if(length(secondaryConceptIdList)!=0){
      secondaryConceptRecordsOneSubject <- lapply(1:length(secondaryConceptIdList),function(i){secondaryConceptRecords[[i]] %>% filter (subjectId == targetSubjectId)})
      
    }
    
    if(length(excludingConceptIdList)!=0){
      excludingConceptRecordsOneSubject <- excludingConceptRecords %>% filter(subjectId == targetSubjectId)}
    
    drugConditionPassedDate <- c()
    drugConditionPassedStartDate <- c()
    drugConditionPassedEndDate <- c()
    eventItem <- c()
    
    
    for(x in 1:nrow(indexDateList)){
      
      inResult <- list()
      endDateList <- list()
      
      
      if(length(secondaryConceptIdList)!=0){
        for(i in 1:length(secondaryConceptIdList)){
          
          secondaryConceptInPeriod <- dplyr::filter(secondaryConceptRecordsOneSubject[[i]],between(drugExposureStartDate,indexDateList[x,3]-drugInspectionDate,indexDateList[x,3]+drugInspectionDate))
          
          inResult<-append(inResult,list(secondaryConceptInPeriod[1,3]))
          if (length(secondaryConceptInPeriod$drugExposureEndDate)!=0){
            endDateList<-append(endDateList,list(unique(max(secondaryConceptInPeriod$drugExposureEndDate,na.rm =TRUE))))}
        }
      }else{secondaryConceptInPeriod <- NULL}
      
      if(length(excludingConceptIdList)!=0){
        excludingConceptInPeriod <- dplyr::filter(excludingConceptRecordsOneSubject,between(drugExposureStartDate,indexDateList[x,3]-drugInspectionDate,indexDateList[x,3]+drugInspectionDate))
        
        outResult <- excludingConceptInPeriod[1,3]
        
      }else{
        outResult<-NA}
      
      if(sum(is.na(inResult))==0 & sum(!is.na(outResult))==0){
        
        if(!is.null(secondaryConceptInPeriod)){
          drugConditionPassedStartDate[x]<- min(c(indexDateList[x,3],secondaryConceptInPeriod$drugExposureStartDate),na.rm =TRUE)
          targetCycleItemSec <- paste0(secondaryConceptInPeriod$drugExposureId,collapse = '_')
          targetCycleItemPri <- paste0(indexDateList[x,5],collapse = '_')
          targetCycleItem<- paste0(c(targetCycleItemSec,targetCycleItemPri),collapse = '_')
        }else
        {drugConditionPassedStartDate[x]<- indexDateList[x,3]
        targetCycleItem<- paste0(indexDateList[x,5],collapse = '_')}
        
        eventItem[x] <- targetCycleItem
        if(!is.null(drugConditionPassedStartDate)){
          drugConditionPassedEndDate[x]<- max(c(indexDateList[x,3],unlist(endDateList)),na.rm =TRUE)
        }
      }
    }
    
    if(!is.null(drugConditionPassedStartDate)){
      drugConditionPassedDate <- data.frame(drugConditionPassedStartDate,drugConditionPassedEndDate,eventItem)
      drugConditionPassedDate <- na.omit(drugConditionPassedDate)
      drugConditionPassedDate <- drugConditionPassedDate[c(order(drugConditionPassedDate$drugConditionPassedStartDate)),]
      drugConditionPassedDate <- unique(drugConditionPassedDate)
      rownames(drugConditionPassedDate) <- NULL
    }else{drugConditionPassedDate <- data.frame()}
  }else{drugConditionPassedDate<-data.frame()}
  
  return(drugConditionPassedDate)
}

################################
## Consecutive cycle criteria ##
################################
#' @export
gapDateExamination<-function(targetSubjectId,
                             primaryConceptRecords,
                             secondaryConceptRecords,
                             excludingConceptRecords,
                             drugInspectionDate,
                             secondaryConceptIdList,
                             excludingConceptIdList,
                             gapDateBetweenCycle,
                             gapDateBefore,
                             gapDateAfter,
                             regimenConceptId){
  
  drugPassed<-drugRecordExamination(targetSubjectId=targetSubjectId,
                                    primaryConceptRecords=primaryConceptRecords,
                                    secondaryConceptRecords=secondaryConceptRecords,
                                    excludingConceptRecords=excludingConceptRecords,
                                    drugInspectionDate=drugInspectionDate,
                                    secondaryConceptIdList=secondaryConceptIdList,
                                    excludingConceptIdList=excludingConceptIdList)
  if (!nrow(drugPassed)==0){
    
    drugConditionPassedStartDate<-drugPassed$drugConditionPassedStartDate
    drugConditionPassedDate<-as.data.frame(drugConditionPassedStartDate)
    
    drugConditionPassedDate$lagdate <- data.table::shift(drugConditionPassedDate$drugConditionPassedStartDate,fill = drugConditionPassedDate$drugConditionPassedStartDate[1])
    
    drugConditionPassedDate$datediff <-drugConditionPassedDate$drugConditionPassedStartDate-drugConditionPassedDate$lagdate
    drugConditionPassedDate$datediff[1] <- 'first'
    drugConditionPassedStartDate<-subset(drugConditionPassedDate,datediff >= gapDateBetweenCycle-gapDateBefore|datediff == 'first')$drugConditionPassedStartDate
    drugConditionPassedDate<-as.data.frame(drugConditionPassedStartDate)
    ## Generate first date list of cycle. From very first record of date list, until next cycle date cannot be found.
    
    gapDatePassedDate <- list()
    gapDatePassedDate[[1]]<-drugConditionPassedDate[1,]
    
    x=1
    repeat{
      
      i=1
      repeat{
        
        minimumGapVariationDate <- gapDatePassedDate[[x]][i]+gapDateBetweenCycle-gapDateBefore
        maximumGapVariationDate <- gapDatePassedDate[[x]][i]+gapDateBetweenCycle+gapDateAfter
        
        gapDatePassedDate[[x]][i+1] <- dplyr::filter(drugConditionPassedDate,between(drugConditionPassedDate$`drugConditionPassedStartDate`,minimumGapVariationDate,maximumGapVariationDate))[1,]
        
        if(is.na(gapDatePassedDate[[x]][i+1]))
        {gapDatePassedDate[[x+1]]<-subset(drugConditionPassedDate,!drugConditionPassedDate$`drugConditionPassedStartDate` %in% unlist(gapDatePassedDate))[1,]}
        
        if(is.na(gapDatePassedDate[[x]][i+1])) break
        i = i+1
      }
      
      if(is.na(gapDatePassedDate[[x+1]])) break
      x = x+1}
    
    gapDatePassedDate[[length(gapDatePassedDate)]] <- NULL
    
    ## Generate result of cycle extraction
    
    subjectCycleList<-lapply(1:length(gapDatePassedDate),function(i){
      cycleStartDate <- gapDatePassedDate[[i]]
      subjectId <-c(targetSubjectId)
      cycleNum <- c(seq_along(cycleStartDate))
      REGIMEN_CONCEPT_ID <- regimenConceptId
      cycle <- data.frame(subjectId,cycleStartDate,cycleNum,regimenConceptId)
      
      drugPassed<-drugPassed %>% group_by(drugConditionPassedStartDate) %>% slice(which.max(drugConditionPassedEndDate))
      cycle <- dplyr::left_join(cycle,drugPassed, by =c("cycleStartDate" = "drugConditionPassedStartDate"))
      
      names(cycle) <- c('SUBJECT_ID','CYCLE_START_DATE','CYCLE_NUM','REGIMEN_CONCEPT_ID','CYCLE_END_DATE','EVENT_ITEM')
      treatmentLineStartDate<-cycle %>% slice(which.min(CYCLE_START_DATE)) %>% select(CYCLE_START_DATE)
      treatmentLineEndDate<-cycle %>% slice(which.max(CYCLE_END_DATE)) %>% select(CYCLE_END_DATE)
      treatmentLineNumberPadding <- 0
      treatmentLineEventItemPadding <- '1_1'
      treatmentLine <- data.frame(subjectId,treatmentLineStartDate,treatmentLineNumberPadding,REGIMEN_CONCEPT_ID,treatmentLineEndDate,treatmentLineEventItemPadding)
      names(treatmentLine) <- c('SUBJECT_ID','CYCLE_START_DATE','CYCLE_NUM','REGIMEN_CONCEPT_ID','CYCLE_END_DATE','EVENT_ITEM')
      cycle<-rbind(cycle,treatmentLine)
      return(cycle)
    }
    )
    
    subjectCycleList<- data.table::rbindlist(subjectCycleList)}else{
      SUBJECT_ID<-c(NA)
      CYCLE_START_DATE <- c(NA)
      CYCLE_NUM<- c(NA)
      CYCLE_END_DATE <- c(NA)
      EVENT_ITEM <- c(NA)
      REGIMEN_CONCEPT_ID <- c(NA)
      subjectCycleList <- data.frame(SUBJECT_ID,CYCLE_START_DATE,CYCLE_NUM,REGIMEN_CONCEPT_ID,CYCLE_END_DATE,EVENT_ITEM)
    }
  
  return(subjectCycleList)}

