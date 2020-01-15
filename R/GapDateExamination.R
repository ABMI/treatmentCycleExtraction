#' gapDateExamination
#'
#' check cycle interval in the range of setting
#' @param subjectId : targeting subject_id in target cohort
#' @keywords gap date
#' @return cycle result list of single subject in cohort
#' @export
#' @import dplyr
#' @import data.table
#' @examples
#' gapDateExamination(targetSubjectIdsubjectId = 11111111)
# gapDateExamination

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
    
    return(cycle)
  }
  )
  
  subjectCycleList<- data.table::rbindlist(subjectCycleList)}else{
    SUBJECT_ID<-c(NA)
    CYCLE_START_DATE <- c(NA)
    CYCLE_NUM<- c(NA)
    CYCLE_END_DATE <- c(NA)
    EVENT_ITEM <- c(NA)
    REGIMEN_CONCEPT_ID <- regimenConceptId
    subjectCycleList <- data.frame(SUBJECT_ID,CYCLE_START_DATE,CYCLE_NUM,REGIMEN_CONCEPT_ID,CYCLE_END_DATE,EVENT_ITEM)
  }
  
  return(subjectCycleList)}
