#' drugRecordExamination
#'
#' check each index dates of single subject whether drug conditions are satisfied or not.
#' @param subjectId : targeting subjectId in target cohort
#' @keywords primaryDrug, secondaryDrug,eliminatoryDrug
#' @return drug condition passed index dates list
#' @export 
#' @import dplyr
#' @examples
#' drugRecordExamination(subjectId = 11111111)

# drugRecordExamination
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
    secondaryConceptRecordsOneSubject <-lapply(1:length(secondaryConceptIdList),function(i){secondaryConceptRecords[[i]] %>% filter (subjectId == targetSubjectId)})
    
  }
  
  if(length(excludingConceptIdList)!=0){
    excludingConceptRecordsOneSubject <-excludingConceptRecords %>% filter(subjectId == targetSubjectId)}
  
  drugConditionPassedDate <-c()
  drugConditionPassedStartDate <- c()
  drugConditionPassedEndDate <- c()
  ###
  
  for(x in 1:nrow(indexDateList)){
    
    inResult<-list()
    endDateList <-list()
    
    ##secondary concept
    if(length(secondaryConceptIdList)!=0){
      for(i in 1:length(secondaryConceptIdList)){
        
        secondaryConceptInPeriod <- dplyr::filter(secondaryConceptRecordsOneSubject[[i]],between(drugExposureStartDate,indexDateList[x,3],indexDateList[x,3]+drugInspectionDate))
        
        inResult<-append(inResult,list(secondaryConceptInPeriod[1,3]))
        if (length(secondaryConceptInPeriod$drugExposureEndDate)!=0){
          endDateList<-append(endDateList,list(unique(max(secondaryConceptInPeriod$drugExposureEndDate,na.rm =TRUE))))}
      }
    }
    ##excluding concept
    if(length(excludingConceptIdList)!=0){
      excludingConceptInPeriod <- dplyr::filter(excludingConceptRecordsOneSubject,between(drugExposureStartDate,indexDateList[x,3],indexDateList[x,3]+drugInspectionDate))
      
      outResult <- excludingConceptInPeriod[1,3]
      
    }else{outResult<-NA}
    ##
    if(sum(is.na(inResult))==0 & sum(!is.na(outResult))==0){
      drugConditionPassedStartDate[x]<- indexDateList[x,3]
      
      if(!is.null(drugConditionPassedStartDate)){
        endDate<-as.Date(max(do.call(rbind,endDateList),na.rm =TRUE),origin="1970-01-01")
        drugConditionPassedEndDate[x]<- max(c(indexDateList[x,3],endDate),na.rm =TRUE)}
      
    }
  }
  ###
  if(!is.null(drugConditionPassedStartDate)){
    drugConditionPassedDate <- data.frame(drugConditionPassedStartDate,drugConditionPassedEndDate)
    drugConditionPassedDate <- na.omit(drugConditionPassedDate)}else{drugConditionPassedDate <- data.frame()}
  }else{drugConditionPassedDate<-data.frame()}
  return(drugConditionPassedDate)
}

