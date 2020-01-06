#' drugRecordExamination
#'
#' check each index dates of single subject whether drug conditions are satisfied or not.
#' @param subjectId : targeting subject_id in target cohort
#' @keywords primaryDrug, secondaryDrug,eliminatoryDrug
#' @return drug condition passed index dates list
#' @export drugRecordExamination
#' @import dplyr
#' @examples
#' drugRecordExamination(subjectId = 11111111)

# drugRecordExamination

drugRecordExamination<-function(targetSubjectId,
                                primaryDrugExposure,
                                secondaryDrugExposure,
                                excludingDrugExposure,
                                drugInspectionDate,
                                secondaryDrugConceptIdList,
                                excludingDrugConceptIdList
                                ){
  
  ## Dispense date of primary drug is index date
  ## Generate index date list in one person
  
  # primaryDrugExposureOneSubject <-lapply(1:length(primaryDrugList),
  #                                        function(i){primaryDrugExposure %>% dplyr::filter (subjectId == subjectId)})
  # if(length(primaryDrugList)>=2){for(i in 2:length(primaryDrugList))
  # {primaryDrugConditionPassed = {primaryDrugExposureOneSubject[[i]]<-subset(primaryDrugExposureOneSubject[[1]],DRUG_EXPOSURE_START_DATE %in% primaryDrugExposureOneSubject[[i]]$DRUG_EXPOSURE_START_DATE)}
  # }} else {primaryDrugConditionPassed = primaryDrugExposureOneSubject[[1]]}
  
  #indexDateList <- primaryDrugConditionPassed %>% select(DRUG_EXPOSURE_START_DATE,DRUG_EXPOSURE_END_DATE)
  
  indexDateList <- primaryDrugExposure %>% filter(subjectId == targetSubjectId) %>% dplyr::select(subjectId,drugExposureStartDate,drugExposureEndDate)
  
  indexDateList <- unique(indexDateList)
  ## Checking all drug condition
  ### The drug observation period is from the index date to the date as long as drug Observation Date.
  ### Secondary drug should be in the range of drug observation period and eliminatory drug should not be in.
  if(sum(!is.na(indexDateList))!=0){
    drugExamPassedDate <-c()
    drugExamPassedStartDate <- c()
    drugExamPassedEndDate <- c()
    
    if(length(secondaryDrugConceptIdList)!=0){
      secondaryDrugExposureOneSubject <-lapply(1:length(secondaryDrugConceptIdList),
                                               function(i){ secondaryDrugExposure[[i]] %>% filter (SUBJECT_ID == targetSubjectId)})
    }
    
    if(length(excludingDrugConceptIdList)!=0){
      excludingDrugExposureOneSubject <-lapply(1:length(excludingDrugConceptIdList),
                                               function(i){excludingDrugExposure[[i]] %>% filter (SUBJECT_ID == targetSubjectId)})
    }

    for(x in 1:nrow(indexDateList)){
      resultin<-list()
      resultout<-list()
      endDateList <-list()
      if(length(secondaryDrugConceptIdList)!=0){
        for(i in 1:length(secondaryDrugConceptIdList)){
          incheck <- dplyr::filter(secondaryDrugExposureOneSubject[[i]],between(DRUG_EXPOSURE_START_DATE,indexDateList[x,3],indexDateList[x,3]+drugInspectionDate))
          resultin[[i]] <-incheck[1,3]
          suppressWarnings(endDateList[[i]] <- max(incheck$DRUG_EXPOSURE_END_DATE))
        }
      }
      ##
      if(length(excludingDrugConceptIdList)!=0){
        for(i in 1:length(excludingDrugConceptIdList)){
          outcheck <- dplyr::filter(excludingDrugExposureOneSubject[[i]],between(DRUG_EXPOSURE_START_DATE,indexDateList[x,3],indexDateList[x,3]+drugInspectionDate))
          resultout[[i]] <- outcheck[1,3]
        }
      }else{
        resultout[1]<-NA}
      
      if(sum(is.na(resultin))==0 & sum(!is.na(resultout))==0){
        drugExamPassedStartDate[x]<- indexDateList[x,2]
        if(!is.null(drugExamPassedStartDate)){
          endDate<-as.Date(max(do.call(rbind,endDateList)),origin="1970-01-01")
          suppressWarnings(drugExamPassedEndDate[x]<- max(indexDateList[x,2],endDate))
        }
      }
    }
    if(!is.null(drugExamPassedStartDate)){
      drugExamPassedDate <- data.frame(drugExamPassedStartDate,drugExamPassedEndDate)
      drugExamPassedDate <- na.omit(drugExamPassedDate)}else{drugExamPassedDate <- c()}
  } else{
    drugExamPassedDate <- c()}
  
}
