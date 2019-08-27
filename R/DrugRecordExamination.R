#' drugRecordExamination
#'
#' check each index dates of single subject whether drug conditions are satisfied or not.
#' @param subjectId : targeting subject_id in target cohort
#' @keywords primaryDrug, secondaryDrug,eliminatoryDrug
#' @return drug condition passed index dates list
#' @export
#' @import dplyr
#' @examples
#' drugRecordExamination(subjectId = 11111111)

# drugRecordExamination

drugRecordExamination<-function(subjectId){
  
  ## Dispense date of primary drug is index date
  ## Generate index date list in one person
  firstDayInCycle<- c()
  primaryDrugExposureOneSubject <-lapply(1:length(primaryDrugList),
                                         function(i){primaryDrugExposure[[i]] %>% dplyr::filter (SUBJECT_ID == subjectId)})
  
  if(length(primaryDrugList)>=2){for(i in 2:length(primaryDrugList))
  {primaryDrugConditionPassed = {primaryDrugExposureOneSubject[[i]]<-subset(primaryDrugExposureOneSubject[[1]],DRUG_EXPOSURE_START_DATE %in% primaryDrugExposureOneSubject[[i]]$DRUG_EXPOSURE_START_DATE)}
  }} else {primaryDrugConditionPassed = primaryDrugExposureOneSubject[[1]]}
  
  indexDateList<-primaryDrugConditionPassed %>% select(DRUG_EXPOSURE_START_DATE)
  
  ## Checking all drug condition
  ### The drug observation period is from the index date to the date as long as drug Observation Date.
  ### Secondary drug should be in the range of drug observation period and eliminatory drug should not be in.
  if(sum(!is.na(indexDateList))!=0){
    drugExamPassedDate <-c()
    
    if(length(secondaryDrugList)!=0){
      secondaryDrugExposureOneSubject <-lapply(1:length(secondaryDrugList),
                                               function(i){ secondaryDrugExposure[[i]] %>% filter (SUBJECT_ID == subjectId)})}
    
    if(length(eliminatoryDrugList)!=0){
      eliminatoryDrugExposureOneSubject <-lapply(1:length(eliminatoryDrugList),
                                                 function(i){ eliminatoryDrugExposure[[i]] %>% filter (SUBJECT_ID == subjectId)})}
    
    for(x in 1:length(indexDateList$DRUG_EXPOSURE_START_DATE)){
      resultin<-list()
      resultout<-list()
      if(length(secondaryDrugList)!=0){for(i in 1:length(secondaryDrugList)){resultin[[i]] <- dplyr::filter(secondaryDrugExposureOneSubject[[i]],between(DRUG_EXPOSURE_START_DATE,indexDateList[x,],indexDateList[x,]+drugObservationDate))[1,2]}}
      
      if(length(eliminatoryDrugList)!=0){for(i in 1:length(eliminatoryDrugList)){resultout[[i]] <- dplyr::filter(eliminatoryDrugExposureOneSubject[[i]],between(DRUG_EXPOSURE_START_DATE,indexDateList[x,],indexDateList[x,]+drugObservationDate))[1,2]}}else{resultout[1]<-NA}
      
      if(sum(is.na(resultin))==0 & sum(is.na(resultout))!=0){drugExamPassedDate[x]<-indexDateList[x,]}}
    drugExamPassedDate <- na.omit(drugExamPassedDate)
    data.frame(drugExamPassedDate)
  }else{drugExamPassedDate <- c()}}
