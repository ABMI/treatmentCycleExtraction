#' gapDateExamination
#'
#' check cycle interval in the range of setting
#' @param subjectId : targeting subject_id in target cohort
#' @keywords gap date
#' @return cycle result list of single subject in cohort
#' @export
#' @import dplyr
#' @import data.table
#' @import drugRecordExamination
#' @examples
#' gapDateExamination(targetSubjectIdsubjectId = 11111111)
# gapDateExamination

gapDateExamination<-function(targetSubjectId,
                             primaryDrugExposure,
                             secondaryDrugExposure,
                             excludingDrugExposure,
                             drugInspectionDate,
                             secondaryDrugConceptIdList,
                             excludingDrugConceptIdList){
  allDrugPassed<-drugRecordExamination(targetSubjectId,
                                       primaryDrugExposure,
                                       secondaryDrugExposure,
                                       excludingDrugExposure,
                                       drugInspectionDate,
                                       secondaryDrugConceptIdList,
                                       excludingDrugConceptIdList)
  if(!is.null(allDrugPassed)){
    drugExamPassedStartDate<-allDrugPassed$drugExamPassedStartDate
    drugExamPassedDate<-as.data.frame(drugExamPassedStartDate)
    drugExamPassedDate$lagdate <- data.table::shift(drugExamPassedDate$drugExamPassedStartDate,fill = drugExamPassedDate$drugExamPassedStartDate[1])
    drugExamPassedDate$datediff <-drugExamPassedDate$drugExamPassedStartDate-drugExamPassedDate$lagdate
    drugExamPassedDate$datediff[1] <- 'first'
    drugExamPassedStartDate<-subset(drugExamPassedDate,datediff >= gapDateBetweenCycle-gapDateBefore|datediff == 'first')$drugExamPassedStartDate
    drugExamPassedDate<-as.data.frame(drugExamPassedStartDate)
    ## Generate first date list of cycle. From very first record of date list, until next cycle date cannot be found.
    
    gapDatePassedDate <- list()
    gapDatePassedDate[[1]]<-drugExamPassedDate[1,]
    
    x=1
    repeat{
      
      i=1
      repeat{
        
        minimumGapVariationDate <- gapDatePassedDate[[x]][i]+gapDateBetweenCycle-gapDateBefore
        maximumGapVariationDate <- gapDatePassedDate[[x]][i]+gapDateBetweenCycle+gapDateAfter
        
        gapDatePassedDate[[x]][i+1] <- dplyr::filter(drugExamPassedDate,between(drugExamPassedDate$`drugExamPassedStartDate`,minimumGapVariationDate,maximumGapVariationDate))[1,]
        
        if(is.na(gapDatePassedDate[[x]][i+1]))
        {gapDatePassedDate[[x+1]]<-subset(drugExamPassedDate,!drugExamPassedDate$`drugExamPassedStartDate` %in% unlist(gapDatePassedDate))[1,]}
        
        if(is.na(gapDatePassedDate[[x]][i+1])) break 
        i = i+1
      }
      
      if(is.na(gapDatePassedDate[[x+1]])) break
      x = x+1}
    
    gapDatePassedDate[[length(gapDatePassedDate)]] <- NULL
    
    ## Generate result of cycle extraction
    
    
    subjectCycleList<-lapply(1:length(gapDatePassedDate),function(i){
      cycle_start_date <- gapDatePassedDate[[i]]
      SUBJECT_ID <-c(targetSubjectId)
      cycle_num <- c(seq_along(cycle_start_date))
      cycle <- data.frame(SUBJECT_ID,cycle_start_date,cycle_num)
      cycle <- dplyr::left_join(cycle,allDrugPassed, by =c("cycle_start_date" = "drugExamPassedStartDate"))
      names(cycle) <- c('SUBJECT_ID','cycle_start_date','cycle_num','cycle_end_date')
      
      return(cycle)
    }
    )
    subjectCycleList<- rbindlist(subjectCycleList) 
    return(subjectCycleList)
  } else {
    cycle_start_date<-NA
    cycle_end_date <- NA
    cycle_num <-NA
    SUBJECT_ID <-NA
    cycle <- data.frame(SUBJECT_ID,cycle_start_date,cycle_num,cycle_end_date)
    return(cycle)
  }
}

