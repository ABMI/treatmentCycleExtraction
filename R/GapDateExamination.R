#' gapDateExamination
#'
#' check cycle interval in the range of setting
#' @param subjectId : targeting subject_id in target cohort
#' @keywords gap date
#' @return cycle result list of single subject in cohort
#' @export
#' @import dplyr
#' @examples
#' gapDateExamination(subjectId = 11111111)

# gapDateExamination

gapDateExamination<-function(subjectId){

  drugExamPassedDate <-as.data.frame(drugRecordExamination(subjectId))

  ## Generate first date list of cycle. From very first record of date list, until next cycle date cannot be found.

  firstDayInCycle <- c()
  firstDayInCycle[1]<-drugExamPassedDate[1,]
  for (i in 1:(maximumCycleNumber-1)){
    firstDayInCycle[i+1] <- dplyr::filter(drugExamPassedDate,
                                          between(drugExamPassedDate,firstDayInCycle[i]+gapDateBetweenCycle-gapDateVariation,firstDayInCycle[i]+gapDateBetweenCycle+gapDateVariation))[1,]
    if(is.null(firstDayInCycle[i+1])==TRUE) break;}

  ## Generate result of cycle extraction

  if(sum(!is.na(firstDayInCycle))!=0){
    firstDayInCycle <- data.frame(firstDayInCycle)
    firstDayInCycle <- firstDayInCycle %>% filter(!is.na(firstDayInCycle))
    cycle_start_date <- as.Date(firstDayInCycle$firstDayInCycle,origin="1970-01-01")
    SUBJECT_ID <-c(subjectId)
    cycle_num <- c(seq_along(cycle_start_date))
    cycle <- data.frame(SUBJECT_ID,cycle_start_date,cycle_num)
  } else {
    cycle_start_date<-NA
    cycle_num <-NA
    SUBJECT_ID <-NA
    cycle <- data.frame(SUBJECT_ID,cycle_start_date,cycle_num)
  }
}
