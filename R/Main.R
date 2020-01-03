#' Excute
#'
#' Excute the Study
#' @param connectionDetails An object of type \code{connectionDetails} as created using the
#'                          \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                          DatabaseConnector package.
#' @param connection
#' @param cohortTable The name of the table that will be created in the work database schema.
#'                    This table will hold the exposure and outcome cohorts used in this
#'                    study.
#' @param includeDescendant
#' @param outofCohortPeriod
#' @param cohortDatabaseSchema
#' @param primaryDrugList
#' @param secondaryDrugList
#' @param eliminatoryDrugList
#' @param targetCohortId
#' @param createCsv
#' @param targetCohortId
#' @param regimenName
#' @param colorInHistogram
#' @keywords execute
#' @return histogram and count for subject distribution in repeated cycle,The number of distinct person_id in result, episode table in CSV file 
#' @export
#' @import plotly
#' @import ggplot2
#' @examples
#' execute(connectionDetails,
#' connection,
#' cohortTable = cohortTable,
#' includeDescendant = includeDescendant,
#' outofCohortPeriod = outofCohortPeriod,
#' cohortDatabaseSchema = cohortDatabaseSchema,
#' primaryDrugList = primaryDrugList,
#' secondaryDrugList = secondaryDrugList,
#' eliminatoryDrugList= eliminatoryDrugList,
#' targetCohortId = targetCohortId,
#' createCsv = createCsv,
#' regimenName = 'FOLFOX',
#' colorInHistogram = FF8200)


execute<-function(connectionDetails,
                  connection,
                  cohortTable,
                  includeDescendant,
                  outofCohortPeriod,
                  cohortDatabaseSchema,
                  primaryDrugList,
                  secondaryDrugList,
                  eliminatoryDrugList,
                  targetCohortId,
                  createCsv = FALSE,
                  resultsSaveInFile = FALSE,
                  regimenName = 'Unknown',
                  colorInHistogram = 'FF8200',
                  regimenConceptId = regimenConceptId
                  ){
  # All drug list calling
  connection <- DatabaseConnector::connect(connectionDetails)
  primaryDrugExposure <<- DrugListinCohort(connectionDetails,
                                          connection,
                                          cohortTable = cohortTable,
                                          includeDescendant=includeDescendant,
                                          outofCohortPeriod = outofCohortPeriod,
                                          cohortDatabaseSchema=cohortDatabaseSchema,
                                          drugList=primaryDrugList,
                                          targetCohortId=targetCohortId)
  print('primary drug list loaded')
  secondaryDrugExposure <<- DrugListinCohort(connectionDetails,
                                             connection,
                                             cohortTable = cohortTable,
                                             includeDescendant=includeDescendant,
                                             outofCohortPeriod = outofCohortPeriod,
                                             cohortDatabaseSchema=cohortDatabaseSchema,
                                             drugList=secondaryDrugList,
                                             targetCohortId=targetCohortId)
  print('secondary drug list loaded')
  eliminatoryDrugExposure <<- DrugListinCohort(connectionDetails,
                                               connection,
                                               cohortTable = cohortTable,
                                               includeDescendant=includeDescendant,
                                               outofCohortPeriod = outofCohortPeriod,
                                               cohortDatabaseSchema=cohortDatabaseSchema,
                                               drugList=eliminatoryDrugList,
                                               targetCohortId=targetCohortId)
  print('eliminatory drug list loaded')
  DatabaseConnector::disconnect(connection)
 
 # drug & cycle condition check (It will take some time)
  data<-lapply(unique(primaryDrugExposure[[1]]$SUBJECT_ID),function(i){gapDateExamination(subjectId = i)})
  print('condition check finish')

  # Generate total cycle list
  cycleListInCohort<- na.omit(do.call(rbind, data))
  cycleListInCohort$cycle_start_date<-as.Date(cycleListInCohort$cycle_start_date,origin="1970-01-01")
  cycleListInCohort$cycle_end_date<-as.Date(cycleListInCohort$cycle_end_date,origin="1970-01-01")
  cycleListInCohort$episode_type_concept_id <-32545
  cycleListInCohort$episode_concept_id <-32532
  cycleListInCohort$episode_parent_id <-1
  cycleListInCohort$episode_object_concept_id <-32525
  cycleListInCohort$episode_id <-1
  cycleListInCohort$episode_source_value <-1
  cycleListInCohort$episode_source_value <-NA
  cycleListInCohort$episode_source_concept_id <-regimenConceptId
  names(cycleListInCohort) <- c('person_id',
                                'episode_start_datetime',
                                'episode_number',
                                'episode_end_datetime',
                                'episode_type_concept_id',
                                'episode_concept_id',
                                'episode_parent_id',
                                'episode_object_concept_id',
                                'episode_id',
                                'episode_source_value',
                                'episode_source_concept_id')
  cycleList <- data.frame(cycleListInCohort)
  cycleListInCohort <- cycleList[,c(9,1,6,2,4,7,3,8,5,10,11)]
  # Generate csv file
  if(createCsv){
    if(file.exists('result')){
      fileName <- paste0('./result/cycleExtraction_',regimenName,'_',targetCohortId,'.csv')
      if(!file.exists(fileName)){
        write.csv(cycleListInCohort,file = fileName, row.names = FALSE )}else{
          file.remove(fileName)
          write.csv(cycleListInCohort,file = fileName, row.names = FALSE )}
    }else{
      dir.create("result")
      fileName <- paste0('./result/cycleExtraction_',regimenName,'_',targetCohortId,'.csv')
      write.csv(cycleListInCohort,file = fileName, row.names = FALSE )
    }
  }

  # select max cycle in each person
  aggregateCycle<-aggregate(cycleListInCohort$episode_number,by = list(cycleListInCohort$person_id), max)
  colnames(aggregateCycle) <- c('person_id','Cycle_num')
  

  # Total count
  totalCount<-length(unique(aggregateCycle$person_id))
  
  # Count in each cycle number
  countEachcycle<-as.data.frame(aggregateCycle %>% group_by(Cycle_num) %>% summarise(n = n()))
  countEachcycle$'%'<-round(prop.table(table(aggregateCycle$Cycle_num))*100, digits = 1)
  sum<- sum(countEachcycle$n)
  sumName<- paste0('N','(','total=',sum,')')
  names(countEachcycle) <- c('Treatment cycle',sumName,'%')
  
  if(resultsSaveInFile){
    if(file.exists('result')){
      distributionFileName <- paste0('./result/distribution_',regimenName,'_',targetCohortId,'.csv')
      if(!file.exists(distributionFileName)){
        write.csv(countEachcycle,file = distributionFileName, row.names = FALSE )}else{
          file.remove(distributionFileName)
          write.csv(countEachcycle,file = distributionFileName, row.names = FALSE )}
    }else{
      dir.create("result")
      fileName <- paste0('./result/distribution_',regimenName,'_',targetCohortId,'.csv')
      write.csv(countEachcycle,file = distributionFileName, row.names = FALSE )
    }}
  # Histogram
  
  histogramColor <- paste0('#',colorInHistogram)
  histogram <- ggplot2::ggplot(aggregateCycle, aes(x=Cycle_num)) + geom_histogram(fill = histogramColor ,alpha = 0.8,binwidth = 1) + theme_bw() + labs(x="Treatment cycle", y="Number of patients") 
  histogram <- histogram + ggtitle("Histogram of treated cycle length") + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) + scale_x_continuous(breaks=seq(1:max(countEachcycle$`Treatment cycle`)))
  histogram <- plotly::ggplotly(histogram)
  
  if(resultsSaveInFile){
    if(file.exists('result')){
      histogramFileName <- paste0('./result/histogram_',regimenName,'_',targetCohortId,'.jpg')
      if(!file.exists(histogramFileName)){
        ggsave(histogramFileName, dpi = 500) }else{
          file.remove(histogramFileName)
          ggsave(histogramFileName, dpi = 500) }
    }else{
      dir.create("result")
      fileName <- paste0('./result/histogram_',regimenName,'_',targetCohortId,'.jpg')
      ggsave(histogramFileName, dpi = 500)
    }}
  return(list(countEachcycle = countEachcycle,histogram = histogram,totalCount = totalCount))
  
}
