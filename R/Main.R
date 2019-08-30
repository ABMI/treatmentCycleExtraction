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
#' @keywords execute
#' @return histogram and count for subject distribution in repeated cycle,The number of distinct person_id in result, episode table in CSV file 
#' @export
#' @import plotly
#' @importFrom plotly last_plot
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
#' createCsv = createCsv
#' regimenName = 'FOLFOX')


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
                  regimenName = 'Unknown'){
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
  secondaryDrugExposure <<- DrugListinCohort(connectionDetails,
                                             connection,
                                             cohortTable = cohortTable,
                                             includeDescendant=includeDescendant,
                                             outofCohortPeriod = outofCohortPeriod,
                                             cohortDatabaseSchema=cohortDatabaseSchema,
                                             drugList=secondaryDrugList,
                                             targetCohortId=targetCohortId)
  eliminatoryDrugExposure <<- DrugListinCohort(connectionDetails,
                                               connection,
                                               cohortTable = cohortTable,
                                               includeDescendant=includeDescendant,
                                               outofCohortPeriod = outofCohortPeriod,
                                               cohortDatabaseSchema=cohortDatabaseSchema,
                                               drugList=eliminatoryDrugList,
                                               targetCohortId=targetCohortId)
  DatabaseConnector::disconnect(connection)

  # drug & cycle condition check (It will take some time)
  data<-lapply(unique(primaryDrugExposure[[1]]$SUBJECT_ID),function(i){gapDateExamination(subjectId = i)})
  # Generate total cycle list

  cycleListInCohort<- na.omit(do.call(rbind, data))
  cycleListInCohort$cycle_start_date<-as.Date(cycleListInCohort$cycle_start_date,origin="1970-01-01")
  cycleListInCohort<- data.frame(cycleListInCohort)

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
  aggregateCycle<-aggregate(cycleListInCohort$cycle_num,by = list(cycleListInCohort$SUBJECT_ID), max)
  colnames(aggregateCycle) <- c('SUBJECT_ID','Cycle_num')
  
  # Histogram
  histogram <- ggplot2::ggplot(aggregateCycle, aes(x=Cycle_num)) + geom_histogram(fill = '#0078FF',alpha = 0.8,binwidth = 1) + theme_bw() + labs(x="Number of cycle repetitions in treatment line", y="Number of patient") 
  histogram <- histogram + ggtitle("Histogram for treated cycle length") + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))
  histogram <- plotly::ggplotly(histogram)

  # Total count
  totalCount<-length(unique(aggregateCycle$SUBJECT_ID))

  # Count in each cycle number
  countEachcycle<-as.data.frame(aggregateCycle %>% group_by(Cycle_num) %>% summarise(n = n()))
  names(countEachcycle) <- c('cycleNum','n')

  return(list(countEachcycle = countEachcycle,histogram = histogram,totalCount = totalCount))

}
