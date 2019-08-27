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
#' @keywords execute
#' @return histogram and count for subject distribution in repeated cycle,The number of distinct person_id in result, episode table in CSV file 
#' @export
#' @import dplyr
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
#' createCsv = createCsv)


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
                  createCsv = FALSE){
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

  # drug & cycle condition check
  data<-lapply(unique(primaryDrugExposure[[1]]$SUBJECT_ID),function(i){gapDateExamination(subjectId = i)})
  # Generate total cycle list

  cycleListInCohort<- na.omit(do.call(rbind, data))
  cycleListInCohort$cycle_start_date<-as.Date(cycleListInCohort$cycle_start_date,origin="1970-01-01")
  cycleListInCohort<- data.frame(cycleListInCohort)
  
  # Generate csv file
  if(createCsv){
    dir.create("result")
    write.csv(cycleListInCohort,file = "cycleExtraction.csv", row.name = T )
    }

  # select max cycle in each person
  aggregateCycle<-aggregate(cycleListInCohort$cycle_num,by = list(cycleListInCohort$SUBJECT_ID), max)

  # Histogram
  histogram <- ggplot2::ggplot(aggregateCycle,aes(x=x)) + geom_histogram(alpha = 0.5,fill = 'deep sky blue4') + geom_density(fill = "khaki3",alpha = 0.4) + theme_minimal()
  histogram <- plotly::ggplotly(histogram)

  # Total count
  totalCount<-length(unique(aggregateCycle$Group.1))

  # Count in each cycle number
  countEachcycle<-as.data.frame(aggregateCycle %>% group_by(x) %>% summarise(n = n()))
  names(countEachcycle) <- c('cycleNum','n')

  return(list(countEachcycle = countEachcycle,histogram = histogram,totalCount = totalCount))

}
