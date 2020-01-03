#' DrugExposureInCohort
#'
#' This function allows you to extract drug exposure records of target cohort
#' @param connectionDetails
#' @param cohortTable
#' @param includeDescendant
#' @param outofCohortPeriod
#' @param cohortDatabaseSchema
#' @param targetConceptIds
#' @paramtargetCohortId
#' @keywords drug list
#' @return list form drug exposure data list
#' @export
#' @examples
#' DrugListinCohort(connectionDetails,connection,cohortTable,includeDescendant,outofCohortPeriod,cohortDatabaseSchema,drugList,targetCohortId)

# DrugExposureInCohort
DrugExposureInCohort <- function(connection,
                                  cohortTable,
                                  includeDescendant = TRUE,
                                  outofCohortPeriod = TRUE,
                                 cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  targetConceptIds,
                                  targetCohortId){
  pathToSql <- system.file("sql/sql_server", "DrugExposureInCohort.sql", package = "treatmentCycleExtraction")

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
  
  return(result)
}
# Get list of DrugExposureInCohort
DrugListinCohort <- function(connection,
                             cohortTable,
                             includeDescendant,
                             outofCohortPeriod,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             drugList,
                             targetCohortId){
  result<-lapply(drugList,function(targetConceptIds){DrugExposureInCohort(connection,
                                                                          cohortTable,
                                                                          includeDescendant,
                                                                          outofCohortPeriod,
                                                                          cdmDatabaseSchema,
                                                                          cohortDatabaseSchema,
                                                                          targetConceptIds,
                                                                          targetCohortId)})
  return(result)
  }
