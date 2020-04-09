# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CancerTxPathway
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Drug exposure records

#' @param targetConceptIds
#' @param connection
#' @param cohortTable
#' @param includeDescendant
#' @param outofCohortPeriod
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param targetCohortId
#' @keywords Drug
#' @return Drug exposure

#' @export
# DrugExposureInCohort
DrugExposureInCohort <- function(targetConceptIds,
                                 connection,
                                 cohortTable,
                                 includeDescendant = TRUE,
                                 outofCohortPeriod = TRUE,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 targetCohortId){
  pathToSql <- system.file("sql/sql_server", "DrugExposureInCohort.sql", package = "CancerTxPathway")

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
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))

  return(result)
}
