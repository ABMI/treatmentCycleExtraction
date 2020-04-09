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

#' Create cohort table, Insert target cohort

#' @param connection
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param vocaDatabaseSchema
#' @param cohortTable
#' @param includeConceptIdSetDescendant

#' @export
createCohortTable <- function(connection,
                              oracleTempSchema = NULL,
                              cohortDatabaseSchema = cdmDatabaseSchema,
                              cohortTable
){
  # Create Cohort table in your DB
    ParallelLogger::logInfo("Create table for the cohorts")
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateCohortTable.sql",
                                             packageName = "CancerTxPathway",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable)
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
}

#' @export
TargetCohortGeneration <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   vocaDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cdmDatabaseSchema,
                                   cohortTable,
                                   includeConceptIdSetDescendant = TRUE
){
  # Load CSV file including Concept_ids for target cohort
  pathToCsv <- system.file("csv", "Info_TargetCohort.csv", package = "CancerTxPathway")
  cohortInfo <- read.csv(pathToCsv, stringsAsFactors = F)

  ParallelLogger::logInfo("Insert cohort of interest into the cohort table")

  # Insert multiple target cohort
  for (i in 1:nrow(cohortInfo)) {

    # Load target Cohort_Definition_Id, Concept_Id
    targetConceptIdSet <- paste(strsplit(as.character(cohortInfo$conceptIds),';')[[i]],collapse = ',')
    targetCohortId <- cohortInfo$cohortId[i]

    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CohortGeneration.sql",
                                             packageName = "CancerTxPathway",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocaDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             include_descendant = includeConceptIdSetDescendant,
                                             condition_concept_ids = targetConceptIdSet,
                                             target_cohort_id = targetCohortId)
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
  }
}
