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

#' Create Episode / Episode_event table

#' @param connection
#' @param oracleTempSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param episodeEventTable

#' @export
createEpisodeTable <- function(connection,
                               oracleTempSchema,
                               oncologyDatabaseSchema,
                               episodeTable,
                               episodeEventTable
){
  # Create Episode
  ParallelLogger::logInfo("Create Episode table")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateEpisodeTable.sql",
                                           packageName = "CancerTxPathway",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           oncology_database_schema = oncologyDatabaseSchema,
                                           episode_table = episodeTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)

  # Create Episode_event
  ParallelLogger::logInfo("Create Episode_event table")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateEpisodeEventTable.sql",
                                           packageName = "CancerTxPathway",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           oncology_database_schema = oncologyDatabaseSchema,
                                           episode_event_table = episodeEventTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
}
