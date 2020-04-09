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

#' Extract chemotherapy records for target regimen.
#' @param targetParameter
#' @param connection
#' @param cohortTable
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param targetCohortId
#' @param maxCores Number of cores in clusterApply
#' @keywords chemotherapy records
#' @return chemotherapy records for specific regimen
#' @export
chemotherapyRecordsExtraction <- function(targetParameter,
                                          connection,
                                          cohortTable,
                                          cdmDatabaseSchema,
                                          cohortDatabaseSchema,
                                          targetCohortId,
                                          maxCores
)
{
  if (class(targetParameter)!="regimenLists") stop ("parameters should be regimenLists type")

  # Define parameters
  regimenConceptId <- parameters$regimenConceptId
  regimenName <- parameters$regimenName
  includeDescendant <- parameters$includeDescendant
  outofCohortPeriod <- parameters$outofCohortPeriod
  combinationCriteria <- parameters$combinationCriteria
  gapDateBetweenCycle <- parameters$gapDateBetweenCycle
  gapDateAfter <- parameters$gapDateAfter
  gapDateBefore <- parameters$gapDateBefore
  primaryConceptIdList <- parameters$primaryConceptIdList
  secondaryConceptIdList <- parameters$secondaryConceptIdList
  excludingConceptIdList <- parameters$excludingConceptIdList

  # Primary records
  primaryConceptRecords <- DrugExposureInCohort(connection,
                                                cohortTable,
                                                includeDescendant,
                                                outofCohortPeriod,
                                                cdmDatabaseSchema,
                                                cohortDatabaseSchema,
                                                targetConceptIds = primaryConceptIdList,
                                                targetCohortId)

  # Secondary records
  cluster <- ParallelLogger::makeCluster(numberOfThreads = maxCores)
  secondaryConceptRecords <- ParallelLogger::clusterApply(cluster,
                                                          secondaryConceptIdList,
                                                          DrugExposureInCohort,
                                                          connection,
                                                          cohortTable,
                                                          includeDescendant = TRUE,
                                                          outofCohortPeriod = TRUE,
                                                          cdmDatabaseSchema,
                                                          cohortDatabaseSchema,
                                                          targetCohortId)
  ParallelLogger::stopCluster(cluster)

  # Exclude records
  if(length(excludingConceptIdList)==0){excludingConceptRecords <- NULL}else{
    excludingConceptRecords <- DrugExposureInCohort(connection,
                                                    cohortTable,
                                                    includeDescendant,
                                                    outofCohortPeriod,
                                                    cdmDatabaseSchema,
                                                    cohortDatabaseSchema,
                                                    targetConceptIds = excludingConceptIdList,
                                                    targetCohortId)
  }

  # Extraction
  data <- lapply(unique(primaryConceptRecords$subjectId),function(x){
    try(gapDateExamination(x,
                           primaryConceptRecords,
                           secondaryConceptRecords,
                           excludingConceptRecords,
                           combinationCriteria,
                           secondaryConceptIdList,
                           excludingConceptIdList,
                           gapDateBetweenCycle,
                           gapDateBefore,
                           gapDateAfter,
                           regimenConceptId)
    )
    }
    )

  data <- na.omit(data.table::rbindlist(data))

  return(data)
}
