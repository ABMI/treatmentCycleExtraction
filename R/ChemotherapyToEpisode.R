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

#' Transform regimen records to episode table.
#' @param chemotherapyRecords
#' @import dplyr
#' @import tidyr
#' @return list of episode table and episode event table
#' @export
chemotherapyToEpisode<- function(chemotherapyRecords){
  # Chemotherapy records to Episode
  classIndex <- class(chemotherapyRecords)
  chemotherapyRecords$CYCLE_START_DATE<-as.Date(chemotherapyRecords$CYCLE_START_DATE,origin="1970-01-01")
  chemotherapyRecords$CYCLE_END_DATE<-as.Date(chemotherapyRecords$CYCLE_END_DATE,origin="1970-01-01")
  chemotherapyRecords$episode_type_concept_id <-32545
  chemotherapyRecords$episode_concept_id <-32532
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_concept_id <- 32531
  chemotherapyRecords$episode_parent_id <-NA
  chemotherapyRecords$episode_object_concept_id <-32525
  chemotherapyRecords$episode_id <- seq(nrow(chemotherapyRecords))
  chemotherapyRecords$episode_source_value <-NA
  class(chemotherapyRecords$episode_parent_id)<- 'integer'
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_parent_id <- chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_id
  chemotherapyRecords<-chemotherapyRecords %>% group_by(SUBJECT_ID) %>% fill(episode_parent_id,.direction = c("up"))
  class(chemotherapyRecords) <- classIndex
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$episode_parent_id <- NA
  chemotherapyRecords[chemotherapyRecords$CYCLE_NUM == 0]$CYCLE_NUM <- NA

  # Colnames in Episode
  names(chemotherapyRecords) <- c('person_id',
                                  'episode_start_datetime',
                                  'episode_number',
                                  'episode_source_concept_id',
                                  'episode_end_datetime',
                                  'episode_item',
                                  'episode_type_concept_id',
                                  'episode_concept_id',
                                  'episode_parent_id',
                                  'episode_object_concept_id',
                                  'episode_id',
                                  'episode_source_value')

  chemotherapyRecords <- data.frame(chemotherapyRecords)
  episodeRecords <- chemotherapyRecords[,c(11,1,8,2,5,9,3,10,7,12,4,6)]
  episode <- episodeRecords[,c(1:11)]
  ParallelLogger::logInfo("Episode generated")
  # Episode_event generation
  episodeEventRecords <- data.table::data.table()

  episodeRecords <- episodeRecords %>% subset(episode_concept_id == 32532)
  episodeDrug <- episodeRecords[,c(1,12)]

  n <- nrow(episodeDrug)

  for (i in 1:n){

    nameIndex <- as.character(episodeDrug[i, 1])
    itemIndex <- as.character(episodeDrug[i, 2])

    itemIndexSplitTemp <- data.frame(strsplit(itemIndex, split = '_'))
    episodeDrugTemp <- data.frame(cbind(nameIndex, itemIndexSplitTemp))

    names(episodeDrugTemp) <- c("episode_id", "drug_exposure_id")

    episodeEventRecords <- rbind(episodeEventRecords, episodeDrugTemp)
  }

  episodeEventRecords$visit_occurrence_id <- NA
  episodeEventRecords$condition_occurrence_id <- NA
  episodeEventRecords$procedure_occurrence_id <- NA
  episodeEventRecords$device_exposure_id <- NA
  episodeEventRecords$measurement_id <- NA
  episodeEventRecords$specimen_id <- NA
  episodeEventRecords$observation_id <- NA
  episodeEventRecords$note_id <- NA
  episodeEventRecords$cost_id <- NA

  episodeEvent <- episodeEventRecords[,c(1,3,4,5,2,6,7,8,9,10,11)]
  ParallelLogger::logInfo("Episode_event generated")

  return(list(episode,episodeEvent))
}
