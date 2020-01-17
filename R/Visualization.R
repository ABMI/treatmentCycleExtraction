# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of treatmentCycleExtraction
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
#' Visualization
#' Visualization tool for episode table.
#' @param episodeTable,
#' @param episodeSourceConceptId
#' @keywords graph,visualization
#' @return Graph or table for episode table
#' @examples 
#' @export
distributionTable <- function(episodeTable,
                              episodeSourceConceptId){
  
  episode <- episodeTable %>% filter(episode_source_concept_id == episodeSourceConceptId)
  maxCycleNumberPerPerson<-aggregate(episode$episode_number,by = list(episode$person_id), max)
  colnames(maxCycleNumberPerPerson) <- c('person_id','Cycle_num')
  
  # Total count
  totalCount<-length(unique(maxCycleNumberPerPerson$person_id))
  
  # Count the number of patients in the value of each cycle number
  countCycle<-as.data.frame(maxCycleNumberPerPerson %>% group_by(Cycle_num) %>% summarise(n = n()))
  countCycle$'%'<-round(prop.table(table(maxCycleNumberPerPerson$Cycle_num))*100, digits = 1)
  sum<- sum(countCycle$n)
  sumName<- paste0('N','(','total=',sum,')')
  names(countCycle) <- c('Treatment cycle',sumName,'%')
  return(countCycle)}
