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
#' NeutrophilGraph
#' Visualization tool for episode table.
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param cohortTable
#' @param neutrophilTargetRegimen
#' @param topNregimen
#' @param neutropeniaSeperationWithRatio
#' @param neutrophilCohortId
#' @keywords neutrophil,visualization
#' @return Graph or data for neutrophil analysis
#' @examples 
#' @import dplyr
#' @import tidyr
#' @export
treatmentLineFromEpisode <- function(connectionDetails,
                                     vocaDatabaseSchema,
                                     oncologyDatabaseSchema,
                                     episodeTable){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'select episode.*,concept.concept_name from @oncology_database_schema.@episode_table episode
  left join @voca_database_schema.concept concept
  on episode.episode_source_concept_id = concept.concept_id
  where episode_concept_id in (32531) 
  '
  sql <- SqlRender::render(sql,voca_database_schema = vocaDatabaseSchema,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  DatabaseConnector::disconnect(connection)
  return(result)
}

#' @export
neutropenia<-function(connectionDetails,
                      cohortDatabaseSchema,
                      cohortTable,
                      neutrophilCohortId){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- "select distinct * from @cohort_database_schema.@cohort where cohort_definition_id = @cohort_definition_id order by subject_id,cohort_start_date"
  sql <- SqlRender::render(sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort= cohortTable,
                           cohort_definition_id = neutrophilCohortId)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  DatabaseConnector::disconnect(connection)
  return(result)}

#' @export extractDataNeutrophilAnalysis
extractDataNeutrophilAnalysis<-function(connectionDetails,
                                        vocaDatabaseSchema,
                                        oncologyDatabaseSchema,
                                        cohortDatabaseSchema,
                                        episodeTable,
                                        cohortTable,
                                        neutrophilTargetRegimen,
                                        topNregimen,
                                        neutrophilCohortId){
  treatmentLineData<-treatmentLineFromEpisode(connectionDetails,
                                              vocaDatabaseSchema,
                                              oncologyDatabaseSchema,
                                              episodeTable)
  episodeTable<-episodeTableForVisualization(connectionDetails,
                                             vocaDatabaseSchema,
                                             oncologyDatabaseSchema,
                                             episodeTable)
  neutropeniaData<-neutropenia(connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable,
                               neutrophilCohortId)
  
  neutropeniaData$conceptName <- "ANCUnder2000"
  neutropeniaData$episodeNumPadd <- 0
  names(neutropeniaData) <- c('episodeSourceConceptId','personId','episodeStartDatetime','episodeEndDatetime','conceptName','episodeNumber')
  neutropeniaData$episodeSourceConceptId <- 0
  
  treatmentLineData <-treatmentLineData %>% subset(episodeSourceConceptId %in% neutrophilTargetRegimen)
  
  firstLineIndex<-treatmentLineData %>% group_by(personId) %>% arrange(personId,episodeStartDatetime) %>% mutate(rown = row_number()) %>% subset(rown == 1) %>% ungroup()%>% select(episodeId)
  
  firstLineCycle<-episodeTable %>% subset(episodeParentId %in% firstLineIndex$episodeId) %>% select(episodeSourceConceptId,personId,episodeStartDatetime,episodeEndDatetime,conceptName,episodeNumber)
  
  neutropeniaAfterChemo<-rbind(firstLineCycle,neutropeniaData) %>% group_by(personId)%>% arrange(personId,episodeStartDatetime) %>% mutate(lagReg = lag(episodeNumber)) %>% subset(is.na(lagReg)==FALSE & lagReg !=0) %>% subset(episodeSourceConceptId == 0)%>% arrange(personId,episodeStartDatetime) %>% mutate(n=row_number()) %>% subset(n == 1)%>% select(episodeSourceConceptId,personId,episodeStartDatetime,episodeEndDatetime,conceptName,episodeNumber,lagReg)%>% ungroup()
  
  patientsNumberInFirstChemo<-firstLineCycle %>% select(conceptName,personId) 
  patientsNumberInFirstChemo<-unique(patientsNumberInFirstChemo)%>% group_by(conceptName)%>% summarise(n=n()) %>% filter(rank(desc(n))<=topNregimen)
  
  firstLineCycle$lagReg <- 0
  
  personPerCycle<-firstLineCycle %>% group_by(personId) %>% mutate(cycle = row_number(episodeNumber)) %>% select(personId,conceptName,cycle) %>% ungroup %>% group_by(conceptName,cycle) %>% summarise(total=n())
  
  chemoAndNeutropenia<-rbind(neutropeniaAfterChemo,firstLineCycle) %>% arrange(personId,episodeStartDatetime,desc(episodeSourceConceptId))%>% group_by(personId)%>%  mutate(lagConceptName = lag(conceptName)) %>% ungroup()
  
  neutropeniaSeperation<-chemoAndNeutropenia %>% subset(episodeNumber == 0) %>% select(personId,lagReg,lagConceptName) %>% group_by(lagReg,lagConceptName) %>% summarise(n=n()) %>% subset(lagConceptName %in% patientsNumberInFirstChemo$conceptName) %>% ungroup() 
  
  neutropeniaSeperationWithTotal<-merge(personPerCycle,neutropeniaSeperation,by.x=c("cycle","conceptName"),by.y=c("lagReg","lagConceptName"),all=T) %>% arrange(conceptName,cycle)
  
  neutropeniaSeperationWithTotal[is.na(neutropeniaSeperationWithTotal)] <- 0
  
  neutropeniaSeperationWithRatio<-neutropeniaSeperationWithTotal %>% subset(is.na(total)==FALSE)%>% mutate(ratio= (n/total*100)) %>% subset(conceptName %in%patientsNumberInFirstChemo$conceptName)%>% select(cycle,conceptName,ratio) %>% arrange(conceptName,cycle)
  names(neutropeniaSeperationWithRatio) <- c("cycle","Regimen","ratio")
  max(neutropeniaSeperationWithRatio$cycle)
  return(neutropeniaSeperationWithRatio)}

#'@export plotNeutrophil
plotNeutrophil<-function(neutropeniaSeperationWithRatio){
  ggplot(neutropeniaSeperationWithRatio, aes(x=cycle, y= ratio, color=Regimen)) + geom_line(size=1)+
    geom_point()+
    scale_x_continuous(limits = c(1,16),
                       breaks = c(seq(1:16)),
                       label = c(seq(1:16))) +labs(fill="Regimen") +
    labs(title = expression(Absolute~neutrophil~count<2.0%*%10^9/L~Timing),
         subtitle = "1 - 16 cycle",
         caption = "*ratio : the number of ANC < 2000 patients at each cycle of the regimen / the number of patients treated each regimen as first-line therapy",
         y = "*ratio (%)",
         x = "cycle (n)") +
    scale_y_continuous(limits = c(0, 100
    )) + theme_minimal()+scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                                                     "#999999", "#0072B2", "#D55E00", "#CC79A7"))}