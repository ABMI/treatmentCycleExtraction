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
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param episodeSourceConceptId
#' @param targetEpisodeConceptId
#' @param targetRegimen
#' @keywords graph,visualization
#' @return Graph or table for episode table
#' @examples 
#' @import dplyr
#' @import superheat
#' @import networkD3
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
#' @import collapsibleTree
#' @export distributionTable
distributionTable <- function(episodeTable,
                              targetEpisodeConceptId){
  episode <- episodeTable %>% filter(episodeSourceConceptId == targetEpisodeConceptId)
  maxCycleNumberPerPerson<-aggregate(episode$episodeNumber,by = list(episode$personId), max)
  colnames(maxCycleNumberPerPerson) <- c('person_id','Cycle_num')
  
  # Total count
  totalCount<-length(unique(maxCycleNumberPerPerson$personId))
  
  # Count the number of patients in the value of each cycle number
  countCycle<-as.data.frame(maxCycleNumberPerPerson %>% group_by(Cycle_num) %>% summarise(n = n()))
  countCycle$'%'<-round(prop.table(table(maxCycleNumberPerPerson$Cycle_num))*100, digits = 1)
  sum<- sum(countCycle$n)
  sumName<- paste0('N','(','total=',sum,')')
  countCycle$conceptName <- unique(episode$conceptName)
  names(countCycle) <- c('Treatment cycle',sumName,'%','conceptName')
  return(countCycle)}

#' @export 
episodeTableForVisualization <- function(connectionDetails,
                                         vocaDatabaseSchema,
                                         oncologyDatabaseSchema,
                                         episodeTable){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'select episode.*,concept.concept_name from @oncology_database_schema.@episode_table episode
  left join @voca_database_schema.concept concept
  on episode.episode_source_concept_id = concept.concept_id
  where episode_concept_id in (32532) 
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

#' @export parameterTree
parameterTree<-function(targetRegimen){regimenJson<-readJson()
targetRegimenParameter<-regimenJson[unlist(lapply(regimenJson,`[`,'conceptId')) %in% targetRegimen][[1]]
targetRegimenParameterBasic<-targetRegimenParameter[names(targetRegimenParameter)%in%c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod')]

targetRegimenParameterDrug<-targetRegimenParameter[!names(targetRegimenParameter)%in%c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod')]

if(length(targetRegimenParameterBasic$invalidReason)==0){targetRegimenParameterBasic$invalidReason <-NA}
contents<-names(targetRegimenParameterBasic)
role<-unlist(targetRegimenParameterBasic)
parameters <- data.frame(contents,role) %>% subset(contents != 'regimenName')
regimenName<- data.frame(contents,role) %>% subset(contents == 'regimenName')
regimenName<-as.character(regimenName$role)
rownames(parameters) <- NULL

index <- !(names(targetRegimenParameter) %in% c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod'))
roleIndex<-lapply(targetRegimenParameter[index],`[`,"role")
primaryDrugList<-targetRegimenParameterDrug[unlist(roleIndex) == "primary"] 
primaryDrugDataframe<-do.call(rbind.data.frame, primaryDrugList)
secondaryDrugList<-targetRegimenParameterDrug[unlist(roleIndex) == "secondary"] 
secondaryDrugDataframe<-do.call(rbind.data.frame, secondaryDrugList)
excludedDrugList<-targetRegimenParameterDrug[unlist(roleIndex) == "excluded"] 
excludedDrugDataframe<-do.call(rbind.data.frame, excludedDrugList)

drug<-rbind(primaryDrugDataframe,secondaryDrugDataframe,excludedDrugDataframe)
drug$contents <- 'drug'         
Regimen<-merge(drug,parameters,all=TRUE)
Regimen$regimenName <- regimenName
tree<-collapsibleTree::collapsibleTree(
  Regimen,
  hierarchy = c("regimenName","contents","role","conceptName","conceptId","table"), 
  fill = c(
    
    "Black","seashell",rep("#00798c",length(unique(Regimen$contents))),rep("#d1495b",length(unique(na.omit(Regimen$role)))),rep("#edae49",length(unique(na.omit(Regimen$conceptName)))),rep("#66a182",length(unique(na.omit(Regimen$conceptName)))+length(unique(na.omit(Regimen$conceptName))))),
  collapsed = FALSE
)
return(tree)}