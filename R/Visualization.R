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
#' @import dplyr
#' @import superheat
#' @import networkD3
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
#' @import collapsibleTree
#' @export
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
#' @export 
regimenHeatmap<-function(episodeTableFromDatabase,
                         visualizationTargetRegimenId = NULL,
                         heatmapInRatio = TRUE,
                         maximumCycleNumber = NULL,
                         colors){
  
  if(is.null(visualizationTargetRegimenId)){visualizationTargetRegimenId<-unique(episodeTableFromDatabase$episodeSourceConceptId)}
  
  totalDistribution <-data.table::rbindlist(
    lapply(visualizationTargetRegimenId,function(episodeSourceConceptId){
      targetRegimenDistributionTable<-distributionTable(episodeTable=episodeTableFromDatabase,
                                                        targetEpisodeConceptId=episodeSourceConceptId)
      names(targetRegimenDistributionTable) <- c('cycle','n','ratio','conceptName')
      return(targetRegimenDistributionTable)})
  )
  if(!is.null(maximumCycleNumber)){
    totalDistribution <- subset(totalDistribution,cycle <= maximumCycleNumber)
  }
  
  if(heatmapInRatio){
    totalDistribution <- as_tibble(totalDistribution) %>% select(cycle, conceptName, ratio)
    class(totalDistribution$ratio) = "dbl"
    plotdata <- tidyr::spread(totalDistribution, cycle, ratio)
  }else{totalDistribution <- as_tibble(totalDistribution) %>% select(cycle, conceptName, n)
  class(totalDistribution$n) = "dbl"
  plotdata <- tidyr::spread(totalDistribution, cycle, n)}
  
  # 
  plotdata <- as.data.frame(plotdata)
  plotdata[is.na(plotdata)] <- 0
  row.names(plotdata) <- plotdata$conceptName
  plotdata$conceptName <- NULL
  sort.order <- order(plotdata$"12")
  heatmap<-superheat::superheat(plotdata,
                                scale = FALSE,
                                left.label.text.size=3,
                                left.label.size = 0.3,
                                bottom.label.text.size=3,
                                bottom.label.size = .05,
                                heat.pal = colors,
                                heat.pal.values = c(seq(0,0.3,length.out = 8),1),
                                order.rows = sort.order,
                                title = "Repeated cycle number in each regimen")
  return(heatmap)
}
#'@export generateHeatmap
generateHeatmap <- function(connectionDetails,
                            vocaDatabaseSchema,
                            oncologyDatabaseSchema,
                            episodeTable,
                            visualizationTargetRegimenId = NULL,
                            heatmapInRatio = TRUE,
                            maximumCycleNumber = NULL,
                            colors){
  episodeTableFromDatabase<- episodeTableForVisualization(connectionDetails,
                                                          vocaDatabaseSchema,
                                                          oncologyDatabaseSchema,
                                                          episodeTable)
  
  regimenHeatmap(episodeTableFromDatabase,
                 visualizationTargetRegimenId,
                 heatmapInRatio,
                 maximumCycleNumber,
                 colors)
}
#'@export sankeyFromEpisode
sankeyFromEpisode<-function(connectionDetails,
                            vocaDatabaseSchema,
                            oncologyDatabaseSchema,
                            episodeTable,
                            sankeyTargetRegimen,
                            surgeryConceptId,
                            regimenChangeNumber,
                            regimenMinimumChangeNumber,
                            surgeryName,
                            gapDatesInTherapy){#call episode record
  episodeTableFromDatabase<- episodeTableForVisualization(connectionDetails,
                                                          vocaDatabaseSchema,
                                                          oncologyDatabaseSchema,
                                                          episodeTable)
  
  if(is.null(sankeyTargetRegimen)){sankeyTargetRegimen<-unique(episodeTableFromDatabase$episodeSourceConceptId)}
  
  regimenSankeyData<-episodeTableFromDatabase %>% select(personId,episodeSourceConceptId,conceptName,episodeStartDatetime,episodeNumber) %>% subset(episodeNumber == 1) %>% subset(episodeSourceConceptId %in% sankeyTargetRegimen)
  
  #call surgery record.
  
  surgeryRecord<-surgeryForSankey(connectionDetails,
                                  cdmDatabaseSchema,
                                  oncologyDatabaseSchema,
                                  episodeTable,
                                  surgeryConceptId)
  
  surgeryRecord$surgery <- surgeryName
  surgeryRecord$episodeNumber <- 1
  surgeryRecord<-surgeryRecord %>% select(personId,procedureConceptId,surgery,procedureDate,episodeNumber)
  colnames(surgeryRecord) <- c('personId','episodeSourceConceptId','conceptName','episodeStartDatetime','episodeNumber')
  ##
  sankeyData<-rbind(regimenSankeyData,unique(surgeryRecord))%>% group_by(personId,episodeStartDatetime,conceptName) %>% arrange(personId,episodeStartDatetime,conceptName)
  
  ##
  sankeyData<-sankeyData %>% group_by(personId) %>% mutate(lag = lag(conceptName)) %>% subset(is.na(lag) |conceptName !=lag)
  sankeyData<-as.data.frame(sankeyData)
  ##
  sankeyData<-data.table::rbindlist(lapply(unique(sankeyData$personId),function(targetPersonId){
    sankeyDataRecon <-data.frame()
    targetSankeyData<-sankeyData %>% subset(personId == targetPersonId)
    sankeyDataRecon<-rbind(sankeyDataRecon,targetSankeyData[1,])
    
    if(nrow(targetSankeyData)>=2){
      for(x in 2:nrow(targetSankeyData)){
        if(as.integer(targetSankeyData[x,4]-sankeyDataRecon[nrow(sankeyDataRecon),4])>gapDatesInTherapy){
          sankeyDataRecon <-rbind(sankeyDataRecon,targetSankeyData[x,])}else{
            sortNames<-sort(c(targetSankeyData[x,3],sankeyDataRecon[nrow(sankeyDataRecon),3]))
            sankeyDataRecon[nrow(sankeyDataRecon),3]<-paste0(sortNames,collapse = '/')
          }
      } 
    }
    return(sankeyDataRecon)
  }
  ))
  ##
  sankeyData<-sankeyData%>% group_by(personId) %>% mutate(rowNum = 1:n()) %>% select(personId,conceptName,rowNum)
  
  if(is.null(regimenChangeNumber)){regimenChangeNumber<-max(sankeyData$rowNum)}
  data<-sankeyData  %>% subset(rowNum <= regimenChangeNumber)
  #
  if(is.null(regimenMinimumChangeNumber)){regimenMinimumChangeNumber<-0}
  sankeyDataOverNumber<-data %>% group_by(personId) %>% summarise(n=n()) %>% subset(n >= regimenMinimumChangeNumber)
  
  data<-data %>% subset(personId %in% sankeyDataOverNumber$personId) 
  data<-as.data.frame(data)
  ##For minimunChangeNumber working 
  if(regimenMinimumChangeNumber!=regimenChangeNumber){
  dataUniquePersonId<-data %>% select(personId) %>% unique()
  rowNumber<-rep(1:regimenChangeNumber,nrow(dataUniquePersonId))
  dataUniquePersonId<-rep(dataUniquePersonId$personId,regimenChangeNumber)
  paddingRowNum<-data.frame(dataUniquePersonId) %>% group_by(dataUniquePersonId) %>% arrange(dataUniquePersonId)
  paddingRowNum<-as.data.frame(paddingRowNum)
  paddingRowNum$rowNum <-rowNumber
  colnames(paddingRowNum) <- c('personId','rowNum')
  data<-merge(data,paddingRowNum,by=c("personId","rowNum"),all=TRUE)}
  
  
  ##links
  pasteConceptName<-paste(data$rowNum,data$conceptName,sep = '_')
  dataPasteConceptName<-cbind(data,pasteConceptName)
  ##
  conceptNameList<-sort(unique(pasteConceptName))
  conceptNameNumber<-seq(0,length(conceptNameList)-1)
  conceptNameIndexTable<-data.frame(conceptNameList=conceptNameList,conceptNameNumber=conceptNameNumber)
  indexedData<-left_join(dataPasteConceptName,conceptNameIndexTable,by=c('pasteConceptName'=
                                                                           'conceptNameList'))
  ##
  reshapedData<-reshape2::dcast(indexedData,personId~rowNum,value.var="conceptNameNumber")
  regimenNumber<-unlist(lapply(1:regimenChangeNumber,function(x){paste('Regimen',x,sep='_')}))
  colnames(reshapedData) <- c('personId',regimenNumber)
  reshapedData<-as.data.frame(reshapedData)
  ##ratio
  
  ratioTable<-data.table::rbindlist(lapply(1:regimenChangeNumber,function(x){
    ratioData <- data%>%subset(rowNum == x) %>% group_by(conceptName) %>% summarise(total = n())
    ratioData$conceptName<-paste(x,ratioData$conceptName,sep='_')
    ratioData$ratio <- round(ratioData$total/sum(ratioData$total)*100,digit =1)
    return(ratioData)
  }))
  
  #generate links
  links<-data.table::rbindlist(lapply(2:regimenChangeNumber,function(x){
    source<-reshapedData[,x]
    target<-reshapedData[,x+1]
    links<-data.frame(source,target)
    links<-links %>% group_by(source,target)%>%  summarise(freq=n())
    return(links)
  }))
  links<-as.data.frame(links)
  
  #nodes
  nodesNameIndex<-unique(indexedData %>% select(pasteConceptName,conceptName))
  nodesNameIndex$pasteConceptName<-as.character(nodesNameIndex$pasteConceptName)
  nodesNameIndexRatio<-left_join(nodesNameIndex,ratioTable,by=c("pasteConceptName"="conceptName"))
  nodesNameIndexRatio$label<-paste0(nodesNameIndexRatio$conceptName,',',' n = ',nodesNameIndexRatio$total,' (',nodesNameIndexRatio$ratio,"%)")
  conceptNameIndexTable$conceptNameList<-as.character(conceptNameIndexTable$conceptNameList)
  nodes<-left_join(conceptNameIndexTable,nodesNameIndexRatio,by=c('conceptNameList'='pasteConceptName')) %>% select(label)
  #sankey
  regimenSankey <- list(links=links,nodes=nodes)
  sankeyDiagram <- networkD3::sankeyNetwork(Links = regimenSankey$links, Nodes = regimenSankey$nodes, Source = "source",
                                            Target = "target", Value = "freq", NodeID = "label", fontSize = 12, nodeWidth = 30)
  
  return(sankeyDiagram)}

#' @export 
surgeryForSankey <- function(connectionDetails,
                             cdmDatabaseSchema,
                             oncologyDatabaseSchema,
                             episodeTable,
                             surgeryConceptId){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'with cte as(select * from @cdm_database_schema.procedure_occurrence where person_id in (select person_Id from @oncology_database_schema.@episode_table)
  )
  select * from cte where procedure_concept_Id in(@surgery_concept_id)
  union
  select * from cte where procedure_concept_Id in (
  select descendant_concept_Id from @cdm_database_schema.concept_ancestor where ancestor_concept_Id in (@surgery_concept_id))'
  sql <- SqlRender::render(sql,
                           cdm_database_schema = cdmDatabaseSchema,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable,
                           surgery_concept_id = surgeryConceptId)
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