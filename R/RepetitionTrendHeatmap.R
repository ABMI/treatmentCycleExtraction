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
#' Heatmap
#' Trends in regimen heatmap
#' @param episodeTableFromDatabase
#' @param visualizationTargetRegimenId
#' @param heatmapInRatio
#' @param maximumCycleNumber
#' @param colors
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param plotData
#' @keywords heatmap
#' @return repitition trend heatmap
#' @examples 
#' @import dplyr
#' @import superheat
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
#' @export 
regimenHeatmap<-function(episodeTableFromDatabase,
                         visualizationTargetRegimenId = NULL,
                         heatmapInRatio = TRUE,
                         maximumCycleNumber = NULL){
  
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
    plotData <- tidyr::spread(totalDistribution, cycle, ratio)
  }else{totalDistribution <- as_tibble(totalDistribution) %>% select(cycle, conceptName, n)
  class(totalDistribution$n) = "dbl"
  plotData <- tidyr::spread(totalDistribution, cycle, n)}
  
  # 
  plotData <- as.data.frame(plotData)
  plotData[is.na(plotData)] <- 0
  row.names(plotData) <- plotData$conceptName
  plotData$conceptName <- NULL
  return(plotData)
}
#'@export generateHeatmap
generateHeatmap <- function(connectionDetails,
                            vocaDatabaseSchema,
                            oncologyDatabaseSchema,
                            episodeTable,
                            visualizationTargetRegimenId = NULL,
                            heatmapInRatio = TRUE,
                            maximumCycleNumber = NULL){
  episodeTableFromDatabase<- episodeTableForVisualization(connectionDetails,
                                                          vocaDatabaseSchema,
                                                          oncologyDatabaseSchema,
                                                          episodeTable)
  
  plotData<-regimenHeatmap(episodeTableFromDatabase,
                 visualizationTargetRegimenId,
                 heatmapInRatio,
                 maximumCycleNumber)
return(plotData)
}

#'@export repetitionTrendHeatmap
repetitionTrendHeatmap<-function(plotData,colors){
  sort.order <- order(plotData$"1")
  heatmap<-superheat::superheat(plotData,
                              scale = FALSE,
                              left.label.text.size=3,
                              left.label.size = 0.3,
                              bottom.label.text.size=3,
                              bottom.label.size = .05,
                              heat.pal = colors,
                              heat.pal.values = c(seq(0,0.3,length.out = 8),1),
                              order.rows = sort.order,
                              title = "Repeated cycle number in each regimen")}