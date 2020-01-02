#' regimenSetting
#' 
#' @param connectionDetails
#' @param connection
#' @param vocaDatabaseSchema
#' @param regimenConceptId
#' @param indexDrugConceptId
#' @keywords regimenSetting
#' @return drugList
#' @export
#' @import SqlRender
#' @import DatabaseConnector
#' @examples
#' regimenSetting()

# eliminatoryDrugExtraction
regimenSetting <- function(connectionDetails,
                           connection,
                           vocaDatabaseSchema=connectionDetails$schema,
                           regimenConceptId){
  connection <- DatabaseConnector::connect(connectionDetails)
  
  sql <- "select a.concept_id_2 from (select concept_id_2, row_number()over() as row_num from @voca_database_schema.concept_relationship where relationship_id = 'Has antineopl Rx' and concept_id_1 in (@regimen_concept_id)) a 
where row_num = 1"
  sql <- SqlRender::render(sql,
                           voca_database_schema = vocaDatabaseSchema,
                           regimen_concept_id=regimenConceptId)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  indexDrugConceptId <<- DatabaseConnector::querySql(connection, sql)[,1]
  sql <- "select concept_name from @voca_database_schema.concept where concept_id = @regimen_concept_id"
  sql <- SqlRender::render(sql,
                           voca_database_schema = vocaDatabaseSchema,
                           regimen_concept_id=regimenConceptId)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  
  regimenName<<-DatabaseConnector::querySql(connection, sql)[,1]
  
  primaryDrugList <<- list(indexDrugConceptId) 
  
  pathToSql <- system.file("sql/sql_server", "secondaryDrugSelection.sql", package = "treatmentCycleExtraction")
  sql <- SqlRender::readSql(pathToSql)
  sql <- SqlRender::render(sql,
                           voca_database_schema = vocaDatabaseSchema,
                           regimen_concept_id=regimenConceptId,
                           Index_drug_concept_id = indexDrugConceptId)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  secondaryDrugList <- DatabaseConnector::querySql(connection, sql)
  regimenDrugNo <- length(primaryDrugList)+length(secondaryDrugList$SECONDARY_DRUG_LIST)
  secondaryDrugList <<- lapply(secondaryDrugList$SECONDARY_DRUG_LIST,function(x){x})

  connection <- DatabaseConnector::connect(connectionDetails)
  pathToSql <- system.file("sql/sql_server", "eliminatoryDrugSelection.sql", package = "treatmentCycleExtraction")
  
  sql <- SqlRender::readSql(pathToSql)
  sql <- SqlRender::render(sql,voca_database_schema = vocaDatabaseSchema,
                           regimen_concept_id=regimenConceptId,
                           regimen_drug_no = regimenDrugNo)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  eliminatoryDrugList <- DatabaseConnector::querySql(connection, sql)
  if(length(eliminatoryDrugList$ELIMINATORYDRUG)!=0){
  eliminatoryDrugList <<- list(eliminatoryDrugList$ELIMINATORYDRUG)}
  else {eliminatoryDrugList <<- list()}
  DatabaseConnector::disconnect(connection)
  return(list(primaryDrugList=primaryDrugList,secondaryDrugList=secondaryDrugList,
                eliminatoryDrugList = eliminatoryDrugList,regimenName=regimenName))
  
}
