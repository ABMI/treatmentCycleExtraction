## Drug condition setting :

### Primary drugs should be in first day of regimen cycle.
### Secondary drugs are rest of drugs in your targeting regimen. Not necessary for excuting but recommanded.
### Eliminatory drugs are drugs that distracting your targeting regimen. Not necessary for excuting.
### Multiple concept_id available and drug name is not necessary for excuting.

## Details for connecting to the server:
#install.packages('ParallelLogger')

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL)

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-'scratch.dbo'
cohortTable <-'cohort'
cdmDatabaseSchema <- 'cdm_Database_Schema.dbo'
vocaDatabaseSchema <- 'voca_Database_Schema.dbo'

targetRegimenConceptIds <- c(35806407,
                             35806419)

## Create the cohort for treatmentCycleExtraction

conceptIdSet <- c(443384,4181344,443381,443390,4180792,4180791,443382,4180790,443391,435754,443383,4089661)
targetCohortId <- 10000

createCohort(createCohortTable = FALSE,
             connectionDetails = connectionDetails,
             oracleTempSchema = NULL,
             cdmDatabaseSchema = cdmDatabaseSchema,
             cohortDatabaseSchema = cohortDatabaseSchema,
             vocabularyDatabaseSchema = cdmDatabaseSchema,
             cohortTable = cohortTable,
             conceptIdSet = conceptIdSet,
             includeConceptIdSetDescendant = TRUE,
             targetCohortId = targetCohortId
)


## The number of cores in use
maxCores <- 4


data<-generateEpisodeTable(targetRegimenConceptIds,
                           connectionDetails,
                           cohortTable,
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           targetCohortId,
                           maxCores)

## Execute_____________________________________________##

#extractRegimens(connectionDetails,
 #               regimenLists,
                # includeDescendant = includeDescendant,
                # outofCohortPeriod = outofCohortPeriod,
#                cohortDatabaseSchema = cohortDatabaseSchema,
 #               cohortTable = cohortTable,
  #              targetCohortId = targetCohortId,
   #             cdmDatabaseSchema = cdmDatabaseSchema,
    #            oraleTempTable = NULL,
     #           returnData = T,
      #          insertOncologyDatabase=F,
       #         oncologyDatabaseSchema = oncologyDatabaseSchema,
        #        customEpisodeTableName = c("EPISODE"),
         #       customEpisodeEventTableName = c("EPISODE_EVENT"),
          #      deleteExistingRecordsInTable = F,
                # primaryDrugConceptIdList = primaryDrugConceptIdList,
                # secondaryDrugConceptIdList = secondaryDrugConceptIdList,
                # excludingDrugConceptIdList= excludingDrugConceptIdList,
                # regimenName = regimenName,
                # regimenConceptId =regimenConceptId
#)

##_____________________________________________________##
#path <- location of result episode_table 
#dir <- ("path")
#file_list <- list.files(dir)
#data <- data.frame()
#for (file in file_list) {
#        print(file)
#        temp <- read.csv(paste(dir, file, sep = "\\"),header = TRUE, sep=",",stringsAsFactors = FALSE)
#        data <- rbind(data,temp)
#}
#head(data)

## Insert episode table

##connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
#server=Sys.getenv("PDW_SERVER"),
#schema='cdmDatabaseSchema',
#user=NULL,
#password=NULL)


##conn <- DatabaseConnector::connect(connectionDetails)
##DatabaseConnector::insertTable(conn, "FOLFOX", data, createTable = TRUE, progressBar = TRUE )
##DatabaseConnector::disconnect(conn)


