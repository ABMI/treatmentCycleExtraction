
## Execute_____________________________________________##

#extractRegimens(connectionDetails,

   #            
    #            oracleTempTable = NULL,
     #          
      #         
       #         oncologyDatabaseSchema = oncologyDatabaseSchema,
        #        customEpisodeTableName = c("EPISODE"),
         #       customEpisodeEventTableName = c("EPISODE_EVENT"),
         

######################################################################################
## Details for connecting to the server:
## Create the cohort for treatmentCycleExtraction

conceptIdSet <- c(443384,4181344,443381,443390,4180792,4180791,443382,4180790,443391,435754,443383,4089661)
targetCohortId <- 1234

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

## parameter setting

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL)


# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-'result_v27'
cohortTable <-'cohort'
cdmDatabaseSchema <- 'cdmpv531'
vocaDatabaseSchema <- 'hkocdm'
oncologyDatabaseSchema <- 'hkocdm'
episodeTable <- 'episode_test'
episodeEventTable <- 'episode_event_test'
createEpisodeAndEventTable <- TRUE

targetRegimenConceptIds <- c(35803688)

targetCohortId <- 272

## The number of cores in use
maxCores <- 4

episodeAndEpisodeEvent<-generateEpisodeTable(targetRegimenConceptIds,
                                             connectionDetails,
                                             cohortTable,
                                             cdmDatabaseSchema,
                                             cohortDatabaseSchema,
                                             targetCohortId,
                                             maxCores)

insertEpisodeToDatabase(connectionDetails,oncologyDatabaseSchema,episodeTable,episodeEventTable,createEpisodeAndEventTable,episodeAndEpisodeEvent)

