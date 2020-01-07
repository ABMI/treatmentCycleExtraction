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

## 1. Manual drug condition
primaryDrugConceptIdList <- list(955632)
names(primaryDrugConceptIdList) <- c('Fluorouracil')

secondaryDrugConceptIdList <- list(c(1318011),c(1388796,19111620))
names(secondaryDrugConceptIdList) <- c('Oxaliplatin','leucovorin')

excludingDrugConceptIdList <- list(1397141)
names(excludingDrugConceptIdList) <- c('Bevacizumab')

regimenName <- 'FOLFOX'
regimenConceptId <- 35806596

## 2. regimen setting when you do not want to ingredient manually
regimenConceptId <- 35806596
regimenSetting(connectionDetails,
               connection,
               vocaDatabaseSchema,
               regimenConceptId = regimenConceptId)

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

## Include descendants of drugs in list or not:
includeDescendant <- TRUE

## Drug records could be out of cohort period or not :
outofCohortPeriod <- FALSE

## Period of observing secondary drug from primary drug used date :
drugInspectionDate <- 7

## Each cycle start date should be apart as gap date, and gap date can be in range of +- date as gap date variation :
gapDateBetweenCycle <-20
gapDateAfter<-15  #+
gapDateBefore<-10  #-

## The number of cores in use
maxCores <- 4

## Generate all cycle records list of cohort as csv file in working directory 
## It will be treatment episode table later version...
createCsv <- TRUE
resultsSaveInFile <- TRUE ## save histogram and distribution table
colorInHistogram <- 'FF8200'



## Execute_____________________________________________##

execute(connectionDetails,
        connection,
        cohortTable = cohortTable,
        includeDescendant = includeDescendant,
        outofCohortPeriod = outofCohortPeriod,
        cohortDatabaseSchema = cohortDatabaseSchema,
        primaryDrugConceptIdList = primaryDrugConceptIdList,
        secondaryDrugConceptIdList = secondaryDrugConceptIdList,
        excludingDrugConceptIdList= excludingDrugConceptIdList,
        targetCohortId = targetCohortId,
        createCsv = createCsv,
        resultsSaveInFile = resultsSaveInFile,
        regimenName = regimenName,
        colorInHistogram = colorInHistogram,
        regimenConceptId =regimenConceptId)

##_____________________________________________________##


## Insert episode table

##connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
#server=Sys.getenv("PDW_SERVER"),
#schema='cdmDatabaseSchema',
#user=NULL,
#password=NULL)


##conn <- DatabaseConnector::connect(connectionDetails)
##DatabaseConnector::insertTable(conn, "FOLFOX", cycleListInCohort, createTable = TRUE, progressBar = TRUE )
##DatabaseConnector::disconnect(conn)


