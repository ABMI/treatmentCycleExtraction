#parameter setting
## Drug condition setting :

### Primary drugs should be in first day of regimen cycle.
### Secondary drugs are rest of drugs in your targeting regimen. Not necessary for excuting but recommanded.
### Eliminatory drugs are drugs that distracting your targeting regimen. Not necessary for excuting.
### Multiple concept_id available and drug name is not necessary for excuting.

primaryDrugList <- list(955632)
names(primaryDrugList) <- c('Fluorouracil')

secondaryDrugList <- list(c(1318011),c(1388796,19111620))
names(secondaryDrugList) <- c('Oxaliplatin','leucovorin')

eliminatoryDrugList <- list(1397141)
names(eliminatoryDrugList) <- c('Bevacizumab')

regimenName <- 'FOLFOX'

## The cohort definition id of the target cohort:
targetCohortId <-314

## Include descendants of drugs in list or not:
includeDescendant <- TRUE

## Drug records could be out of cohort period or not :
outofCohortPeriod <- FALSE

## Period of observing secondary drug from primary drug used date :
drugInspectionDate <- 7

## Each cycle start date should be apart as gap date, and gap date can be in range of +- date as gap date variation :
gapDateBetweenCycle <-20
gapDateVariation <-18

## maximum cycle number in this regimen :
maximumCycleNumber <-50

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-'scratch.dbo'
cohortTable <-'cohort'

## Generate all cycle records list of cohort as csv file in working directory 
## It would be treatment episode table later version...
createCsv <- FALSE

## Details for connecting to the server:

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL)

## Execute_____________________________________________##

execute(connectionDetails,
        connection,
        cohortTable = cohortTable,
        includeDescendant = includeDescendant,
        outofCohortPeriod = outofCohortPeriod,
        cohortDatabaseSchema = cohortDatabaseSchema,
        primaryDrugList = primaryDrugList,
        secondaryDrugList = secondaryDrugList,
        eliminatoryDrugList= eliminatoryDrugList,
        targetCohortId = targetCohortId,
        createCsv = createCsv,
        regimenName = regimenName)

##_____________________________________________________##


## Insert episode table (under developing...)

##conn <- connect(connectionDetails)
##insertTable(conn, "my_table", data)
##disconnect(conn)

