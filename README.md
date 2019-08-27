# treatmentCycleExtraction

Introduction
==========
Tool for extracting treatment cycle from single medication records in CDM database

Technology
==========
treatmentCycleExtraction is an R package.

Dependencies
============
* SqlRender
* DatabaseConnector
* plotly
* dplyr

Getting started
============
In R, use the following commands to download and install:

install.packages("devtools")

devtools::install_github("ABMI/treatmentCycleExtraction")

library('treatmentCycleExtraction')
How to run
============
# Parameter setting for algorithm :

parameter setting
 Drug condition setting :

Primary drugs should be in first day of regimen cycle.
Secondary drugs are rest of drugs in your targeting regimen. Not necessary for excuting but recommanded.
Eliminatory drugs are drugs that distracting your targeting regimen. Not necessary for excuting.
Multiple concept_id available and drug name is not necessary for excuting.

primaryDrugList <- list(c(1367268))
names(primaryDrugList) <- c('irinotecan')

secondaryDrugList <- list(955632,c(1388796,19111620))
names(secondaryDrugList) <- c('Fluorouracil','leucovorin')

eliminatoryDrugList <- list(1397141)
names(eliminatoryDrugList) <- c('Bevacizumab')

The cohort definition id of the target cohort:
targetCohortId <-314

Include descendants of drugs in list or not:
includeDescendant <- TRUE

Drug records could be out of cohort period or not :
outofCohortPeriod <- FALSE

Period of observing secondary drug from primary drug used date :
drugObservationDate <- 7

Each cycle start date should be apart as gap date, and gap date can be in range of +- date as gap date variation :
gapDateBetweenCycle <-14
gapDateVariation <-10

maximum cycle number in this regimen :
maximumCycleNumber <-50

The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-'scratch.dbo'
cohortTable <-'colon cancer'

Generate all cycle records list of cohort in csv file ## It would be treatment episode table later...
createCsv <- TRUE

Details for connecting to the server:

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL)
                                                             
# Then run the following:

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
        createCsv = createCsv)
        
License
=======
  treatmentCycleExtraction is licensed under Apache License 2.0

Development
===========
  treatmentCycleExtraction is being developed in R Studio.

