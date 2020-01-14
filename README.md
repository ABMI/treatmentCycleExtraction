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
* ggplot2
* data.table
* rjson

Getting started
============
In R, use the following commands to download and install:

install.packages("devtools")

devtools::install_github("ABMI/treatmentCycleExtraction")

library('treatmentCycleExtraction')

How to run
============
# Parameter setting for algorithm :

Drug condition setting :
```r
## Primary drugs should be in first day of regimen cycle.
## Secondary drugs are rest of drugs in your targeting regimen. Not necessary for excuting but recommanded.
## Eliminatory drugs are drugs that distracting your targeting regimen. Not necessary for excuting.
## Multiple concept_id available and drug name is not necessary for excuting.

## 1. Manual drug condition
primaryDrugList <- list(c(1367268))
names(primaryDrugList) <- c('irinotecan')

secondaryDrugList <- list(955632,c(1388796,19111620))
names(secondaryDrugList) <- c('Fluorouracil','leucovorin')

eliminatoryDrugList <- list(1397141)
names(eliminatoryDrugList) <- c('Bevacizumab')

regimenName <- 'FOLFOX'
regimenConceptId <- 35806596

## 2. regimen setting when you do not want to ingredient manually
regimenConceptId <- 35806596
regimenSetting(connectionDetails,
               connection,
               vocaDatabaseSchema,
               regimenConceptId = regimenConceptId)

## The cohort definition id of the target cohort:
targetCohortId <-314

## Include descendants of drugs in list or not:
includeDescendant <- TRUE

## Drug records could be out of cohort period or not :
outofCohortPeriod <- FALSE

## Period of observing secondary drug from primary drug used date :
drugObservationDate <- 7

## Each cycle start date should be apart as gap date, and gap date can be in range of +- date as gap date variation :
gapDateBetweenCycle <-20
gapDateAfter<-10  #+
gapDateBefore<-5  #-

## The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-'scratch.dbo'
cohortTable <-'cohort'
vocaDatabaseSchema <- 'voca_Database_Schema.dbo'

## Generate all cycle records list of cohort as csv file in working directory 
## It will be treatment episode table
createCsv <- FALSE
resultsSaveInFile <- FALSE ## save histogram and distribution table
colorInHistogram <- 'FF8200'
## Details for connecting to the server:

## Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL)
```

# Then run the following :
execute :
```r
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
        resultsSaveInFile = resultsSaveInFile,
        regimenName = regimenName,
        colorInHistogram = colorInHistogram,
        regimenConceptId =regimenConceptId)
```
License
=======
  treatmentCycleExtraction is licensed under Apache License 2.0

Development
===========
  treatmentCycleExtraction is being developed in R Studio.

