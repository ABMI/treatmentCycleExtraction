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
* dplyr
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

Database parameters :
```r
# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')

oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_database_schema.dbo"
cohortDatabaseSchema <- "cohort_database_schema.dbo"
vocaDatabaseSchema <- "voca_database_schema.dbo"
oncologyDatabaseSchema <- "oncology_database_schema.dbo" # Schema for Episode table and Episode_eventtable, default = cdmDatabaseSchema

createCohortTable = FALSE # Create cohort table for your cohort table
createEpisodeTable = FALSE  # warning: existing table might be erased
generateTargetCohort = FALSE  # Create target cohort ,i.e., 'colorectal cancer'

episodeTable <- "episode_table"
episodeEventTable <- "episode_event_table"
cohortTable <- "cohort"

maxCores <- 4
```

# Then run the following :
Generate episode and episode event table :
```r
executeExtraction(connectionDetails,
                  oracleTempSchema = NULL,
                  cdmDatabaseSchema,
                  vocaDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema,
                  oncologyDatabaseSchema,
                  cohortTable,
                  episodeTable,
                  episodeEventTable,
                  includeConceptIdSetDescendant = TRUE,
                  maxCores,
                  createCohortTable = FALSE,
                  createEpisodeTable = FALSE,
                  generateTargetCohort = FALSE)
```
# Rule Editor

If you need a modification in the rule for algorithm :
```r
targetRegimenIds <- c(35806596,35804761)
newJson <- ruleEditor(targetRegimenIds) # Edit your rule
newJson <- ruleEditor(new= TRUE) # Add a new rule
ruleSave(newJson,targetRegimenIds) # Save your rule
```

License
=======
  treatmentCycleExtraction is licensed under Apache License 2.0

Development
===========
  treatmentCycleExtraction is being developed in R Studio.

