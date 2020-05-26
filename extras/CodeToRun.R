install.packages("listviewer")
install.packages("jsonlite")
install.packages("collapsibleTree")
install.packages("rjson")
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")
install.packages("DatabaseConnector")
install.packages("ParallelLogger")
install.packages("SqlRender")

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

cohortTableCreation = TRUE # Create cohort table for your cohort table
episodeTableCreation = TRUE  # warning: existing table might be erased
generateTargetCohort = TRUE  # Create target cohort ,i.e., 'colorectal cancer'
includeConceptIdSetDescendant = TRUE

episodeTable <- "episode_table"
episodeEventTable <- "episode_event_table"
cohortTable <- "cohort"

maxCores <- 4

executeExtraction(connectionDetails,
                  oracleTempSchema = NULL,
                  cdmDatabaseSchema,
                  vocaDatabaseSchema,
                  cohortDatabaseSchema,
                  oncologyDatabaseSchema,
                  cohortTable,
                  episodeTable,
                  episodeEventTable,
                  includeConceptIdSetDescendant = TRUE,
                  maxCores,
                  cohortTableCreation = FALSE,
                  episodeTableCreation = FALSE,
                  generateTargetCohort = FALSE)
