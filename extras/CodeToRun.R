install.packages("DatabaseConnector")
install.packages("collapsibleTree")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("reshape2")
install.packages("scales")
install.packages("highcharter")
install.packages("gridExtra")
install.packages("viridis")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("plotly")
install.packages("SqlRender")
install.packages("listviewer")
install.packages("tidyr")
install.packages("networkD3")
install.packages("ggbeeswarm")
install.packages("flexdashboard")
install.packages("grid")

library(flexdashboard)

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
                  generateTargetCohort = FALSE
)