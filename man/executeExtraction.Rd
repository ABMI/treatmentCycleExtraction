% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Main.R
\name{executeExtraction}
\alias{executeExtraction}
\title{Episode / Episode_event table generation for chemotherapy}
\usage{
executeExtraction(
  connectionDetails,
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
  cohortTableCreation = FALSE,
  episodeTableCreation = FALSE,
  generateTargetCohort = FALSE
)
}
\value{
Target Cohort, Episode, Episode Event
}
\description{
Episode / Episode_event table generation for chemotherapy
}
