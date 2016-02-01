library(shiny)
library(synapseClient)
library(shinydashboard)
library(shinyBS)
library(DT)

# ------------------------------------------------------
# global functions
# ------------------------------------------------------
change.annotations <- function(title, annotation.label, value) {
  Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
  rowIndex <- which(Dynamic.annotations@values$AwardTitle == title)
  if (value != "") {
    Dynamic.annotations@values[[annotation.label]][rowIndex] <- value
    synStore(Dynamic.annotations)
    Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
  }
  return(Dynamic.annotations@values[[annotation.label]][rowIndex])
}

