library(shiny)
library(synapseClient)
library(shinydashboard)
library(shinyBS)
library(DT)
#synapseLogin()
#source("load.R")
metaStageMenu <- c("arrest & extravasation","immune surveillance/escape",
                   "intravasation & circulation","invasion","metabolic deregulation",
                   "metastatic colonization","multiple","other/not specified","auto generated")
metaStageMenu= sort(unique(tolower(metaStageMenu)))
metaStageMenu = c("all",metaStageMenu)
mutablemetastage = c("arrest & extravasation","immune surveillance/escape",
                     "intravasation & circulation","invasion","metabolic deregulation",
                     "metastatic colonization","other/not specified","auto generated")
pathways <- read.csv("pathways.csv",stringsAsFactors = F)
pathways <- pathways$pathways
pathways <- sort(pathways)
# ------------------------------------------------------
# global functions
# ------------------------------------------------------
# change.annotations <- function(title, annotation.label, value) {
#   Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
#   rowIndex <- which(Dynamic.annotations@values$AwardTitle == title)
#   if (value != "") {
#     Dynamic.annotations@values[[annotation.label]][rowIndex] <- value
#     synStore(Dynamic.annotations)
#     Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
#   }
#   return(Dynamic.annotations@values[[annotation.label]][rowIndex])
# }
confusionMatrix_scores <- function(true, pred, positive) {
  Pos <- true[pred == positive]
  TP <- sum(Pos == positive)
  FN <- sum(Pos != positive)
  Neg <- true[pred != positive]
  TN <- sum(Neg != positive)
  FP <- sum(Neg == positive)
  sens = TP/(TP+FN)
  spec = TN/(TN+FP)
  c(round(sens,2),round(spec,2))
}
