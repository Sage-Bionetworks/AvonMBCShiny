
library(shiny)

library(synapseClient)
library(shinydashboard)
synapseLogin() 

#Dynamic content
Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
#There are duplicated award titles

#List all the downloaded files if the table has been updated a new file will be downloaded, 
#then delete the old file to save space
annotation.Names = list.files(".","Job-*")
#delete.files <- annotation.Names[-which.max(file.mtime(annotation.Names))]
#unlink(delete.files)
unlink(annotation.Names)

grantdf.Rdata <- synGet("syn5574249")
load(grantdf.Rdata@filePath)

#Static content
#grant.df <- read.csv("ICRP_allcombined_grants.csv",stringsAsFactors = F)
#grant.df <- grant.df[!duplicated(grant.df$AwardTitle),]

#grant.df$ROW_INDEX <- paste(grant.df$ROW_ID,grant.df$ROW_VERSION,sep="_")
grant.MBC <- grant.df[grant.df$Metastasis_YN == 'y',]
allTitles <- grant.MBC$AwardTitle
allPathways <- grant.MBC$Pathway
allmetaStage <-grant.MBC$Metastasis_stage

#Grab all words to highlight
highlight.keywords <- colnames(grant.df)[grep("KW*",colnames(grant.df))][-1]
highlight.keywords <- sub("KW_","",highlight.keywords)
highlight.keywords <- sub("_"," ",highlight.keywords)

#Need to account for if the user wants to see grants related to multiple pathways or metastages
metaStageMenu= sort(unique(tolower(allmetaStage)))
metaStageMenu = c("all",metaStageMenu)
pathwayMenu = unique(tolower(allPathways))

#allMT = c(allMT = sprintf("and Pathway = %s",allMT))
#allMTGroup = c(allMTGroup = sprintf("and Pathway = %s",allMTGroup))

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

