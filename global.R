
library(shiny)

library(synapseClient)
library(shinydashboard)
synapseLogin() 

#grants<-synTableQuery("SELECT * FROM syn5522369",filePath = ".",loadResult = F)

#List all the downloaded files if the table has been updated a new file will be downloaded, 
#then delete the old file to save space
#grantsFileName = list.files(".","Job-*")
#oldFiles = grantsFileName[basename(grants@filePath) != grantsFileName]
#unlink(oldFiles)

#Static content
grant.df <- read.csv("ICRP_allcombined_grants.csv",stringsAsFactors = F)

#grant.df$ROW_INDEX <- paste(grant.df$ROW_ID,grant.df$ROW_VERSION,sep="_")
grant.MBC <- grant.df[grant.df$Metastasis_YN == 'y',]
allTitles <- grant.MBC$AwardTitle
allPathways <- grant.MBC$Pathway
allmetaStage <-grant.MBC$Metastasis_stage

highlight.keywords <- colnames(grant.df)[grep("KW*",colnames(grant.df))][-1]
highlight.keywords <- sub("KW_","",highlight.keywords)

highlight.keywords <- sub("_"," ",highlight.keywords)

#Need to account for if the user wants to see grants related to multiple pathways or metastages
metaStageMenu= unique(tolower(allmetaStage))
pathwayMenu = unique(tolower(allPathways))

#allMT = c(allMT = sprintf("and Pathway = %s",allMT))
#allMTGroup = c(allMTGroup = sprintf("and Pathway = %s",allMTGroup))



