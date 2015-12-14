
library(shiny)

library(synapseClient)
synapseLogin() 

grants<-synTableQuery("SELECT * FROM syn5522369",filePath = ".",loadResult = F)

#List all the downloaded files if the table has been updated a new file will be downloaded, 
#then delete the old file to save space
grantsFileName = list.files(".","Job-*")
oldFiles = grantsFileName[basename(grants@filePath) != grantsFileName]
unlink(oldFiles)


grant.df <- read.csv(grants@filePath,stringsAsFactors = F)
grant.df$ROW_INDEX <- paste(grant.df$ROW_ID,grant.df$ROW_VERSION,sep="_")
grant.MBC <- grant.df[grant.df$Metastasis_YN == 'y',]
allTitles <- grant.MBC$AwardTitle
allPathways <- grant.MBC$Pathway
#allPwGroup <- menus@values$Pathway_Group
#allmetaYN <- menus@values$Metastasis_YN
allmetaStage <-grant.MBC$Metastasis_stage
#allMT <- menus@values$Molecular_Target
#allMTGroup <- menus@values$Molecular_Target_Group

#allPathways = c(allPathways = sprintf("and Pathway = %s",allPathways))
#allPwGroup = c(allPwGroup = sprintf("and Pathway = %s",allPwGroup))
#allmetaYN = c(allmetaYN = sprintf("and Pathway = %s",allmetaYN))



#Need to account for if the user wants to see grants related to multiple pathways or metastages
metaStageMenu= unique(allmetaStage)
pathwayMenu = unique(allPathways)
#allMT = c(allMT = sprintf("and Pathway = %s",allMT))
#allMTGroup = c(allMTGroup = sprintf("and Pathway = %s",allMTGroup))



