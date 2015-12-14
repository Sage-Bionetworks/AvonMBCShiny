
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This interface has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

shinyUI(fluidPage(
  
  tags$head(
    singleton(
      includeScript("www/readCookie.js")
    )
  ),
  titlePanel(h3("Avon Metastatic Breast Cancer Abstract Analysis")),
  
  sidebarLayout(position = "right",
                sidebarPanel( 
                  #textInput("abstractIndex", label = h3("Abstract Index"), value = "Enter abstract index, 0-99"),
                  selectInput("grants", "Grants",selectize = T,
                              choices = allTitles),
                  selectInput("pathways","Pathways", selectize=T,
                              choices = pathwayMenu),
                  #selectInput("pwgroup","Pathway Group",choices = c("All" = " ",allPwGroup)),
                  #selectInput("metaYN","Metastatis",choices = c("All" = " ",allmetaYN)),
                  selectInput("stage","Metastatic Stage",selectize = T,
                              choices = metaStageMenu),
                  #selectInput("mt","Molecular Targets",choices = c("All" = " ",allMT)),
                  #selectInput("mtgroup","Molecular Target Groups",c("All" = " ",choices=allMTGroup))
                  #submitButton("Submit")
                  h5("Number of Grants"),
                  textOutput("numGrants")
                ),
                mainPanel(
                  tags$style(type='text/css', '#TechAbstract {font-size:10px;}'),
                  h5("Title"),
                  textOutput("AwardTitle"),
                  h5("Abstract"), 
                  textOutput("TechAbstract"),
                  h5("Principal Investigator"),
                  textOutput("PIName"),
                  h5("Institution"),
                  textOutput("Institution"),
                  h5("Pathway"),
                  textOutput("Pathway"),
                  h5("Pathway Group"),
                  textOutput("PathwayGroup"),
                  h5("Molecular Target"),
                  textOutput("MolecularTarget"),
                  h5("Molecular Target Group"),
                  textOutput("MolecularTargetGroup")
                  
                  
                )
  )
))
