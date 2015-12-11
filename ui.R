
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This interface has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

library(shiny)

library(synapseClient)
shinyUI(fluidPage(

  tags$head(
    singleton(
      includeScript("www/readCookie.js")
    )
  ),
  uiOutput("title"),  
  titlePanel(h3("Avon Metastatic Breast Cancer Abstract Analysis")),

  sidebarLayout(position = "right",
    sidebarPanel( 
      textInput("abstractIndex", label = h3("Abstract Index"), value = "Enter abstract index, 0-99"),
      submitButton("Submit")
      ),
    mainPanel(
      h5("Award Title"),
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
