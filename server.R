
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This server has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

library(shiny)

shinyServer(function(input, output, session) {

  session$sendCustomMessage(type="readCookie",
                            message=list(name='org.sagebionetworks.security.user.login.token'))
  
  foo <- reactive(synapseLogin(sessionToken=input$cookie))
  
  output$title <- renderUI({
    titlePanel(sprintf("Welcome, %s", synGetUserProfile()@userName))
  })

  table<-synTableQuery("SELECT * FROM syn5479989 LIMIT 100")

   output$TechAbstract<-renderText(
    {
      rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
      if (length(rowIndex)!=1) {
        "Error:  No unique matching record"
      } else {
        table@values[rowIndex, "TechAbstract"]
      }
    }
  )
      
  output$AwardTitle<-renderText(
    {
      rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
      if (length(rowIndex)!=1) {
        "Error:  No unique matching record"
      } else {
        table@values[rowIndex, "AwardTitle"]
      }
    }
  )

      
  output$Pathway<-renderText(
    {
      rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
      if (length(rowIndex)!=1) {
        "Error:  No unique matching record"
      } else {
        table@values[rowIndex, "Pathway"]
      }
    }
  )

      
  output$PathwayGroup<-renderText(
    {
      rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
      if (length(rowIndex)!=1) {
        "Error:  No unique matching record"
      } else {
        table@values[rowIndex, "Pathway_Group"]
      }
    }
  )

      
  output$MolecularTarget<-renderText(
    {
      rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
      if (length(rowIndex)!=1) {
        "Error:  No unique matching record"
      } else {
        table@values[rowIndex, "Molecular_Target"]
      }
    }
  )
      
  output$MolecularTargetGroup<-renderText(
    {
      rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
      if (length(rowIndex)!=1) {
        "Error:  No unique matching record"
      } else {
        table@values[rowIndex, "Molecular_Target_Group"]
      }
    }
  )

})
