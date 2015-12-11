
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
library(synapseClient)

shinyServer(function(input, output, session) {

  session$sendCustomMessage(type="readCookie",
                            message=list(name='org.sagebionetworks.security.user.login.token'))
  
  #foo <- observeEvent(input$cookie, {

    #log into synapse
   # synapseLogin(sessionToken=input$cookie)
    synapseLogin() 
    output$title <- renderUI({
      titlePanel(sprintf("Welcome, %s", synGetUserProfile()@userName))
      #print(head(table))
    })
    #For the download file path
    filePath<-synTableQuery("SELECT * FROM syn5522369 LIMIT 100",loadResult = F)
    table<-synTableQuery("SELECT * FROM syn5522369 LIMIT 100")
    #Remove the files downloaded
    unlink(filePath@filePath)

    output$TechAbstract<-renderText(
      {
        rowIndex<-rownames(table@values)[grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))]
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          temp= synDownloadTableFile(table, rowIndex, "TechAbstract")
          return(readLines(temp))
        }
      }
    )
    
    output$PIName<-renderText(
      {
        rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          paste(table@values[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
        }
      }
    )
    
    output$Institution<-renderText(
      {
        rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          table@values[rowIndex, "Institution"]
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
  #}) Integrate with synapse END

})
