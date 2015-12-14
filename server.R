
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This server has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

shinyServer(function(input, output, session) {

  session$sendCustomMessage(type="readCookie",
                            message=list(name='org.sagebionetworks.security.user.login.token'))
  
  #foo <- observeEvent(input$cookie, {

    #log into synapse
   # synapseLogin(sessionToken=input$cookie)
    
    #Get the grants with respect to different metadata
    tableQuery <- reactive({
      table <- grant.MBC[grant.MBC$Metastasis_stage == input$stage,]
      table
    })
    
    observe({
      updateSelectInput(session, "grants", label = "Grants", choices = tableQuery()$AwardTitle)
      updateSelectInput(session, "pathways", label = "Pathways", choices = tableQuery()$Pathway)
      #updateSelectInput(session, "pwgroup", label = "Pathway Group", choices = c(tableQuery()@values$Pathway_Group = sprintf("and Pathway_Group = %s",tableQuery()@values$Pathway_Group)))
      #updateSelectInput(session, "metaYN", label = "Metastatis", choices = c(tableQuery()@values$Metastatis_YN = sprintf("and Metastatis_YN = %s",tableQuery()@values$Metastatis_YN)))
      #updateSelectInput(session, "metaStage", label = "Metastatic Stage", choices = c(tableQuery()@values$Metastasis_stage = sprintf("and Metastasis_stage = %s",tableQuery()@values$Metastasis_stage)))
      #updateSelectInput(session, "mt", label = "Molecular Targets", choices = c(tableQuery()@values$Molecular_Target = sprintf("and Molecular_Target = %s",tableQuery()@values$Molecular_Target)))
      #updateSelectInput(session, "mtgroup", label = "Molecular Target Groups", choices = c(tableQuery()@values$Molecular_Target_Group = sprintf("and Molecular_Target = %s",tableQuery()@values$Molecular_Target_Group)))
      
    })
    
    output$PIName<-renderText({
        table <- tableQuery() 
        #rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
        rowIndex<-grep(input$grants, table$AwardTitle)
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          paste(table[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
        }
    })
    output$numGrants <- renderText({
      table<-tableQuery()
      return(nrow(table))
    })
    
    output$Institution<-renderText({
        table <- tableQuery() 
        rowIndex<-grep(input$grants, table$AwardTitle)
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          table[rowIndex, "Institution"]
        }
    })
    
    output$AwardTitle<-renderText({
        table <- tableQuery() 
        rowIndex<-grep(input$grants, table$AwardTitle)
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          table[rowIndex, "AwardTitle"]
        }
    })

        
    output$Pathway<-renderText({
        table <- tableQuery() 
        rowIndex<-grep(input$grants, table$AwardTitle)
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          table[rowIndex, "Pathway"]
        }
    })

        
    output$PathwayGroup<-renderText({
        table <- tableQuery() 
        rowIndex<-grep(input$grants, table$AwardTitle)
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          table[rowIndex, "Pathway_Group"]
        }
    })

        
    output$MolecularTarget<-renderText({
        table <- tableQuery() 
        rowIndex<-grep(input$grants, table$AwardTitle)
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          table[rowIndex, "Molecular_Target"]
        }
    })
    
    output$MolecularTargetGroup<-renderText({
        table <- tableQuery() 
        rowIndex<-grep(input$grants, table$AwardTitle)
        if (length(rowIndex)!=1) {
          "Error:  No unique matching record"
        } else {
          table[rowIndex, "Molecular_Target_Group"]
        }
    })
    
    output$TechAbstract<-renderText({
        table <- tableQuery() 
        rowName<-table$ROW_INDEX[grep(input$grants, table$AwardTitle)]
        if (length(rowName)!=1) {
          "Error:  No unique matching record"
        } else {
          temp= synDownloadTableFile(grants, rowName, "TechAbstract")
          abstract <- readLines(temp)
          unlink(temp)
          return(abstract)
        }
      }
    )
  # })
  #}) Integrate with synapse END

})
