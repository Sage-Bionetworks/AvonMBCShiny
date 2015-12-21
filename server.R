
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This server has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org


server <- function(input, output,session) {
  #input$searchText
  getGrantsTitles <- reactive({
    table <- tableQuery()$TechAbstract
    sapply(table, function(x) {
      gregexpr(pattern = input$searchText,x)[[1]]
    })
  })
  
  tableQuery <- reactive({
    if (input$show_MBC) {
      table <- grant.MBC[tolower(grant.MBC$Metastasis_stage) == input$stage,]
    } else {
      table <- grant.df[grant.df$Metastasis_YN == 'n',]
    }
    table
  })
  output$mySite <- renderUI({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)
    author = paste(table[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
    tags$a(href = sprintf("http://www.ncbi.nlm.nih.gov/pubmed/?term=%s+%s",author,"breast"), "NCBI resources")
  })
  

  output$numGrants <- renderText({
    table<-tableQuery()
    return(nrow(table))
  })
  
  observe({
    updateSelectInput(session, "grants", label = "Grants", choices = tableQuery()$AwardTitle)
  })
  # ---------------------------------------------
  # STATIC CONTENT
  # ---------------------------------------------
  output$PIName<-renderText({
    table <- tableQuery() 
    #rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
    rowIndex<-grep(input$grants, table$AwardTitle)
    paste(table[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
  })
  
  output$Institution<-renderText({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)
    table[rowIndex, "Institution"]
  })
  
  output$AwardTitle<-renderText({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)
    table[rowIndex, "AwardTitle"]
  })
  
  
  output$Pathway<-renderText({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)
    table[rowIndex, "Pathway"]
    
  })
  
  
  output$PathwayGroup<-renderText({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)
    table[rowIndex, "Pathway_Group"]
    
  })
  
  
  output$MolecularTarget<-renderText({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)
    table[rowIndex, "Molecular_Target"]

  })
  
  output$MolecularTargetGroup<-renderText({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)

    table[rowIndex, "Molecular_Target_Group"]
  })
  
  output$TechAbstract<-renderText({
    table <- tableQuery() 
    rowIndex<-grep(input$grants, table$AwardTitle)
    #if (length(rowIndex)!=1) {
    #  "Error:  No unique matching record"
    #} else {
    text <- table[rowIndex, "TechAbstract"]
    
    for (word in highlight.keywords) {
      text <- gsub(sprintf(" %s",word), sprintf(' <span style="background-color: #FFFF00">%s</span>',word),text,fixed=T)
    }
    text
    #}
  })
}
# 
# shinyServer(function(input, output, session) {
# 
#   session$sendCustomMessage(type="readCookie",
#                             message=list(name='org.sagebionetworks.security.user.login.token'))
#   
#   #foo <- observeEvent(input$cookie, {
# 
#     #log into synapse
#    # synapseLogin(sessionToken=input$cookie)
#     
#     #Get the grants with respect to different metadata
#     tableQuery <- reactive({
#       table <- grant.MBC[grant.MBC$Metastasis_stage == input$stage,]
#       table
#     })
#     
#     observe({
#       updateSelectInput(session, "grants", label = "Grants", choices = tableQuery()$AwardTitle)
#       #updateSelectInput(session, "pathways", label = "Pathways", choices = tableQuery()$Pathway)
#     })
#     
#     output$PIName<-renderText({
#         table <- tableQuery() 
#         #rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           paste(table[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
#         }
#     })
#     output$numGrants <- renderText({
#       table<-tableQuery()
#       return(nrow(table))
#     })
#     # ---------------------------------------------
#     # STATIC CONTENT
#     # ---------------------------------------------
#     output$Institution<-renderText({
#         table <- tableQuery() 
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           table[rowIndex, "Institution"]
#         }
#     })
#     
#     output$AwardTitle<-renderText({
#         table <- tableQuery() 
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           table[rowIndex, "AwardTitle"]
#         }
#     })
# 
#         
#     output$Pathway<-renderText({
#         table <- tableQuery() 
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           table[rowIndex, "Pathway"]
#         }
#     })
# 
#         
#     output$PathwayGroup<-renderText({
#         table <- tableQuery() 
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           table[rowIndex, "Pathway_Group"]
#         }
#     })
# 
#         
#     output$MolecularTarget<-renderText({
#         table <- tableQuery() 
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           table[rowIndex, "Molecular_Target"]
#         }
#     })
#     
#     output$MolecularTargetGroup<-renderText({
#         table <- tableQuery() 
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           table[rowIndex, "Molecular_Target_Group"]
#         }
#     })
#     
#     output$TechAbstract<-renderText({
#         table <- tableQuery() 
#         rowIndex<-grep(input$grants, table$AwardTitle)
#         if (length(rowIndex)!=1) {
#           "Error:  No unique matching record"
#         } else {
#           table[rowIndex, "TechAbstract"]
#         }
#       }
#     )
#     # ---------------------------------------------
#     # DYNAMIC CONTENT
#     # ---------------------------------------------
#   # })
#   #}) Integrate with synapse END
# 
# })
