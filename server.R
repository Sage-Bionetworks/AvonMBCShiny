
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
  
  tableQuery <- reactive({
    if (input$show_MBC) {
      if (input$stage != "all") {
        table <- grant.MBC[tolower(grant.MBC$Metastasis_stage) == input$stage,]
      } else {
        table <- grant.MBC
      }
      input$searchButton
      if (input$searchText != "") {
        length <- unname(sapply(table$TechAbstract, function(x) {
          length(gregexpr(pattern = input$searchText,x,ignore.case = T)[[1]])
        }))
        table <- table[order(length,decreasing = T),]
      }
    } else {
      table <- grant.df[grant.df$Metastasis_YN == 'n',]
    }
    table
  })
  
  output$grantTitles <- DT::renderDataTable({
    DT::datatable(tableQuery()[,c("AwardTitle","PIFirstName","PILastName","Institution")],selection = 'single')
  },server=F)
  
  output$mySite <- renderUI({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    author = paste(table[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
    tags$a(href = sprintf("http://www.ncbi.nlm.nih.gov/pubmed/?term=%s+%s",author,"breast"), "NCBI resources",target="_blank")
  })
  
  output$numGrants <- renderText({
    table<-tableQuery()
    return(nrow(table))
  })
  
  #observe({
  #  updateSelectInput(session, "grants", label = "Grants", choices = tableQuery()$AwardTitle)
  #})
  # ---------------------------------------------
  # STATIC CONTENT
  # ---------------------------------------------
  observe({
    # TRUE if input$controller is even, FALSE otherwise.
    x_even <- input$grantTitles_rows_selected
    # Change the selected tab.
    # Note that the tabsetPanel must have been created with an 'id' argument
    if (length(x_even)!=0) {
      updateTabItems(session, "tabs","GrantInfo")
    } else {
      updateTabItems(session,"tabs", "GrantSel")
    }
  })
  
  output$PIName<-renderText({
    table <- tableQuery() 
    #rowIndex<-grep(sprintf("^%s_", input$abstractIndex), rownames(table@values))
    rowIndex<-input$grantTitles_rows_selected
    paste(table[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
  })
  
  output$Institution<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "Institution"]
  })
  
  output$AwardTitle<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "AwardTitle"]
  })
  
  output$Pathway<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "Pathway"]
  })
  
  output$PathwayGroup<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "Pathway_Group"]
  })
  
  output$MolecularTarget<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "Molecular_Target"]
  })
  
  output$MolecularTargetGroup<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "Molecular_Target_Group"]
  })
  
  output$MetaStage<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "Metastasis_stage"]
  })
  
  output$MetaYN<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    table[rowIndex, "Metastasis_YN"]
  })
  
  output$TechAbstract<-renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    text <- table[rowIndex, "TechAbstract"]
    
    for (word in highlight.keywords) {
      text <- gsub(sprintf(" %s",word), sprintf(' <span style="background-color: #FFFF00">%s</span>',word),text,fixed=T)
    }
    text
  })
  
  
  
  # ---------------------------------------------
  # Dynamic Content
  # ---------------------------------------------
#   output$mutable.Pathway <- renderText({
#     table <- tableQuery() 
#     rowIndex<-grep(input$grants, Dynamic.annotations@values$AwardTitle)
#     input$button1
#     pathwayName <- isolate(input$mutable.pathway)
#    # change.annotations(rowIndex,"Pathway",pathwayName)
#     if (pathwayName != "") {
#       Dynamic.annotations@values$Pathway[rowIndex] <- pathwayName
#       synStore(Dynamic.annotations)
#       Dynamic.annotations <-synTableQuery("SELECT * FROM syn5562008",filePath = ".")
#       Dynamic.annotations@values <- Dynamic.annotations@values[!duplicated(Dynamic.annotations@values$AwardTitle),]
#       pathwayName = ""
#     }
#     Dynamic.annotations@values$Pathway[rowIndex]
#   })
#   
#   output$mutable.PathwayGroup <- renderText({
#     table <- tableQuery() 
#     rowIndex<-grep(input$grants, Dynamic.annotations@values$AwardTitle)
#     input$button2
#     pathwaygroup <- isolate(input$mutable.pathwaygroup)
#     if (pathwaygroup != "") {
#       Dynamic.annotations@values$Pathway_Group[rowIndex] <- pathwaygroup
#       synStore(Dynamic.annotations)
#       Dynamic.annotations <-synTableQuery("SELECT * FROM syn5562008",filePath = ".")
#       Dynamic.annotations@values <- Dynamic.annotations@values[!duplicated(Dynamic.annotations@values$AwardTitle),]
#       pathwaygroup = ""
#     }
#     Dynamic.annotations@values$Pathway_Group[rowIndex]
#   })
#   
#   output$mutable.MT <- renderText({
#     table <- tableQuery() 
#     rowIndex<-grep(input$grants, Dynamic.annotations@values$AwardTitle)
#     input$button3
#     mt <- isolate(input$mutable.mt)
#     if (mt != "") {
#       Dynamic.annotations@values$Molecular_Target[rowIndex] <- mt
#       synStore(Dynamic.annotations)
#       Dynamic.annotations <-synTableQuery("SELECT * FROM syn5562008",filePath = ".")
#       Dynamic.annotations@values <- Dynamic.annotations@values[!duplicated(Dynamic.annotations@values$AwardTitle),]
#       mt = ""
#     }
#     Dynamic.annotations@values$Molecular_Target[rowIndex]
#   })
#   
#   output$mutable.MTGroup <- renderText({
#     table <- tableQuery() 
#     rowIndex<-grep(input$grants, Dynamic.annotations@values$AwardTitle)
#     input$button4
#     mtgroup <- isolate(input$mutable.mtgroup)
#     if (mtgroup != "") {
#       Dynamic.annotations@values$Molecular_Target_Group[rowIndex] <- mtgroup
#       synStore(Dynamic.annotations)
#       Dynamic.annotations <-synTableQuery("SELECT * FROM syn5562008",filePath = ".")
#       Dynamic.annotations@values <- Dynamic.annotations@values[!duplicated(Dynamic.annotations@values$AwardTitle),]
#       mtgroup = ""
#     }
#     Dynamic.annotations@values$Molecular_Target_Group[rowIndex]
#   })
#   
  output$mutable.Metayn <- renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    input$button5
    metayn <- isolate(input$mutable.metayn)
    change.annotations(rowIndex = rowIndex,annotation.label = "Metastasis_YN",value = metayn)
  })
  
  output$mutable.Metastage <- renderText({
    table <- tableQuery() 
    rowIndex<-input$grantTitles_rows_selected
    input$button6
    metastage <- isolate(input$mutable.metastage)
    change.annotations(rowIndex = rowIndex,annotation.label = "Metastasis_stage",value = metastage)
  })
}
