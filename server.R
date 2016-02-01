
# Server logic for AvonMBC

server <- function(input, output,session) {
   session$sendCustomMessage(type="readCookie",
                             message=list(name='org.sagebionetworks.security.user.login.token'))
   
   foo <- observeEvent(input$cookie, {
    if (!is.null(input$cookie)) {
      synapseLogin(sessionToken=input$cookie)
    } else {
      synapseLogin()
    }
    source("load.R")
    output$userLoggedIn <- renderText({
      sprintf("Logged in as %s", synGetUserProfile()@userName)
    })
    #Grants (allow for querying)

    tableQuery <- reactive({
      if (input$show_MBC) {
        if (input$stage != "all") {
          table.df <- grant.MBC[tolower(grant.MBC$Metastasis_stage) == input$stage,]
        } else {
          table.df <- grant.MBC
        }
        input$searchButton
        if (input$searchText != "") {
          length <- unname(sapply(table.df$TechAbstract, function(x) {
            length(gregexpr(pattern = input$searchText,x,ignore.case = T)[[1]])
          }))
          table.df <- table.df[order(length,decreasing = T),]
        }
      } else {
        table.df <- grant.df[grant.df$Metastasis_YN == 'n',]
      }
      table.df
    })
    
    #show list of grants
    output$grantTitles <- DT::renderDataTable({
      DT::datatable(tableQuery()[,c("AwardTitle","PIFirstName","PILastName","Institution")],selection = 'single')
    },server=F)
    
    #NCBI connection
    output$mySite <- renderUI({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      author = paste(table.df[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
      tags$a(href = sprintf("http://www.ncbi.nlm.nih.gov/pubmed/?term=%s+%s",author,"breast"), "NCBI resources",target="_blank")
    })
    
    #number of grants
    output$numGrants <- renderText({
      table.df<-tableQuery()
      return(nrow(table.df))
    })
    
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
    
    #PI name
    output$PIName<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      paste(table.df[rowIndex, c("PILastName","PIFirstName")],collapse = ", ")
    })
    
    #grant PI institution
    output$Institution<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "Institution"]
    })
    
    #Grant title
    output$AwardTitle<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "AwardTitle"]
    })
    
    #Pathway annotation
    output$Pathway<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "Pathway"]
    })
    
    #pathway group annotation
    output$PathwayGroup<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "Pathway_Group"]
    })
    
    #Molecular target annotation
    output$MolecularTarget<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "Molecular_Target"]
    })
    
    #MBC Molecular Target Group annotation
    output$MolecularTargetGroup<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "Molecular_Target_Group"]
    })
    
    #Display of MBC metastatic stage annotation
    output$MetaStage<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "Metastasis_stage"]
    })
    
    #Display of MBC Metastatic YN annotation
    output$MetaYN<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      table.df[rowIndex, "Metastasis_YN"]
    })
    
    #Display of confidence of metastatic YN classifier
    output$MetaYNPostProb <- renderText({
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      if (table.df[rowIndex,"Y_meta"] >= 0.5) {
        return(paste0(round(table.df[rowIndex,"Y_meta"]*100,2),"%"))
      } else {
        return(paste0(round(table.df[rowIndex,"N_meta"]*100,2),"%"))
      }
    })
    
    # Display of metastatic stage posterior probability bar plot
    output$MetaStagePostProb <- renderPlot({
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      invasion = table.df[rowIndex,"invasion_meta"]*100
      arrest = table.df[rowIndex,"arrest_meta"]*100
      immune = table.df[rowIndex,"immune_meta"]*100
      metastatic = table.df[rowIndex,"metastatic_meta"]*100
      intra = table.df[rowIndex,"intravasatsion_meta"]*100
      metabolic = table.df[rowIndex,"metabolic_meta"]*100
      par(las=2,mar = c(10,3,4,2) + 0.1)
      barplot(c("Invasion" = invasion, "Arrest" = arrest,"Immune Surveillance" = immune,
                "Metastatic Colonization" = metastatic, "Intravasation" = intra, 
                "Metabolic Deregulation" = metabolic),main="Posterior Probabilities",
              col = c("red","blue","grey","black","purple","green"),cex.names = 0.8,ylim=c(0,100))
    })
    
    # Display of grant abstracts with highlighting of keywords
    output$TechAbstract<-renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      text <- table.df[rowIndex, "TechAbstract"]
      
      for (word in highlight.keywords) {
        text <- gsub(sprintf(" %s",word), sprintf(' <span style="background-color: #FFFF00">%s</span>',word),text,fixed=T)
      }
      text
    })
    
    #For san antonio abstract list pop up 
    output$sanantonio_abstracts <- DT::renderDataTable({
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      abstracts <- table.df[rowIndex, "SanAntonio_Abstracts"]
      abstracts <- unlist(strsplit(abstracts,","))
      abstract_table <- sanantonio[sanantonio$control  %in% abstracts,]
      DT::datatable(abstract_table[,c('title','Authors','inst','sess_date')],selection = 'single')
    },server=F)
    
    #For pop up of san antonio abstract text 
    output$sanantonio_text <- renderText({
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      abstracts <- table.df[rowIndex, "SanAntonio_Abstracts"]
      abstracts <- unlist(strsplit(abstracts,","))
      abstract_table <- sanantonio[sanantonio$control  %in% abstracts,]
      text <- abstract_table[,'body1']
      text <- gsub("\\[plusmn\\]","\\&plusmn\\;",text)
      text <- gsub("\\[","\\<",text)
      text <- gsub("\\]","\\>",text)
    })
    
    #If you click on a san antonio abstract, a pop up of the abstract text will appear
    observeEvent({
      input[[paste("sanantonio_abstracts","rows_selected",sep="_")]]
    }, {
      toggleModal(session, "abstractText","open")
    })
    
    #Gene list annotation
    output$geneList <- renderText({
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      text <- table.df[rowIndex, "TechAbstract"]
      gene_index <- unlist(lapply(gene_info,function(x) {
        count = sapply(x, function(y) {
          if (y != "") {
            return(grepl(y, text))
          } else {
            return(0)
          }
        })
        if (sum(count>0)) {
          return(which(gene_info == x))
        }
      }))
      print(temp$name[gene_index])
      temp$name[gene_index]
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
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      input$button5
      metayn <- isolate(input$mutable.metayn)
      title = table.df[rowIndex,"AwardTitle"]
      Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
      rowIndex <- which(Dynamic.annotations@values$AwardTitle == title)
      if (metayn != "") {
        Dynamic.annotations@values$Metastasis_YN[rowIndex] <- metayn
        synStore(Dynamic.annotations)
        updateSelectInput(session, "mutable.metayn", label = "Change Metastasis (y/n) here:",selected = "")
      }
      Dynamic.annotations@values$Metastasis_YN[rowIndex]
    })
    
    output$mutable.Metastage <- renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      input$button6
      metastage <- isolate(input$mutable.metastage)
      title = table.df[rowIndex,"AwardTitle"]
      Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
      rowIndex <- which(Dynamic.annotations@values$AwardTitle == title)
      if (metastage != "") {
        Dynamic.annotations@values$Metastasis_stage[rowIndex] <- metastage
        synStore(Dynamic.annotations)
        #Update menu input
        updateSelectInput(session, "mutable.metastage", label = "Change Metastasic stage here:", selected = "")
      }
      Dynamic.annotations@values$Metastasis_stage[rowIndex]
    })
  })#Synapse shiny token session end
}
