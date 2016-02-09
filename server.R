
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
  
    # ---------------------------------------------
    # Grants (allow for querying)
    # ---------------------------------------------
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
        table.df <- grant.df[grant.df$Metastasis_YN == 'no',]
      }
      table.df
    })
    
    # ------------------------------------------------------------
    # Grant Selection Page
    # ------------------------------------------------------------
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
      nrow(table.df)
    })
    
    # ---------------------------------------------
    # STATIC CONTENT - GRANT INFORMATION
    # ---------------------------------------------
    
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
    
    #Gene list annotation
    output$geneList <- renderText({
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      text <- table.df[rowIndex, "gene_list"]
    })
    
    #Display of confidence of metastatic YN classifier
    output$MetaYNPostProb <- renderText({
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      if (table.df[rowIndex,"Y_meta"] >= 0.5) {
        paste0(round(table.df[rowIndex,"Y_meta"]*100,2),"%")
      } else {
        paste0(round(table.df[rowIndex,"N_meta"]*100,2),"%")
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
    # ------------------------------------
    # SAN ANTONIO ABSTRACTS
    # ------------------------------------
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
    
    # ------------------------------------------------------
    # Observe Events
    # ------------------------------------------------------
    #If you click on a san antonio abstract, a pop up of the abstract text will appear
    observeEvent({
      #input[[paste("sanantonio_abstracts","rows_selected",sep="_")]]
      input$sanantonio_abstracts_rows_selected
    }, {
      toggleModal(session, "abstractText","open")
    })
    
    # When you click on a grant, go to grant info
    observeEvent({
      input$grantTitles_rows_selected
    }, {
      table.df <- tableQuery()
      rowIndex <- input$grantTitles_rows_selected
      text <- table.df[rowIndex, "gene_list"]
      text <- unlist(strsplit(text,"\n"))
      updateTabItems(session, "tabs","GrantInfo")
      updateSelectInput(session, "mutable.mtmenu", label = "Select Molecular Target here:", choices = c("",text), selected = "")
    })

    # ---------------------------------------------
    # Project Dashboard
    # ---------------------------------------------
    
    output$dashboard_metastage <- renderPlot({
      bars = sort(table(tolower(grant.MBC$Metastasis_stage)))
     # predicted = sort(table(paste0("predicted_",grant.MBC$Predicted_metastage)))
      predicted = sort(table(grant.MBC$Predicted_metastage))
      
      total <- c(bars,predicted)
      total <- total[order(names(total))]
      names(total)[names(total) == "arrest & extravasation"] <- "A&E"
      names(total)[names(total) == "immune surveillance/escape"] <- "IS&E"
      names(total)[names(total) == "intravasation & circulation"] <- "I&C"
      names(total)[names(total) == "metabolic deregulation"] <- "MD"
      names(total)[names(total) == "metastatic colonization"] <- "MC"
      names(total)[names(total) == "other/not specified"] <- "NA"
      
      #total <- table(grant.MBC$Predicted_metastage,tolower(grant.MBC$Metastasis_stage))
      par(las=2)
      f <- barplot(total,
                   main="Distribution of Metastatic Stages",
                   cex.names = 0.75,ylim = c(0,1500),ylab = "Number of Grants",beside=TRUE,
                   col=c("grey","red","grey","red","grey","red","grey","red","grey","red","grey","red","grey","grey"))
      text(x=f,
           y=total,
           label=total,po=3) 
      legend("topright",pch = c(19,19),
             col=c("grey","red"),
             legend = c("Avon","Predicted"))
      f
    })
    output$dashboard_metastageLegend <- renderText(
      string <- "A&E = arrest & extravasation \nIS&E = immune surveillance/escape \nI&C = intravasation & circulation \nMD = metabolic deregulation \nMC = metastatic colonization \nNA = other/not specified"
    )
    
    output$dashboard_metaYN <- renderPlot({
      bars = table(grant.df$Metastasis_YN)
      predicted = table(grant.df$Predicted_metaYN)
      
      total = c(bars,predicted)
      total <- total[order(names(total))]
      YN <- barplot(total,
                 main = "Distribution of Grants",
                 ylim = c(0,15500),xlab="Metastatic Breast Cancer Related",
                 ylab = "Number of Grants",
                 col=c("grey","red"))
      text(x = YN, y=total, label=total,po=3)
      legend("topright",pch = c(19,19),
             col=c("grey","red"),
             legend = c("Avon","Predicted"))
      YN
    })
    
    output$dashboard_postmetaYN <- renderPlot({
      Yes = grant.df[grant.df$Metastasis_YN == "yes","Y_meta"]
      Yes = round(Yes,2)
      No = grant.df[grant.df$Metastasis_YN == "no","Y_meta"]
      No = round(No,2)
      plot(density(Yes),main = "MBC Relatedness",ylim=c(0,60))
      lines(density(No),col="red")
      legend("topright",pch = c(19,19),
             col=c("black","red"),
             legend = c("True MBC","True Non-MBC"))
    })
    
    output$dashboard_postmetaStage <- renderPlot({
      invasion = grant.MBC$invasion_meta[tolower(grant.MBC$Metastasis_stage) =="invasion"]
      arrest = grant.MBC$arrest_meta[tolower(grant.MBC$Metastasis_stage) =="arrest & extravasation"]
      immune = grant.MBC$immune_meta[tolower(grant.MBC$Metastasis_stage) =="immune surveillance/escape"]
      metastatic = grant.MBC$metastatic_meta[tolower(grant.MBC$Metastasis_stage) =="metastatic colonization"]
      intravasation = grant.MBC$intravasatsion_meta[tolower(grant.MBC$Metastasis_stage) =="intravasation & circulation"]
      metabolic = grant.MBC$metabolic_meta[tolower(grant.MBC$Metastasis_stage) =="metabolic deregulation"]
      par(mfrow = c(3,2))
      plot(density(round(invasion,2),na.rm = T), main = "Invasion")
      plot(density(round(arrest,2),na.rm=T), main = "Arrest and Extravasation")
      plot(density(round(immune,2),na.rm=T), main = "Immune Surveillance/escape")
      plot(density(round(metastatic,2),na.rm=T), main = "Metastatic colonization")
      plot(density(round(intravasation,2),na.rm=T), main = "Intravasation and Circulation")
      plot(density(round(metabolic,2),na.rm=T),main = "Metabolic Deregulation")
    })
    
    output$dashboard_metaYN_stats <- renderText({
      scores <- confusionMatrix_scores(true = grant.df$Metastasis_YN,pred = grant.df$Predicted_metaYN,positive = "yes")
      paste0("Sensitivity (TPR): ",scores[1],"\nSpecificity (TNR): ",scores[2])
    })

    
    output$dashboard_metastage_stats <- renderText({
      invasion <- confusionMatrix_scores(true = tolower(grant.MBC$Metastasis_stage),pred = grant.MBC$Predicted_metastage,positive = "invasion")
      arrest <- confusionMatrix_scores(true = tolower(grant.MBC$Metastasis_stage),pred = grant.MBC$Predicted_metastage,positive = "arrest & extravasation")
      immune <- confusionMatrix_scores(true = tolower(grant.MBC$Metastasis_stage),pred = grant.MBC$Predicted_metastage,positive = "immune surveillance/escape")
      metastatic <- confusionMatrix_scores(true = tolower(grant.MBC$Metastasis_stage),pred = grant.MBC$Predicted_metastage,positive = "metastatic colonization")
      intravasation <- confusionMatrix_scores(true = tolower(grant.MBC$Metastasis_stage),pred = grant.MBC$Predicted_metastage,positive = "intravasation & circulation")
      metabolic <- confusionMatrix_scores(true = tolower(grant.MBC$Metastasis_stage),pred = grant.MBC$Predicted_metastage,positive = "metabolic deregulation")
      paste("invasion:",
             sprintf("Sensitivity (TPR) - %.2f",invasion[1]),
             sprintf("Specificity (TNR) - %.2f ",invasion[2]),
             "arrest & extravasation:",
             sprintf("Sensitivity (TPR) - %.2f",arrest[1]),
             sprintf("Specificity (TNR) - %.2f",arrest[2]),
             "immune surveillance/escape:",
             sprintf("Sensitivity (TPR) - %.2f",immune[1]),
             sprintf("Specificity (TNR) - %.2f",immune[2]),
             "metastatic colonization:",
             sprintf("Sensitivity (TPR) - %.2f",metastatic[1]),
             sprintf("Specificity (TNR) - %.2f",metastatic[2]),
             "intravasation & circulation:",
             sprintf("Sensitivity (TPR) - %.2f",intravasation[1]),
             sprintf("Specificity (TNR) - %.2f",intravasation[2]),
             "metabolic deregulation:",
             sprintf("Sensitivity (TPR) - %.2f",metabolic[1]),
             sprintf("Specificity (TNR) - %.2f",metabolic[2]),
             sep="\n")
    })
    # ---------------------------------------------
    # Dynamic Content - Assist in grant curation
    # ---------------------------------------------
    output$mutable.MT <- renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      input$button3
      mt <- isolate(input$mutable.mtmenu)
      title = table.df[rowIndex,"AwardTitle"]
      Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
      rowIndex <- which(Dynamic.annotations@values$AwardTitle == title)
      #if (mt != "") {
      #  Dynamic.annotations@values$Molecular_Target[rowIndex] <- mt
      #  synStore(Dynamic.annotations)
      #  updateSelectInput(session, "mutable.mtmenu", label = "Select Molecular Target here:",selected = "")
      #}
      Dynamic.annotations@values$Molecular_Target[rowIndex]
    })
    
    output$mutable.Metayn <- renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      input$button5
      metayn <- isolate(input$mutable.metaynmenu)
      title = table.df[rowIndex,"AwardTitle"]
      Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
      rowIndex <- which(Dynamic.annotations@values$AwardTitle == title)
      if (metayn != "") {
        Dynamic.annotations@values$Metastasis_YN[rowIndex] <- metayn
        synStore(Dynamic.annotations)
        updateSelectInput(session, "mutable.metaynmenu", label = "Change Metastasis (y/n) here:",selected = "")
      }
      if (Dynamic.annotations@values$Metastasis_YN[rowIndex] =='y'){
        'yes'
      } else {
        'no'
      }
    })
    
    output$mutable.Metastage <- renderText({
      table.df <- tableQuery() 
      rowIndex<-input$grantTitles_rows_selected
      input$button6
      metastage <- isolate(input$mutable.metastagemenu)
      title = table.df[rowIndex,"AwardTitle"]
      Dynamic.annotations <-synTableQuery("SELECT * FROM syn5584661",filePath = ".")
      rowIndex <- which(Dynamic.annotations@values$AwardTitle == title)
      if (metastage != "") {
        Dynamic.annotations@values$Metastasis_stage[rowIndex] <- metastage
        synStore(Dynamic.annotations)
        #Update menu input
        updateSelectInput(session, "mutable.metastagemenu", label = "Change Metastasic stage here:", selected = "")
      }
      Dynamic.annotations@values$Metastasis_stage[rowIndex]
    })
  })#Synapse shiny token session end
}
