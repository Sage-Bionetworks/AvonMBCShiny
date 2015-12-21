
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This interface has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

#shinyUI(fluidPage(
dashboardPage(
  #   tags$head(
  #     singleton(
  #       includeScript("www/readCookie.js")
  #     )
  #   ),
  dashboardHeader(title = "Avon Metastatic Breast Cancer Abstract Analysis"),
  dashboardSidebar(  
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                       label = "Search..."),
    selectInput("grants", "Grants",selectize = T,
                choices = allTitles),
    checkboxInput('show_MBC', 'Only show MBC related grants', value = TRUE),
    
    conditionalPanel("input.show_MBC",
                     selectInput("stage","Metastatic Stage",selectize = T,
                                 choices = metaStageMenu))
    #submitButton(text = "Apply Changes:", icon = NULL, width = NULL)
    
  ),
  
  dashboardBody(
    fluidRow(
      box(title="Grant Info", width = 8,
          tags$style(type='text/css', '#AwardTitle {font-weight: bold;}'),
          textOutput("AwardTitle"),
          textOutput("PIName"),
          textOutput("Institution"),
          htmlOutput("mySite")
      ),
      box(title="MBC Annotations",width = 4,
          strong("Pathway"),
          textOutput("Pathway"),
          strong("Pathway Group"),
          textOutput("PathwayGroup"),
          strong("Molecular Target"),
          textOutput("MolecularTarget"),
          strong("Molecular Target Group"),
          textOutput("MolecularTargetGroup")
      )
      
    ),
    fluidRow(
      box(title= "Abstract",collapsible=T, collapsed = F, width = 12,
          tags$style(type='text/css', '#TechAbstract {font-size:10px;}'), 
          htmlOutput("TechAbstract")
      )
      
    ),
    fluidRow(
      box(title="Mutable Annotations",width = 4,
          strong("Pathway"),
          textOutput("mutable.Pathway"),
          tags$form(
            textInput("mutable.pathway","Input pathway here:",""),
            actionButton("button1","Save")
          )
      ),
      box(title="Mutable Annotations",width = 4,
          strong("Pathway Group"),
          textOutput("mutable.PathwayGroup"),
          tags$form(
            textInput("mutable.pathwaygroup","Input pathway group here:",""),
            actionButton("button2","Save")
          )
      ),
      box(title="Mutable Annotations",width = 4,
          strong("Molecular Target"),
          textOutput("mutable.MT"),
          tags$form(
            textInput("mutable.mt","Input molecular target here:",""),
            actionButton("button3","Save")
          )
      )
    ),
    fluidRow(
      box(title="Mutable Annotations",width = 4,
          strong("Molecular Target Group"),
          textOutput("mutable.MTGroup"),
          tags$form(
            textInput("mutable.mtgroup","Input molecular target group here:",""),
            actionButton("button4","Save")
          )
      ),
      box(title="Mutable Annotations",width = 4,
          strong("Metastasis"),
          textOutput("mutable.Metayn"),
          tags$form(
            textInput("mutable.metayn","Input Metastasis (y/n) here:",""),
            actionButton("button5","Save")
          )
      ),
      box(title="Mutable Annotations",width = 4,
          strong("Metastasis Stage"),
          textOutput("mutable.Metastage"),
          tags$form(
            textInput("mutable.metastage","Input Metastatic stage here:",""),
            actionButton("button6","Save")
          )
      )
    )
  )
)
