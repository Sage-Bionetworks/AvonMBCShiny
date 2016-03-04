
# User Interface of Avon MBC App

dashboardPage(
  dashboardHeader(title = "MBC Abstracts"),
  dashboardSidebar(  
    sidebarMenu(id = "tabs",
      menuItem("Project Dashboard", tabName = "projDash", icon = icon("dashboard")),
      menuItem("Grant Selection", tabName = "GrantSel", icon = icon("dashboard")),
      menuItem("Grant Information", tabName = "GrantInfo", icon = icon("dashboard"))
    ),
    tags$head(
      singleton(
        includeScript("www/readCookie.js")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      #Selection of grants
      tabItem(tabName = "GrantSel",
        fluidRow(
          box(title="Search", width = 4,
            p("This returns a ranked ordering of grants based on frequency of the word"),
            sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                    label = "Search...")
          ),
          box(title = "Grant Filtering", width = 4,
              checkboxInput('show_MBC', 'Only show MBC related grants', value = TRUE),
              conditionalPanel("input.show_MBC",
                               selectInput("stage","Metastatic Stage",selectize = T,
                                           choices = metaStageMenu,selected = "all"))
          ), 
          box(title="Number of Grants",width = 4,status = "info",
              textOutput("numGrants"),
              # Download box,
              strong("Download Grants"),
              downloadButton(outputId='download_data', label='Download')
          )
        ),
        
        fluidRow(
          box(title = "Grants",width = 12,
              p("Click on a grant to view its information"),
              DT::dataTableOutput('grantTitles'),
              textOutput("userLoggedIn")
          )
        )
        
      ),
      
      #Grant information
      tabItem(tabName = "GrantInfo",
        column(width = 8,
          box(title="Grant Info", width = NULL,status = "info",
              tags$style(type='text/css', '#AwardTitle {font-weight: bold; font-size: 16px;}'),
              textOutput("AwardTitle"),
              textOutput("PIName"),
              textOutput("Institution"),
              textOutput("Date"),
              htmlOutput("mySite"),
              actionButton("tabBut", "View Related San Antonio Abstracts")
          ),
          box(title= "Abstract",collapsible=T, collapsed = F, width = NULL,
              tags$style(type='text/css', '#TechAbstract {font-size:12px;}'), 
              htmlOutput("TechAbstract"),
              bsModal("modalExample", "Data Table", "tabBut", size = "large",
                      DT::dataTableOutput("sanantonio_abstracts")),
              bsModal("abstractText", "Abstract Text", "rowtogg", size="large",
                      tags$style(type='text/css', '#sanantonio_text {font-size:10px;}'),
                      htmlOutput("sanantonio_text"))
          )
          
        ),
        column(width = 4,
          
          box(title="MBC annotations",width = NULL,collapsible=T, collapsed = F,status = "info",
              tags$style(type='text/css', '#Pathway {font-size:10px;}'),
              tags$style(type='text/css', '#PathwayGroup {font-size:10px;}'),
              tags$style(type='text/css', '#MolecularTarget {font-size:10px;}'),
              tags$style(type='text/css', '#MolecularTargetGroup {font-size:10px;}'),
              tags$style(type='text/css', '#MetaYN {font-size:10px;}'),
              tags$style(type='text/css', '#MetaStage {font-size:10px;}'),
              tags$style(type='text/css', '#geneList {font-size:10px;}'),
              strong("Pathway"),
              textOutput("Pathway"),
              strong("Pathway Group"),
              textOutput("PathwayGroup"),
              strong("Molecular Target"),
              textOutput("MolecularTarget"),
              strong("Molecular Target Group"),
              textOutput("MolecularTargetGroup"),
              strong("Metastasis?"),
              textOutput("MetaYN"),
              strong("Metastasis Stage"),
              textOutput("MetaStage")
          ),
          box(title="Gene List",collapsible=T, collapsed = F,width = NULL,
              verbatimTextOutput("geneList")
          ),
          box(title="MBC Relatedness",collapsible=T, collapsed = F,width = NULL,
              textOutput("mutable.Metayn"),
              strong("Probability that grant is MBC related:"),
              textOutput("MetaYNPostProb"),
              tags$form(
                selectInput("mutable.metaynmenu","Change Metastasis (y/n) here:",selectize = T,
                            choices = c("","yes","no")),
                actionButton("button5","Save")
              ),
              strong("Last Updated by:"),
              htmlOutput("mutable.MetaYN.User")
          ),
          box(title="Metastatic Stage",collapsible=T, collapsed = F,width = NULL,
              textOutput("mutable.Metastage"),
              plotOutput("MetaStagePostProb"),
              tags$form(
                selectInput("mutable.metastagemenu","Change Metastasic stage here:",selectize = T,
                            choices = c("",metaStageMenu)),
                actionButton("button6","Save")
              ),
              strong("Last Updated by:"),
              htmlOutput("mutable.Metastage.User")
          ),
          box(title="Molecular Target", collapsible = T, collapsed = F, width=NULL,
              verbatimTextOutput("mutable.MT"),
              tags$form(
                selectInput("mutable.mtmenu","Select Molecular Target here:",selectize = T,multiple = T,
                            choices = c("")),
                actionButton("button3","Save")
              ),
              strong("Last Updated by:"),
              htmlOutput("mutable.MT.User")
          )
         
          
        )#,
#         fluidRow(
#           box(title="Mutable Annotations",width = 4,
#               strong("Pathway"),
#               textOutput("mutable.Pathway"),
#               tags$form(
#                 textInput("mutable.pathway","Input pathway here:",""),
#                 actionButton("button1","Save")
#               )
#           ),
#           box(title="Mutable Annotations",width = 4,
#               strong("Pathway Group"),
#               textOutput("mutable.PathwayGroup"),
#               tags$form(
#                 textInput("mutable.pathwaygroup","Input pathway group here:",""),
#                 actionButton("button2","Save")
#               )
#           ),
#           box(title="Mutable Annotations",width = 4,
#               strong("Molecular Target"),
#               textOutput("mutable.MT"),
#               tags$form(
#                 textInput("mutable.mt","Input molecular target here:",""),
#                 actionButton("button3","Save")
#               )
#           )
#         ),
#         fluidRow(
#           box(title="Mutable Annotations",width = 4,
#               strong("Molecular Target Group"),
#               textOutput("mutable.MTGroup"),
#               tags$form(
#                 textInput("mutable.mtgroup","Input molecular target group here:",""),
#                 actionButton("button4","Save")
#               )
#           ),
#           box(title="Mutable Annotations",width = 4,
#               strong("Metastasis"),
#               textOutput("mutable.Metayn"),
#               tags$form(
#                 textInput("mutable.metayn","Input Metastasis (y/n) here:",""),
#                 actionButton("button5","Save")
#               )
#           ),
#           box(title="Mutable Annotations",width = 4,
#               strong("Metastasis Stage"),
#               textOutput("mutable.Metastage"),
#               tags$form(
#                 textInput("mutable.metastage","Input Metastatic stage here:",""),
#                 actionButton("button6","Save")
#               )
#           )
#         )
      ),#Tab item end
      # Project Dashboard
      tabItem(tabName = "projDash",
        fluidRow(
          box(title="MBC Distribution",width=6,
              plotOutput("dashboard_metaYN")
          ),
          box(title="Metastatic Stage Distribution",width=6,
              plotOutput("dashboard_metastage"),
              verbatimTextOutput("dashboard_metastageLegend")
          )
        ),
        fluidRow(
          box(title="MBC Posterior Probability Density Plot",width=6,
              plotOutput("dashboard_postmetaYN"),
              verbatimTextOutput("dashboard_metaYN_stats")
          ),
          box(title="Metastatis Stage Posterior Probability Density Plots",width=6,
              plotOutput("dashboard_postmetaStage"),
              verbatimTextOutput("dashboard_metastage_stats")
            
          )
        )
      )#Dashboard tab end
    )
  )
)
