
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

  dashboardHeader(title = "Avon-MBC Abstract Analysis"),
  dashboardSidebar(  
    sidebarMenu(id = "tabs",
      menuItem("Grant Selection", tabName = "GrantSel", icon = icon("dashboard")),
      menuItem("Grant Information", tabName = "GrantInfo", icon = icon("dashboard"))
    )#,
    #tags$head(
    #  singleton(
    #    includeScript("www/readCookie.js")
    #  )
    #)
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
                                           choices = metaStageMenu))
          ), 
          box(title="Number of Grants",width = 4,status = "info",
                 textOutput("numGrants")
          )
        ),
        
        fluidRow(
          box(title = "Grants",width = 12,
              p("Click on a grant to view its information"),
              DT::dataTableOutput('grantTitles')
          )
        )
        
      ),
      
      #Grant information
      tabItem(tabName = "GrantInfo",
        column(width = 8,
          box(title="Grant Info", width = NULL,status = "info",
              tags$style(type='text/css', '#AwardTitle {font-weight: bold;}'),
              textOutput("AwardTitle"),
              textOutput("PIName"),
              textOutput("Institution"),
              htmlOutput("mySite")
          ),
          box(title= "Abstract",collapsible=T, collapsed = F, width = NULL,
              tags$style(type='text/css', '#TechAbstract {font-size:14px;}'), 
              htmlOutput("TechAbstract")
          )
          
        ),
#         fluidRow(
#           box(title="MBC Annotations",width = 4,status = "info",
# 
#           ),
#           box(title="MBC Annotations",width = 4,status = "info",
# 
#           ),
#           box(title="MBC Annotations",width = 4,status = "info",
# 
#           )
#           
#         ),
        column(width = 4,
          
          box(title="MBC annotations",width = NULL,status = "info",
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
          box(title="Mutable Annotations",width = NULL,
                            strong("Metastasis"),
                            textOutput("mutable.Metayn"),
                            tags$form(
                              selectInput("mutable.metayn","Change Metastasis (y/n) here:",selectize = T,
                                          choices = c("","yes" = "y","no" ="n")),
                              actionButton("button5","Save")
                            )
          ),
          box(title="Mutable Annotations",width = NULL,
                            strong("Metastasis Stage"),
                            textOutput("mutable.Metastage"),
                            tags$form(
                              selectInput("mutable.metastage","Change Metastasic stage here:",selectize = T,
                                          choices = c("",allmetaStage)),
                              actionButton("button6","Save")
                            )
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
      )#Tab item end
    )
  )
)
