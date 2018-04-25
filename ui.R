library(shiny)
library(shinydashboard)
library(markdown)
library(shinycssloaders)

ui <- dashboardPage(title="How do you feel about Facebook?",
  skin="black",
  dashboardHeader(
    title=span(img(src="mozilla.png",alt="Mozilla", width = 100)),titleWidth = "230px"
  ),
  dashboardSidebar(width="230px",
                   h2("Facebook Survey Results"),
                   sidebarMenu(
                     menuItem("Explore Data", tabName = "explore",icon = icon("bar-chart-o")),
                     menuItem("About the Survey", tabName = "about",icon = icon("info-circle")),
                     menuItem("Download Data", tabName = "download",icon = icon("download"))
                   )
                   
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem("explore",
              fluidRow(
                box(
                  width = 6, status = "primary", solidHeader = TRUE,
                  title = "Select a question:",
                  uiOutput("questionDropdown")
                ),
                box(
                  width = 3, status = "primary", solidHeader = TRUE,
                  title = "Compare results by:",
                  uiOutput("compareDropdown")
                ),
                box(
                  width = 3, status = "primary", solidHeader = TRUE,
                  title = "Filter results by:",
                  uiOutput("selectorDropdown"),
                  conditionalPanel(
                    condition = "input.filterSelector!='None'",
                    uiOutput("inSelectorDropdown")
                  )
                )
              ),
              fluidRow(
                tabBox(
                  id="main1",width=12,
                  tabPanel("Plot",
                           div(
                             width=12,
                             uiOutput("message")
                           ),
                           withSpinner(plotOutput("answerPlot",height="650px"),type=3,color.background="#ffffff",color="#3C8DBC")
                  ),
                  tabPanel("Data",
                           div(id="dataWrap",
                               conditionalPanel(
                                 condition="input.compareSelector!='None'",
                                 tabBox(id="tabs",width=12,
                                        tabPanel("Sample size",
                                                 tableOutput("totalSamples")
                                        ),
                                        tabPanel("Share (in %)",
                                                 tableOutput("answerTable")
                                        ),
                                        tabPanel("Total #",
                                                 tableOutput("valueTable")
                                        )
                                 ) 
                               ),
                               conditionalPanel(
                                 condition="input.compareSelector=='None'",
                                 h4("Results"),
                                 tableOutput("simpleTable")
                               )
                           )
                  )  
                )
              )
      ),
      tabItem("about",
              includeMarkdown("www/about.md")
      ),
      tabItem("download",
              h3("You can download all 46,619 anonymized results from the survey (.csv file, 31MB)"),
              p(
                downloadButton("download", "Download results")
              ),
              p("Mozilla is making this data available under a CC BY 4.0 license")
      )
    )
  )
)