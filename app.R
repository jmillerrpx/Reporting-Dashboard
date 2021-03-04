library(shinydashboard)
library(shiny)
library(ggplot2)
source("ggplot_charts.R")

header <- dashboardHeader(title = "Reporting Dashboard")
                          
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "District Court", tabName = "districtcourt"),
    menuItem(text = "PTAB", tabName = "ptab"),
    menuItem(text = "Alice", tabName = "alice")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "districtcourt",
            
    fluidRow(
      column(width = 6,
             box(width = NULL, title = "Defendants Added to Patent Campaigns by Quarter", plotOutput("defs_by_quarter_chart")),
             box(width = NULL, title = "Top Districts (Overall)", plotOutput("top_districts_overall_chart")),
             box(width = NULL, title = "Top Districts (Operating Company)", plotOutput("top_districts_opco_chart"))
      ),
      column(width = 6,
             box(width = NULL, title = "Defendants Added to Patent Campaigns by Year", plotOutput("defs_by_year_chart")),
             box(width = NULL, title = "Top Districts (NPE)", plotOutput("top_districts_npe_chart")),
             box(width = NULL, title = "Defendants Added to Litigation by NPEs, Percent Change")
      )
    )
    ),
    tabItem(tabName = "ptab",
            
    fluidRow(
      column(width = 6,
             box(width = NULL, title = "PTAB Filings by Quarter", plotOutput("ptab_by_quarter_chart")),
             box(width = NULL, title = "Institution Decisions by Quarter")
      ),
      column(width = 6,
             box(width = NULL, title = "PTAB Filings by Year", plotOutput("ptab_by_year_chart")),
             box(width = NULL, title = "Institution Decisions by Year")
      )
    )
    ),
    tabItem(tabName = "alice",
            
    fluidRow(
      column(width = 6,
             box(width = NULL, title = "Patents Invalidated Under Alice Before and After Berkheimer"),
             box(width = NULL, title = "Patents with Alice Orders Addressing Berkheimer by Procedural Stage"),
             box(width = NULL, title = "Patents Invalidated Under Alice Since the Decision’s Issuance")
      ),
      column(width = 6,
             box(width = NULL, title = "Patents Invalidated Under Alice Before and After Berkheimer by Procedural Stage"),
             box(width = NULL, title = "Patents with Alice Orders Addressing Berkheimer by Procedural Stage and Plaintiff Type")
      )
    )
    )
  )
)


ui <- dashboardPage(
                    header = header,
                    sidebar = sidebar,
                    body = body
                    )

server <- function(input, output, session) {
  output$defs_by_quarter_chart <- renderPlot({
    defs_by_quarter_chart
  })
  
  output$defs_by_year_chart <- renderPlot({
    defs_by_year_chart
  })
  
  output$top_districts_overall_chart <- renderPlot({
    top_districts_overall_chart
  })
  
  output$top_districts_npe_chart <- renderPlot({
    top_districts_npe_chart
  })
  
  output$top_districts_opco_chart <- renderPlot({
    top_districts_opco_chart
  })
  
  output$ptab_by_quarter_chart <- renderPlot({
    ptab_by_quarter_chart
  })
  
  output$ptab_by_year_chart <- renderPlot({
    ptab_by_year_chart
  })
  
  
}
  
shinyApp(ui,server)

