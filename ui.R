library(lubridate)
library(shinydashboard)
library(shiny)
library(shinyWidgets)

fifteen_quarters_ago_date <- today() - years(3) - months(9)
fifteen_quarters_ago_start <- floor_date(fifteen_quarters_ago_date, unit = "quarter")

nine_years_ago_date <- today() - years(9)
nine_years_ago_start <- floor_date(nine_years_ago_date, unit = "year")

this_quarter_start <- floor_date(today(), unit = "quarter")


header <- dashboardHeader(title = "Reporting Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "District Court", tabName = "districtcourt", icon = icon("balance-scale-right")),
    menuItem(text = "PTAB", tabName = "ptab", icon = icon("gavel")),
    menuItem(text = "Alice", tabName = "alice", icon = icon("file-contract")),
    br(),
    br(),
    menuItem("SQL Queries", icon = icon("external-link"), href = "https://github.com/jmillerrpx/Reporting-Dashboard/blob/master/SQL%20Queries"),
    br(),
    downloadButton("custom_export", "Custom Excel Export", style = "width: 100%"),
    br(),
    downloadButton("alice_export", "Alice Excel Export", style = "width: 100%")
  )
)

body <- dashboardBody(includeCSS("styles.css"),
                      
                      tabItems(
                        tabItem(tabName = "districtcourt",
                                
                                fluidRow(
                                  column(width = 6,
                                         box(class="dc_box", width = NULL, 
                                             dateRangeInput("defs_by_quarter_dates", "Defendant Start:", start = fifteen_quarters_ago_start, end = Sys.Date(), max = Sys.Date(),
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("defs_by_quarter_pdpef", label = "Include Pure Design Patent, E-Seller, and Franchise Litigation", value = TRUE, inline = TRUE),
                                             box(width = NULL, title = strong("Defendants Added to Patent Campaigns by Quarter"), plotOutput("defs_by_quarter_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_districts_overall_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("Top Districts (Overall)"), plotOutput("top_districts_overall_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_districts_opco_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("Top Districts (Operating Company)"), plotOutput("top_districts_opco_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_judges_npe_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("Top Judges (NPE)"), plotOutput("top_judges_npe_chart"))
                                         ),
                                         
                                  ),
                                  column(width = 6,
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("defs_by_year_dates", "Defendant Start:", start = nine_years_ago_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("defs_by_year_pdpef", "Include Pure Design Patent, E-Seller, and Franchise Litigation", value = TRUE, inline = TRUE),
                                             box(width = NULL, title = strong("Defendants Added to Patent Campaigns by Year"), plotOutput("defs_by_year_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_districts_npe_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("Top Districts (NPE)"), plotOutput("top_districts_npe_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_judges_overall_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("Top Judges (Overall)"), plotOutput("top_judges_overall_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_judges_opco_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("Top Judges (Operating Company)"), plotOutput("top_judges_opco_chart"))
                                         )
                                  )
                                )
                                
                        ),
                        
                        tabItem(tabName = "ptab",
                                
                                fluidRow(
                                  column(width = 6,
                                         box(class = "ptab_box", width = NULL,
                                             dateRangeInput("ptab_by_quarter_dates", "Petition Filed:", start = fifteen_quarters_ago_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("PTAB Filings by Quarter"), plotOutput("ptab_by_quarter_chart"))
                                         ),
                                         
                                         box(class = "ptab_box", width = NULL,
                                             dateRangeInput("institution_by_quarter_dates", "Petition Instituted:", start = fifteen_quarters_ago_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("IPR Institution Decisions by Quarter"), plotOutput("institutions_by_quarter_chart"))
                                             
                                         )
                                  ),
                                  column(width = 6,
                                         box(class = "ptab_box", width = NULL,
                                             dateRangeInput("ptab_by_year_dates", "Petition Filed:", start = ten_years_ago_start, end = Sys.Date(), max = Sys.Date(), format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("PTAB Filings by Year"), plotOutput("ptab_by_year_chart"))
                                         ),
                                         
                                         box(class = "ptab_box", width = NULL,
                                             dateRangeInput("institution_by_year_dates", "Petitions Instituted:", start = ten_years_ago_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             box(width = NULL, title = strong("IPR Institution Decisions by Year"), plotOutput("institutions_by_year_chart"))
                                         )
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "alice",
                                fluidRow(box(class = "notice_box", title = "Alice data updated quarterly. Reach out to Jake Wexler if you need more information.", width = 12)),
                                fluidRow(
                                  column(width = 6,
                                         box(class = "alice_box", width = NULL,
                                             box(width = NULL, title = "Patents with Alice Orders Before and After Berkheimer", plotOutput("patents_prepost_berkheimer_chart"))
                                         ),
                                         box(class = "alice_box", width = NULL, 
                                             box(width = NULL, title = "Patents with Alice Orders Addressing Berkheimer by Procedural Stage", plotOutput("patents_procedural_stage_chart"))
                                         ),
                                         box(class = "alice_box", width = NULL, 
                                             box(width = NULL, title = "Patents with Alice Orders by Case Type", plotOutput("patents_casetype_outcome_chart"))
                                         ),
                                         box(class = "alice_box", width = NULL, 
                                             box(width = NULL, title = "Patents with Alice Orders by Top Districts", plotOutput("patents_districts_chart"))
                                         )
                                  ),
                                  column(width = 6,
                                         box(class = "alice_box", width = NULL,
                                             box(width = NULL, title = "Patents with Alice Orders Before and After Berkheimer by Procedural Stage", plotOutput("patents_prepost_procedural_berkheimer_chart"))
                                         ),
                                         box(class = "alice_box", width = NULL,
                                             box(width = NULL, title = "Patents with Alice Orders Addressing Berkheimer by Procedural Stage and Plaintiff Type", plotOutput("patents_case_stage_and_plaintiff_type_chart"))
                                         ),
                                         box(class = "alice_box", width = NULL,
                                             box(width = NULL, title = "Patents with Alice Orders by Market Sector", plotOutput("patents_market_sector_chart"))
                                         )
                                         
                                  )
                                )
                        )
                        
                        
                      )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# server <- function(input, output, session) {}
# shinyApp(ui,server)