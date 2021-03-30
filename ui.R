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
    downloadButton("blog_post_export", "Blog Post Export", style = "width: 100%")
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
                                             materialSwitch("defs_by_quarter_export", label = "Include in Export", value = TRUE, inline = TRUE),
                                             materialSwitch("defs_by_quarter_design", label = "Include Design Patent Litigation", value = TRUE, inline = TRUE),
                                             box(width = NULL, title = strong("Defendants Added to Patent Campaigns by Quarter"), plotOutput("defs_by_quarter_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_districts_overall_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("top_districts_overall_check", "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("Top Districts (Overall)"), plotOutput("top_districts_overall_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_districts_opco_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("top_districts_opco_check", "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("Top Districts (Operating Company)"), plotOutput("top_districts_opco_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_judges_npe_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("top_judges_npe_check", "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("Top Judges (NPE)"), plotOutput("top_judges_npe_chart"))
                                         ),
                                         
                                  ),
                                  column(width = 6,
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("defs_by_year_dates", "Defendant Start:", start = nine_years_ago_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("defs_by_year_export", "Include In Export", value = TRUE, inline = TRUE),
                                             materialSwitch("defs_by_year_design", "Include Design Patent Litigation", value = TRUE, inline = TRUE),
                                             box(width = NULL, title = strong("Defendants Added to Patent Campaigns by Year"), plotOutput("defs_by_year_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_districts_npe_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("top_districts_npe_check",  "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("Top Districts (NPE)"), plotOutput("top_districts_npe_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_judges_overall_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("top_judges_overall_check", "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("Top Judges (Overall)"), plotOutput("top_judges_overall_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("top_judges_opco_dates", "Defendant Start:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("top_judges_opco_check", "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("Top Judges (Operating Company)"), plotOutput("top_judges_opco_chart"))
                                         ),
                                         
                                         box(class="dc_box", width = NULL,
                                             dateRangeInput("percentage_change_dates_first", "First Period:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             dateRangeInput("percentage_change_dates_second", "Second Period:", start = this_quarter_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("percentage_change_check",  "Include In Export", value = TRUE)
                                             # box(width = NULL, title = strong("Defendants Added to Litigation by NPEs, Percent Change"), plotOutput("percentage_change_chart"))
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
                                             materialSwitch("ptab_by_quarter_check", "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("PTAB Filings by Quarter"), plotOutput("ptab_by_quarter_chart"))
                                         ),
                                         
                                         box(class = "ptab_box", width = NULL,
                                             dateRangeInput("institution_by_quarter_dates", "Petition Instituted:", start = fifteen_quarters_ago_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("institution_quarter_check",  "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("IPR Institution Decisions by Quarter"), plotOutput("institutions_by_quarter_chart"))
                                             
                                         )
                                  ),
                                  column(width = 6,
                                         box(class = "ptab_box", width = NULL,
                                             dateRangeInput("ptab_by_year_dates", "Petition Filed:", start = ten_years_ago_start, end = Sys.Date(), max = Sys.Date(), format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("ptab_by_year_check", "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("PTAB Filings by Year"), plotOutput("ptab_by_year_chart"))
                                         ),
                                         
                                         box(class = "ptab_box", width = NULL,
                                             dateRangeInput("institution_by_year_dates", "Petitions Instituted:", start = ten_years_ago_start, end = Sys.Date(), max = Sys.Date(), 
                                                            format = "mm-dd-yyyy", width = '40%'),
                                             materialSwitch("institution_year_check",  "Include In Export", value = TRUE),
                                             box(width = NULL, title = strong("IPR Institution Decisions by Year"), plotOutput("institutions_by_year_chart"))
                                         )
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "alice",
                                
                                fluidRow(
                                  column(width = 6,
                                         box(class = "ptab_box",
                                             width = NULL, title = "Patents Invalidated Under Alice Before and After Berkheimer"),
                                         box(class = "alice_box", 
                                             width = NULL, title = "Patents with Alice Orders Addressing Berkheimer by Procedural Stage"),
                                         box(class = "alice_box",
                                             width = NULL, title = "Patents Invalidated Under Alice Since Decision Issuance")
                                  ),
                                  column(width = 6,
                                         box(width = NULL, title = "Patents Invalidated Under Alice Before and After Berkheimer by Procedural Stage"),
                                         box(width = NULL, title = "Patents with Alice Orders Addressing Berkheimer by Procedural Stage and Plaintiff Type")
                                  )
                                )
                        )
                        
                        
                      )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# server <- function(input, output, session) {}
# shinyApp(ui,server)