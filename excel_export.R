library(openxlsx)
library(dplyr)
source("data_sets.R")

load_template <- loadWorkbook(file = "ReportingDashboardExport.xlsx")
excel_export <- copyWorkbook(load_template)

data_label_format <-  createStyle(
                  fontName = "Arial",
                  fontSize = 11,
                  fontColour = rgb(0,0,0, maxColorValue = 255),
                  numFmt = "COMMA",
                  halign = "center",
                  valign = "center"
                  )



percentage_format <-  createStyle(
                    fontName = "Arial",
                    fontSize = 11,
                    fontColour = rgb(0,0,0, maxColorValue = 255),
                    numFmt = "0%",
                    halign = "center",
                    valign = "center"
                  )


#### DISTRICT COURT ####
  # Defendants Added by Quarter
    writeData(excel_export, sheet = "Defs Added by Quarter", x = excel_defs_by_quarter, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(excel_export, sheet = "Defs Added by Quarter", style = x_axis_format, rows = 2:100, cols = 1:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Defs Added by Quarter", style = data_label_format, rows = 2:100, cols = 2:100, gridExpand = TRUE)
  
  # Defendants Added by Year
    writeData(excel_export, sheet = "Defs Added by Year", x = excel_defs_by_year, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(excel_export, sheet = "Defs Added by Year", style = x_axis_format, rows = 2:20, cols = 1:20, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Defs Added by Year", style = data_label_format, rows = 2:20, cols = 2:20, gridExpand = TRUE)
  
  # Top Districts (Overall)
    writeData(excel_export, sheet = "Top Districts (Overall)", x = excel_top_districts_overall, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(excel_export, sheet = "Top Districts (Overall)", style = x_axis_format, rows = 2:100, cols = 1:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Top Districts (Overall)", style = data_label_format, rows = 2:100, cols = 2:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Top Districts (Overall)", style = percentage_format, rows = 2:100, cols = 3:100, gridExpand = TRUE)
      
  # Top Districts (NPE)
    writeData(excel_export, sheet = "Top Districts (NPE)", x = excel_top_districts_npe, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(excel_export, sheet = "Top Districts (NPE)", style = x_axis_format, rows = 2:100, cols = 1:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Top Districts (NPE)", style = data_label_format, rows = 2:100, cols = 2:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Top Districts (NPE)", style = percentage_format, rows = 2:100, cols = 3:100, gridExpand = TRUE)

  # Top Districts (Operating Company)
    writeData(excel_export, sheet = "Top Districts (OpCo)", x = excel_top_districts_opco, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(excel_export, sheet = "Top Districts (OpCo)", style = x_axis_format, rows = 2:100, cols = 1:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Top Districts (OpCo)", style = data_label_format, rows = 2:100, cols = 2:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "Top Districts (OpCo)", style = percentage_format, rows = 2:100, cols = 3:100, gridExpand = TRUE)  
      
#### PTAB ####
  # Petitions Filed by Quarter
    writeData(excel_export, sheet = "PTAB Filings by Quarter", x = excel_ptab_by_quarter, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(excel_export, sheet = "PTAB Filings by Quarter", style = x_axis_format, rows = 2:100, cols = 1:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "PTAB Filings by Quarter", style = data_label_format, rows = 2:100, cols = 2:100, gridExpand = TRUE)
      
  # Petitions Filed by Year    
    writeData(excel_export, sheet = "PTAB Filings by Year", x = excel_ptab_by_year, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(excel_export, sheet = "PTAB Filings by Year", style = x_axis_format, rows = 2:100, cols = 1:100, gridExpand = TRUE)
      addStyle(excel_export, sheet = "PTAB Filings by Year", style = data_label_format, rows = 2:100, cols = 2:100, gridExpand = TRUE)
    
saveWorkbook(excel_export, "text1.xlsx", overwrite = TRUE)


