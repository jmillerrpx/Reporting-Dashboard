library(dplyr)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("sql_data.R")
source("ui.R")

server <- function(input, output, session) {
  
  # District Court Section --------------------------------------------------
  defs_by_quarter_filtered <- reactive({
    
    # District Court Defendants by Quarter (With PDPEF Litigation)
    if(input$defs_by_quarter_pdpef == TRUE) {
      
      defs_by_quarter <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_quarter_dates[1] & defendant_started <= input$defs_by_quarter_dates[2]) %>%
        mutate(case_type = factor(case_type, levels=c("PDPEF","Operating Company","NPE"))) %>%
        group_by(quarter_started_ggplot, case_type) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        ungroup()
      defs_by_quarter
      
    } else {
      
      # District Court Defendants by Quarter
      defs_by_quarter <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_quarter_dates[1] & defendant_started <= input$defs_by_quarter_dates[2], case_type != "PDPEF") %>%
        mutate(case_type = factor(case_type, levels=c("PDPEF","Operating Company","NPE"))) %>%
        group_by(quarter_started_ggplot, case_type) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        ungroup()
      defs_by_quarter
      
    }
    
  })
  
  output$defs_by_quarter_chart <- renderPlot({
    
    
    if(input$defs_by_quarter_pdpef == TRUE) {
      
      # District Court Defendants by Quarter Chart (With PDPEF Litigation)
      defs_by_quarter_chart <-ggplot(defs_by_quarter_filtered(), aes(quarter_started_ggplot, defendant_count, fill = case_type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
        coord_cartesian(clip = "off") +
        scale_color_manual(values = c("PDPEF" = "black", "NPE" = "white", "Operating Company" = "white")) +
        scale_fill_manual(values = c(rgb(186,188,192,maxColorValue = 255),rgb(0,119,191,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
        guides(fill = guide_legend(reverse = TRUE)) +
        geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = quarter_started_ggplot), stat = 'summary', fun = sum, vjust = -1) +
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 11, color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.position = 'bottom',
          rect = element_blank()
        )
      defs_by_quarter_chart
      
    } else {
      
      # District Court Defendants by Quarter Chart
      defs_by_quarter_chart <-ggplot(defs_by_quarter_filtered(), aes(quarter_started_ggplot, defendant_count, fill = case_type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
        coord_cartesian(clip = "off") +
        scale_color_manual(values = c("NPE" = "white", "Operating Company" = "white")) +
        scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
        guides(fill = guide_legend(reverse = TRUE)) +
        geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = quarter_started_ggplot), stat = 'summary', fun = sum, vjust = -1) +
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 11, color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.position = 'bottom',
          rect = element_blank()
        )
      defs_by_quarter_chart
    }
    
  })
  
  
  defs_by_year_filtered <- reactive({
    
    if(input$defs_by_year_pdpef == TRUE) {
      
      # District Court Defendants by Year (With PDPEF Litigation)
      defs_by_year <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_year_dates[1] & defendant_started <= input$defs_by_year_dates[2]) %>%
        mutate(case_type = factor(case_type, levels=c("PDPEF","Operating Company","NPE"))) %>%
        group_by(year, case_type) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        ungroup()
      defs_by_year
      
    } else {
      
      # District Court Defendants by Year
      defs_by_year <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_year_dates[1] & defendant_started <= input$defs_by_year_dates[2], case_type != "PDPEF") %>%
        mutate(case_type = factor(case_type, levels=c("PDPEF","Operating Company","NPE"))) %>%
        group_by(year, case_type) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        ungroup()
      defs_by_year
      
    }
    
  })
  
  output$defs_by_year_chart <- renderPlot({
    
    if(input$defs_by_year_pdpef == TRUE) {
      
      # District Court Defendants by Year Chart (With PDPEF Patent Litigation)
      defs_by_year_chart <-ggplot(defs_by_year_filtered(), aes(year, defendant_count, fill = case_type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
        coord_cartesian(clip = "off") +
        scale_color_manual(values = c("PDPEF" = "black", "NPE" = "white", "Operating Company" = "white")) +
        scale_fill_manual(values = c(rgb(186,188,192,maxColorValue = 255),rgb(0,119,191,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
        guides(fill = guide_legend(reverse = TRUE)) +
        geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = year), stat = 'summary', fun = sum, vjust = -1) +
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 11, color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.position = 'bottom',
          rect = element_blank()
        )
      defs_by_year_chart
      
    } else {
      
      # District Court Defendants by Year Chart
      defs_by_year_chart <-ggplot(defs_by_year_filtered(), aes(year, defendant_count, fill = case_type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
        coord_cartesian(clip = "off") +
        scale_color_manual(values = c("NPE" = "white", "Operating Company" = "white")) +
        scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
        guides(fill = guide_legend(reverse = TRUE)) +
        geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = year), stat = 'summary', fun = sum, vjust = -1) +
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 11, color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.position = 'bottom',
          rect = element_blank()
        )
      defs_by_year_chart
      
    }
    
  })
  
  
  top_districts_overall_filtered <- reactive({
    
    # Top Districts (Overall)
    top_districts_overall <- top_districts_and_judges() %>%
      filter(defendant_started_in_district >= input$top_districts_overall_dates[1] & defendant_started_in_district <= input$top_districts_overall_dates[2], case_type != "PDPEF") %>%
      group_by(court_common) %>%
      summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
      top_n(5) %>%
      select(court_common, defendant_count) %>%
      arrange(defendant_count)
    top_districts_overall
    
  })
  
  output$top_districts_overall_chart <- renderPlot({
    
    #Top Districts (Overall) Chart
    top_districts_overall_chart <- ggplot(top_districts_overall_filtered(), aes(reorder(court_common, defendant_count), defendant_count)) +
      geom_bar(stat = "identity", fill = rgb(81,81,81,maxColorValue = 255)) +
      coord_flip() +
      geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), hjust = -.5), show.legend = FALSE) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        rect = element_blank()
      )
    top_districts_overall_chart
    
  })
  
  
  top_districts_npe_filtered <- reactive({
    
    # Top Districts (NPE)
    top_districts_npe <- top_districts_and_judges() %>%
      filter(defendant_started_in_district >= input$top_districts_npe_dates[1] & defendant_started_in_district <= input$top_districts_npe_dates[2], case_type == "NPE") %>%
      group_by(court_common) %>%
      summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
      top_n(5) %>%
      select(court_common, defendant_count) %>%
      arrange(defendant_count)
    top_districts_npe
    
  })
  
  output$top_districts_npe_chart <- renderPlot({
    
    #Top Districts (NPE) Chart
    top_districts_npe_chart <- ggplot(top_districts_npe_filtered(), aes(reorder(court_common, defendant_count), defendant_count)) +
      geom_bar(stat = "identity", fill = rgb(246,138,29,maxColorValue = 255)) +
      coord_flip() +
      geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), hjust = -.5), show.legend = FALSE) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        rect = element_blank()
      )
    top_districts_npe_chart
    
  })
  
  
  top_districts_opco_filtered <- reactive({
    
    # Top Districts (Operating Company)
    top_districts_opco <- top_districts_and_judges() %>%
      filter(defendant_started_in_district >= input$top_districts_opco_dates[1] & defendant_started_in_district <= input$top_districts_opco_dates[2], case_type == "Operating Company") %>%
      group_by(court_common) %>%
      summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
      top_n(5) %>%
      select(court_common, defendant_count) %>%
      arrange(defendant_count)
    top_districts_opco
    
  })
  
  output$top_districts_opco_chart <- renderPlot({
    
    #Top Districts (Operating Company) Chart
    top_districts_opco_chart <- ggplot(top_districts_opco_filtered(), aes(reorder(court_common, defendant_count), defendant_count)) +
      geom_bar(stat = "identity", fill = rgb(0,119,191,maxColorValue = 255)) +
      coord_flip() +
      geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), hjust = -.5), show.legend = FALSE) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        rect = element_blank()
      )
    top_districts_opco_chart
    
  })
  
  
  top_judges_overall_filtered <- reactive({
    
    # Top Judges (Overall)
    top_judges_overall <- top_districts_and_judges() %>%
      filter(defendant_started_in_district >= input$top_judges_overall_dates[1] & defendant_started_in_district <= input$top_judges_overall_dates[2], case_type != "PDPEF") %>%
      group_by(judge) %>%
      summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
      top_n(5) %>%
      select(judge, defendant_count) %>%
      arrange(defendant_count)
    top_judges_overall
    
  })
  
  output$top_judges_overall_chart <- renderPlot({
    
    #Top Judges (Overall) Chart
    top_judges_overall_chart <- ggplot(top_judges_overall_filtered(), aes(reorder(judge, defendant_count), defendant_count)) +
      geom_bar(stat = "identity", fill = rgb(81,81,81,maxColorValue = 255)) +
      coord_flip() +
      geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), hjust = -.5), show.legend = FALSE) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        rect = element_blank()
      )
    top_judges_overall_chart
    
  })
  
  top_judges_npe_filtered <- reactive({
    
    # Top Judges (NPE)
    top_judges_npe <- top_districts_and_judges() %>%
      filter(defendant_started_in_district >= input$top_judges_npe_dates[1] & defendant_started_in_district <= input$top_judges_npe_dates[2], case_type == "NPE") %>%
      group_by(judge) %>%
      summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
      top_n(5) %>%
      select(judge, defendant_count) %>%
      arrange(defendant_count)
    top_judges_npe
    
  })
  
  output$top_judges_npe_chart <- renderPlot({
    
    #Top Judges (NPE) Chart
    top_judges_npe_chart <- ggplot(top_judges_npe_filtered(), aes(reorder(judge, defendant_count), defendant_count)) +
      geom_bar(stat = "identity", fill = rgb(246,138,29,maxColorValue = 255)) +
      coord_flip() +
      geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), hjust = -.5), show.legend = FALSE) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        rect = element_blank()
      )
    top_judges_npe_chart
    
  })
  
  top_judges_opco_filtered <- reactive({
    
    # Top Judges (Operating Company)
    top_judges_opco <- top_districts_and_judges() %>%
      filter(defendant_started_in_district >= input$top_judges_opco_dates[1] & defendant_started_in_district <= input$top_judges_opco_dates[2], case_type == "Operating Company") %>%
      group_by(judge) %>%
      summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
      top_n(5) %>%
      select(judge, defendant_count) %>%
      arrange(defendant_count)
    top_judges_opco
    
  })
  
  output$top_judges_opco_chart <- renderPlot({
    
    #Top Judges (Operating Company) Chart
    top_judges_opco_chart <- ggplot(top_judges_opco_filtered(), aes(reorder(judge, defendant_count), defendant_count)) +
      geom_bar(stat = "identity", fill = rgb(0,119,191,maxColorValue = 255)) +
      coord_flip() +
      geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), hjust = -.5), show.legend = FALSE) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        rect = element_blank()
      )
    top_judges_opco_chart
    
  })
  
  
  # PTAB Section ------------------------------------------------------------
  
  ptab_by_quarter_filtered <- reactive({
    # PTAB Petitions Filed by Quarter
    ptab_by_quarter <- ptab_petitions() %>%
      filter(ptab_filing_date >= input$ptab_by_quarter_dates[1] & ptab_filing_date <= input$ptab_by_quarter_dates[2]) %>%
      mutate(ptab_case_type = factor(ptab_case_type, levels=c("PGR","CBM","IPR"))) %>%
      group_by(quarter_filed_ggplot, ptab_case_type) %>%
      summarize(petition_count = n_distinct(case_number)) %>%
      ungroup()
    ptab_by_quarter
    
  })
  
  output$ptab_by_quarter_chart <- renderPlot({
    
    # PTAB Petitions Filed by Quarter Chart
    ptab_by_quarter_chart <-ggplot(ptab_by_quarter_filtered(), aes(quarter_filed_ggplot, petition_count, fill = ptab_case_type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = format(as.numeric(petition_count), nsmall = 0, big.mark = ","), color = ptab_case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      coord_cartesian(clip = "off") +
      scale_color_manual(values = c("IPR" = "white", "CBM" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "PGR" = rgb(0,0,0, alpha = 0, maxColorValue = 255))) +
      scale_fill_manual(values = c(rgb(127,127,127,maxColorValue = 255),rgb(246,138,29,maxColorValue = 255), rgb(0,119,191,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = quarter_filed_ggplot), stat = 'summary', fun = sum, vjust = -1, size = 4) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank()
      )
    ptab_by_quarter_chart
    
  })
  
  
  ptab_by_year_filtered <- reactive({
    
    # PTAB Petitions Filed by Year
    ptab_by_year <- ptab_petitions() %>%
      filter(ptab_filing_date >= input$ptab_by_year_dates[1] & ptab_filing_date <= input$ptab_by_year_dates[2]) %>%
      mutate(ptab_case_type = factor(ptab_case_type, levels=c("PGR","CBM","IPR"))) %>%
      group_by(year, ptab_case_type) %>%
      summarize(petition_count = n_distinct(case_number)) %>%
      ungroup()
    ptab_by_year
    
  })
  
  output$ptab_by_year_chart <- renderPlot({
    
    # PTAB Petitions Filed by Year Chart
    ptab_by_year_chart <-ggplot(ptab_by_year_filtered(), aes(year, petition_count, fill = ptab_case_type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(rgb(127,127,127,maxColorValue = 255),rgb(246,138,29,maxColorValue = 255), rgb(0,119,191,maxColorValue = 255))) +
      geom_text(aes(label = format(as.numeric(petition_count), nsmall = 0, big.mark = ","), color = ptab_case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      coord_cartesian(clip = "off") +
      scale_color_manual(values = c("IPR" = "white", "CBM" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "PGR" = rgb(0,0,0, alpha = 0, maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = year), stat = 'summary', fun = sum, vjust = -1,  size = 4) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank()
      )
    ptab_by_year_chart
    
  })
  
  institutions_by_quarter_filtered <- reactive({
    
    # IPR Institutions by Quarter
    institutions_by_quarter <- ptab_petitions() %>%
      filter(institution_decision_date >= input$institution_by_quarter_dates[1] & institution_decision_date <= input$institution_by_quarter_dates[2] & ptab_case_type == 'IPR') %>%
      group_by(quarter_instituted_ggplot) %>%
      mutate(total_petitions = n_distinct(case_number)) %>%
      ungroup() %>%
      group_by(quarter_instituted_ggplot, institution_decision_outcome) %>%
      summarize(petition_count = n_distinct(case_number),
                institution_percentage =  scales::percent(n_distinct(case_number) / total_petitions, accuracy = 1)) %>%
      ungroup() %>%
      distinct(quarter_instituted_ggplot, institution_decision_outcome, petition_count, institution_percentage)
    institutions_by_quarter
    
    
  })
  
  output$institutions_by_quarter_chart <- renderPlot({
    
    # IPR Institutions by Quarter Chart
    institutions_by_quarter_chart <-ggplot(institutions_by_quarter_filtered(), aes(quarter_instituted_ggplot, petition_count, fill = institution_decision_outcome)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = institution_percentage, color = institution_decision_outcome), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      coord_cartesian(clip = "off") +
      scale_color_manual(values = c("Partial Institution" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "No Claims Instituted" = "white", "All Claims Instituted" = "white")) +
      scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255), rgb(186,188,192,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = quarter_instituted_ggplot), stat = 'summary', fun = sum, vjust = -1, size = 4) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank()
      )
    institutions_by_quarter_chart
    
  })
  
  institutions_by_year_filtered <- reactive({
    
    # IPR Institutions by Year
    institutions_by_year <- ptab_petitions() %>%
      filter(institution_decision_date >= input$institution_by_year_dates[1] & institution_decision_date <= input$institution_by_year_dates[2] & ptab_case_type == 'IPR') %>%
      group_by(institution_year) %>%
      mutate(total_petitions = n_distinct(case_number)) %>%
      ungroup() %>%
      group_by(institution_year, institution_decision_outcome) %>%
      summarize(petition_count = n_distinct(case_number),
                institution_percentage =  scales::percent(n_distinct(case_number) / total_petitions, accuracy = 1)) %>%
      ungroup() %>%
      distinct(institution_year, institution_decision_outcome, petition_count, institution_percentage)
    institutions_by_year
    
  })
  
  output$institutions_by_year_chart <- renderPlot({
    
    # IPR Institutions by Quarter Chart
    institutions_by_year_chart <-ggplot(institutions_by_year_filtered(), aes(institution_year, petition_count, fill = institution_decision_outcome)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = institution_percentage, color = institution_decision_outcome), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      coord_cartesian(clip = "off") +
      scale_color_manual(values = c("Partial Institution" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "No Claims Instituted" = "white", "All Claims Instituted" = "white")) +
      scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255), rgb(186,188,192,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(aes(label = format(as.numeric(stat(y)), nsmall = 0, big.mark = ","), group = institution_year), stat = 'summary', fun = sum, vjust = -1, size = 4) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank()
      )
    institutions_by_year_chart
    
  })
  
  
  # Alice Section ------------------------------------------------------------
  
  
  output$patents_prepost_berkheimer_chart <- renderPlot({
    
    # Patents with Alice Orders Before and After Berkheimer
    patents_prepost_berkheimer <- alice_patents() %>%
      group_by(prepost) %>%
      mutate(prepost_total = n_distinct(patent)) %>%
      ungroup() %>%
      group_by(prepost, rpx_outcome) %>%
      summarize(rpx_outcome_total = n_distinct(patent),
                rpx_outcome_percentage =  scales::percent(n_distinct(patent) / prepost_total, accuracy = 1)) %>%
      ungroup() %>%
      distinct_all()
    patents_prepost_berkheimer
    
    # Patents with Alice Orders Before and After Berkheimer Chart
    patents_prepost_berkheimer_chart <-ggplot(patents_prepost_berkheimer, aes(prepost, rpx_outcome_total, fill = rpx_outcome)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(label = rpx_outcome_percentage, color = rpx_outcome), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      scale_color_manual(values = c("Mixed by Claim" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "Not Invalid" = "white", "Invalid" = "white")) +
      scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(127,127,127,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.border = element_blank()
      )
    patents_prepost_berkheimer_chart
    
  })
  
  
  output$patents_prepost_procedural_berkheimer_chart <- renderPlot({  
    
    # Patents with Alice Orders Addressing Berkheimer by Procedural Stage
    patents_prepost_procedural_berkheimer <- alice_patents() %>%
      filter(case_stage != 'Other') %>%
      group_by(prepost, case_stage) %>%
      mutate(prepost_case_tage_total = n_distinct(patent)) %>%
      ungroup() %>%
      group_by(case_stage, prepost, rpx_outcome) %>%
      summarize(rpx_outcome_total = n_distinct(patent),
                rpx_outcome_percentage =  scales::percent(n_distinct(patent) / prepost_case_tage_total, accuracy = 1)) %>%
      ungroup() %>%
      distinct_all() %>%
      arrange(case_stage, desc(prepost))
    patents_prepost_procedural_berkheimer
    
    # Patents with Alice Orders Addressing Berkheimer by Procedural Stage Chart
    patents_prepost_procedural_berkheimer_chart <-ggplot(patents_prepost_procedural_berkheimer, aes(prepost, rpx_outcome_total, fill = rpx_outcome)) +
      geom_bar(stat = "identity") +
      facet_wrap("case_stage", nrow = 2, ncol = 1) +
      coord_flip() +
      geom_text(aes(label = rpx_outcome_percentage, color = rpx_outcome), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      scale_color_manual(values = c("Mixed by Claim" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "Not Invalid" = "white", "Invalid" = "white")) +
      scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(127,127,127,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.border = element_blank()
      )
    patents_prepost_procedural_berkheimer_chart
    
  })
  
  
  output$patents_procedural_stage_chart <- renderPlot({
    
    # Patents with Alice Orders by Procedural Stage 
    patents_procedural_stage <- alice_patents() %>%
      filter(case_stage != 'Other' & !is.na(berkheimer_aatrix) & sufficient_facts != 'N/A') %>%
      mutate(case_stage = factor(case_stage, levels=c("Summary Judgment","Rule 12"))) %>%
      mutate(sufficient_facts = factor(sufficient_facts, levels=c("Sufficient Facts for Early Resolution","Early Resolution Premature"))) %>%
      group_by(case_stage) %>%
      mutate(case_stage_total = n_distinct(patent)) %>%
      ungroup() %>%
      group_by(case_stage,sufficient_facts) %>%
      summarize(sufficient_facts_total = n_distinct(patent),
                sufficient_facts_percentage = scales::percent(n_distinct(patent) / case_stage_total, accuracy = 1)) %>%
      ungroup() %>%
      distinct_all() 
    patents_procedural_stage
    
    # Patents with Alice Orders by Procedural Stage
    patents_procedural_stage_chart <-ggplot(patents_procedural_stage, aes(case_stage, sufficient_facts_total, fill = sufficient_facts)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(label = sufficient_facts_percentage, color = sufficient_facts), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      scale_color_manual(values = c("Early Resolution Premature" = "white", "Sufficient Facts for Early Resolution" = "white")) +
      scale_fill_manual(values = c(rgb(154,72,138,maxColorValue = 255), rgb(127,127,127,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.border = element_blank()
      )
    patents_procedural_stage_chart
    
  })
  
  
  output$patents_casetype_outcome_chart <- renderPlot({
    
    # Patents with Alice Orders by Case Type
    patents_casetype_outcome <- alice_patents() %>%
      filter(case_type != 'N/A') %>%
      mutate(case_type = factor(case_type, levels=c("Operating Company","NPE"))) %>%
      group_by(case_type) %>%
      mutate(case_type_total = n_distinct(patent)) %>%
      ungroup() %>%
      group_by(case_type, rpx_outcome) %>%
      summarize(rpx_outcome_total = n_distinct(patent),
                rpx_outcome_percentage =  scales::percent(n_distinct(patent) / case_type_total, accuracy = 1)) %>%
      ungroup() %>%
      distinct_all()
    patents_casetype_outcome
    
    # Patents with Alice Orders by Case Type Chart
    patents_casetype_outcome_chart <-ggplot(patents_casetype_outcome, aes(case_type, rpx_outcome_total, fill = rpx_outcome)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(label = rpx_outcome_percentage, color = rpx_outcome), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      scale_color_manual(values = c("Mixed by Claim" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "Not Invalid" = "white", "Invalid" = "white")) +
      scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(127,127,127,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.border = element_blank()
      )
    patents_casetype_outcome_chart
    
  })
  
  
  output$patents_case_stage_and_plaintiff_type_chart <- renderPlot({
    
    # Patents with Alice Orders Addressing Berkheimer by Procedural Stage and Plaintiff Type
    patents_case_stage_and_plaintiff_type <- alice_patents() %>%
      filter(case_stage != 'Other' & !is.na(berkheimer_aatrix) & sufficient_facts != 'N/A') %>%
      mutate(case_stage = factor(case_stage, levels=c("Summary Judgment","Rule 12"))) %>%
      mutate(sufficient_facts = factor(sufficient_facts, levels=c("Sufficient Facts for Early Resolution","Early Resolution Premature"))) %>%
      group_by(case_type, case_stage) %>%
      mutate(case_stage_total = n_distinct(patent)) %>%
      ungroup() %>%
      group_by(case_type,case_stage, sufficient_facts) %>%
      summarize(sufficient_facts_total = n_distinct(patent),
                sufficient_facts_percentage = scales::percent(n_distinct(patent) / case_stage_total, accuracy = 1)) %>%
      ungroup() %>%
      distinct_all()
    patents_case_stage_and_plaintiff_type
    
    # Patents with Alice Orders Addressing Berkheimer by Procedural Stage and Plaintiff Type Chart
    patents_case_stage_and_plaintiff_type_chart <-ggplot(patents_case_stage_and_plaintiff_type, aes(case_stage, sufficient_facts_total, fill = sufficient_facts)) +
      geom_bar(stat = "identity") +
      facet_wrap("case_type", nrow = 2, ncol = 1) +
      coord_flip() +
      geom_text(aes(label = sufficient_facts_percentage, color = sufficient_facts), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      scale_color_manual(values = c("Sufficient Facts for Early Resolution" = "white", "Early Resolution Premature" = "white")) +
      scale_fill_manual(values = c(rgb(154,72,138,maxColorValue = 255), rgb(127,127,127,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.border = element_blank()
      )
    patents_case_stage_and_plaintiff_type_chart
    
  })
  
  
  output$patents_market_sector_chart <- renderPlot({
    
    # Patents with Alice Orders by Market Sector
    patents_market_sector <- alice_patents() %>%
      mutate(market_sector = factor(market_sector, levels=c("Semiconductors","Other Sectors","Networking","Mobile Communications and Devices",
                                                            "Medical","Media Content and Distribution","Industrial","Financial Services",
                                                            "E-Commerce and Software","Consumer Products","Consumer Electronics and PCs",
                                                            "Biotech and Pharma","Automotive"))) %>%
      filter(market_sector != 'N/A') %>%
      group_by(market_sector) %>%
      mutate(market_sector_total = n_distinct(patent)) %>%
      ungroup() %>%
      group_by(market_sector, rpx_outcome) %>%
      summarize(rpx_outcome_total = n_distinct(patent),
                rpx_outcome_percentage = scales::percent(n_distinct(patent) / market_sector_total, accuracy = 1),
                rpx_outcome_decimal = round(n_distinct(patent) / market_sector_total,2)) %>%
      ungroup() %>%
      distinct_all() %>%
      arrange(desc(market_sector))
    patents_market_sector
    
    # Patents with Alice Orders by Market Sector Chart
    patents_market_sector_chart <-ggplot(patents_market_sector, aes(market_sector, rpx_outcome_decimal, fill = rpx_outcome)) +
      geom_bar(position = "fill", stat = "identity") +
      geom_text(aes(label = rpx_outcome_percentage, color = rpx_outcome), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      coord_flip() +
      scale_color_manual(values = c("Mixed by Claim" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "Not Invalid" = "white", "Invalid" = "white")) +
      scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(127,127,127,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank()
      )
    patents_market_sector_chart
    
  })
  
  
  output$patents_districts_chart <- renderPlot({
    
    # Patents with Alice Orders by Top Districts
    top_patents_district <- alice_patents() %>%
      filter(court_common != 'N/A') %>%
      group_by(court_common) %>%
      mutate(court_common_total = n_distinct(patent)) %>%
      ungroup() %>%
      distinct(court_common, court_common_total) %>%
      arrange(desc(court_common_total)) %>%
      top_n(5, court_common_total) 
    top_patents_district
    
    patents_district <- alice_patents() %>%
      filter(court_common %in% top_patents_district$court_common) %>%
      group_by(court_common) %>%
      mutate(court_common_total = n_distinct(patent)) %>%
      ungroup() %>%
      group_by(court_common, rpx_outcome) %>%
      mutate(rpx_outcome_total = n_distinct(patent),
             rpx_outcome_percentage =  scales::percent(n_distinct(patent) / court_common_total, accuracy = 1)) %>%
      ungroup() %>%
      distinct(court_common, court_common_total, rpx_outcome, rpx_outcome_total, rpx_outcome_percentage) %>%
      arrange(desc(court_common_total)) 
    patents_district
    
    patents_district$court_common = with(patents_district, reorder(court_common, court_common_total))
    
    # Patents with Alice Orders by Top Districts Chart
    patents_districts_chart <-ggplot(patents_district, aes(reorder(court_common, court_common_total), rpx_outcome_total, fill = rpx_outcome)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(label = rpx_outcome_percentage, color = rpx_outcome), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      scale_color_manual(values = c("Mixed by Claim" = rgb(0,0,0, alpha = 0, maxColorValue = 255), "Not Invalid" = "white", "Invalid" = "white")) +
      scale_fill_manual(values = c(rgb(0,119,191,maxColorValue = 255), rgb(127,127,127,maxColorValue = 255), rgb(246,138,29,maxColorValue = 255))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
        rect = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.border = element_blank()
      )
    patents_districts_chart
    
  })
  
  
  # Excel Exports ------------------------------------------------------------    
  output$custom_export <- downloadHandler(
    
    filename = function() {paste0("Reporting Dashboard - Custom Export ", " (", format(Sys.Date(), format = "%m-%d-%Y"), ").xlsx")},
    content = function(file) {
      
      load_template <- loadWorkbook(file = "ReportingDashboardExport.xlsx")
      custom_export <- copyWorkbook(load_template)
      
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
      
      x_axis_format <-  createStyle(
        fontName = "Arial",
        fontSize = 11,
        fontColour = rgb(0,0,0, maxColorValue = 255),
        numFmt = "GENERAL",
        halign = "center",
        valign = "center"
      )
      
      # District Court Defendants by Quarter
      excel_defs_by_quarter <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_quarter_dates[1] & defendant_started <= input$defs_by_quarter_dates[2]) %>%
        group_by(quarter_started) %>%
        summarize(
          npe = sum(case_type == "NPE"),
          opco = sum(case_type == "Operating Company"),
          pdpef = sum(case_type == "PDPEF"),
          total = n(),
          total_no_pdpef = sum(case_type != "PDPEF")
        )
      
      # District Court Defendants by Year
      excel_defs_by_year <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_year_dates[1] & defendant_started <= input$defs_by_year_dates[2]) %>%
        group_by(year) %>%
        summarize(
          npe = sum(case_type == "NPE"),
          opco = sum(case_type == "Operating Company"),
          pdpef = sum(case_type == "PDPEF"),
          total = n(),
          total_no_pdpef = sum(case_type != "PDPEF")
        )
      
      # Top Districts (Overall)
      excel_top_districts_overall <- dc_defendants() %>%
        filter(defendant_started >= input$top_districts_overall_dates[1] & defendant_started <= input$top_districts_overall_dates[2], case_type != "PDPEF") %>%
        group_by(court_abbrev) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(court_abbrev, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))
      
      # Top Districts (NPE)
      excel_top_districts_npe <- dc_defendants() %>%
        filter(defendant_started >= input$top_districts_npe_dates[1] & defendant_started <= input$top_districts_npe_dates[2], case_type == "NPE") %>%
        group_by(court_abbrev) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(court_abbrev, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))
      
      # Top Districts (Operating Company)
      excel_top_districts_opco <- dc_defendants() %>%
        filter(defendant_started >= input$top_districts_opco_dates[1] & defendant_started <= input$top_districts_opco_dates[2], case_type == "Operating Company") %>%
        group_by(court_abbrev) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(court_abbrev, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))
      
      # Top Judges (Overall)
      excel_top_judges_overall <- top_districts_and_judges() %>%
        filter(defendant_started >= input$top_judges_overall_dates[1] & defendant_started <= input$top_judges_overall_dates[2], case_type != "PDPEF") %>%
        group_by(judge) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(judge, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))
      
      # Top Judges (NPE)
      excel_top_judges_npe <- top_districts_and_judges() %>%
        filter(defendant_started >= input$top_judges_npe_dates[1] & defendant_started <= input$top_judges_npe_dates[2], case_type == "NPE") %>%
        group_by(judge) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(judge, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))
      
      # Top Judges (Operating Company)
      excel_top_judges_opco <- top_districts_and_judges() %>%
        filter(defendant_started >= input$top_judges_opco_dates[1] & defendant_started <= input$top_judges_opco_dates[2], case_type == "Operating Company") %>%
        group_by(judge) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(judge, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))
      
      # PTAB filings by Quarter
      excel_ptab_by_quarter <- ptab_petitions() %>%
        filter(ptab_filing_date >= input$ptab_by_quarter_dates[1] & ptab_filing_date <= input$ptab_by_quarter_dates[2]) %>%
        group_by(quarter_filed) %>%
        summarize(
          npe = sum(ptab_case_type == "IPR"),
          opco = sum(ptab_case_type == "CBM"),
          pdpef = sum(ptab_case_type == "PGR"),
          total = n()
        )
      
      # PTAB filings by Year
      excel_ptab_by_year <- ptab_petitions() %>%
        filter(ptab_filing_date >= input$ptab_by_year_dates[1] & ptab_filing_date <= input$ptab_by_year_dates[2]) %>%
        group_by(year) %>%
        summarize(
          npe = sum(ptab_case_type == "IPR"),
          opco = sum(ptab_case_type == "CBM"),
          pdpef = sum(ptab_case_type == "PGR"),
          total = n()
        )
      
      # IPR Institution by Quarter
      excel_institutions_by_quarter <- ptab_petitions() %>%
        filter(institution_decision_date >= input$institution_by_quarter_dates[1] & institution_decision_date <= input$institution_by_quarter_dates[2]) %>%
        group_by(quarter_instituted) %>%
        summarize(
          partial = sum(institution_decision_outcome == "Partial Institution"),
          no_claims = sum(institution_decision_outcome == "No Claims Instituted"),
          all_claims = sum(institution_decision_outcome == "All Claims Instituted"),
          total = n()
        ) %>%
        mutate(percent_partial = round(partial / total,2),
               percent_no_claims = round(no_claims / total,2),
               percent_all_claims = round(all_claims / total,2))
      excel_institutions_by_quarter
      
      # IPR Institutions by Year
      excel_institutions_by_year <- ptab_petitions() %>%
        filter(institution_decision_date >= input$institution_by_year_dates[1] & institution_decision_date <= input$institution_by_year_dates[2]) %>%
        group_by(institution_year) %>%
        summarize(
          partial = sum(institution_decision_outcome == "Partial Institution"),
          no_claims = sum(institution_decision_outcome == "No Claims Instituted"),
          all_claims = sum(institution_decision_outcome == "All Claims Instituted"),
          total = n()
        ) %>%
        mutate(percent_partial = round(partial / total,2),
               percent_no_claims = round(no_claims / total,2),
               percent_all_claims = round(all_claims / total,2))
      excel_institutions_by_year
      
      #### DISTRICT COURT ####
      # Defendants Added by Quarter
      writeData(custom_export, sheet = "Defs Added by Quarter", x = excel_defs_by_quarter, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Defs Added by Quarter", x = paste0(format(input$defs_by_quarter_dates[1], format = "%m/%d/%Y"), " - ", format(input$defs_by_quarter_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Defs Added by Quarter", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Defs Added by Quarter", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Defs Added by Quarter", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      
      # Defendants Added by Year
      writeData(custom_export, sheet = "Defs Added by Year", x = excel_defs_by_year, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Defs Added by Year", x = paste0(format(input$defs_by_year_dates[1], format = "%m/%d/%Y"), " - ", format(input$defs_by_year_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Defs Added by Year", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Defs Added by Year", style = x_axis_format, rows = 8:20, cols = 1:20, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Defs Added by Year", style = data_label_format, rows = 8:20, cols = 2:20, gridExpand = TRUE)
      
      # Top Districts (Overall)
      writeData(custom_export, sheet = "Top Districts (Overall)", x = excel_top_districts_overall, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Top Districts (Overall)", x = paste0(format(input$top_districts_overall_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_districts_overall_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Top Districts (Overall)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Top Districts (Overall)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Districts (Overall)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Districts (Overall)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      
      # Top Districts (NPE)
      writeData(custom_export, sheet = "Top Districts (NPE)", x = excel_top_districts_npe, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Top Districts (NPE)", x = paste0(format(input$top_districts_npe_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_districts_npe_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Top Districts (NPE)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Top Districts (NPE)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Districts (NPE)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Districts (NPE)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      
      # Top Districts (Operating Company)
      writeData(custom_export, sheet = "Top Districts (OpCo)", x = excel_top_districts_opco, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Top Districts (OpCo)", x = paste0(format(input$top_districts_opco_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_districts_opco_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Top Districts (OpCo)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Top Districts (OpCo)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Districts (OpCo)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Districts (OpCo)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      
      # Top Judges (Overall)
      writeData(custom_export, sheet = "Top Judges (Overall)", x = excel_top_judges_overall, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Top Judges (Overall)", x = paste0(format(input$top_judges_overall_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_judges_overall_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Top Judges (Overall)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Top Judges (Overall)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Judges (Overall)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Judges (Overall)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      
      # Top Judges (NPE)
      writeData(custom_export, sheet = "Top Judges (NPE)", x = excel_top_judges_npe, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Top Judges (NPE)", x = paste0(format(input$top_judges_npe_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_judges_npe_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Top Judges (NPE)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Top Judges (NPE)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Judges (NPE)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Judges (NPE)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      
      # Top Judges (Operating Company)
      writeData(custom_export, sheet = "Top Judges (OpCo)", x = excel_top_judges_opco, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "Top Judges (OpCo)", x = paste0(format(input$top_judges_opco_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_judges_opco_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "Top Judges (OpCo)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "Top Judges (OpCo)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Judges (OpCo)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "Top Judges (OpCo)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      
      #### PTAB ####
      # Petitions Filed by Quarter
      writeData(custom_export, sheet = "PTAB Filings by Quarter", x = excel_ptab_by_quarter, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "PTAB Filings by Quarter", x = paste0(format(input$ptab_by_quarter_dates[1], format = "%m/%d/%Y"), " - ", format(input$ptab_by_quarter_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "PTAB Filings by Quarter", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "PTAB Filings by Quarter", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "PTAB Filings by Quarter", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      
      # Petitions Filed by Year
      writeData(custom_export, sheet = "PTAB Filings by Year", x = excel_ptab_by_year, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "PTAB Filings by Year", x = paste0(format(input$ptab_by_year_dates[1], format = "%m/%d/%Y"), " - ", format(input$ptab_by_year_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "PTAB Filings by Year", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "PTAB Filings by Year", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "PTAB Filings by Year", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      
      # IPR Institution Decisions by Quarter
      writeData(custom_export, sheet = "IPR Institutions by Quarter", x = excel_institutions_by_quarter, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "IPR Institutions by Quarter", x = paste0(format(input$institution_by_quarter_dates[1], format = "%m/%d/%Y"), " - ", format(input$institution_by_quarter_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "IPR Institutions by Quarter", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "IPR Institutions by Quarter", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "IPR Institutions by Quarter", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "IPR Institutions by Quarter", style = percentage_format, rows = 8:100, cols = 6:100, gridExpand = TRUE)
      
      # IPR Institution Decisions by Quarter
      writeData(custom_export, sheet = "IPR Institutions by Year", x = excel_institutions_by_year, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(custom_export, sheet = "IPR Institutions by Year", x = paste0(format(input$institution_by_year_dates[1], format = "%m/%d/%Y"), " - ", format(input$institution_by_year_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(custom_export, sheet = "IPR Institutions by Year", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(custom_export, sheet = "IPR Institutions by Year", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "IPR Institutions by Year", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(custom_export, sheet = "IPR Institutions by Year", style = percentage_format, rows = 8:100, cols = 6:100, gridExpand = TRUE)
      
      saveWorkbook(custom_export, file, overwrite = TRUE)
      
    })
  
  output$alice_export <- downloadHandler(

    filename = function() {paste0("Reporting Dashboard - Alice Export ", " (", format(Sys.Date(), format = "%m-%d-%Y"), ").xlsx")},
    content = function(file) {

      load_template <- loadWorkbook(file = "AliceExport.xlsx")
      alice_export <- copyWorkbook(load_template)
      
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

      x_axis_format <-  createStyle(
        fontName = "Arial",
        fontSize = 11,
        fontColour = rgb(0,0,0, maxColorValue = 255),
        numFmt = "GENERAL",
        halign = "left",
        valign = "center"
      )

      # Patents Invalidated under Alice PrePost Berkheimer
      excel_patents_prepost_berkheimer <- alice_patents() %>%
        group_by(prepost) %>%
        summarize(
          not_invalid = n_distinct(subset(patent,rpx_outcome == "Not Invalid")),
          mixed_by_claim = n_distinct(subset(patent,rpx_outcome == "Mixed by Claim")),
          invalid = n_distinct(subset(patent,rpx_outcome == "Invalid")),
          total = n_distinct(patent)) %>%
        mutate(percent_not_invalid = round(not_invalid / total,2),
               percent_mixed_by_claim = round(mixed_by_claim / total,2),
               percent_invalid = round(invalid / total,2)) %>%
        ungroup() %>%
        distinct_all()
      excel_patents_prepost_berkheimer
      
      # Patents Invalidated Under Alice Before and After Berkheimer by Procedural Stage
      excel_patents_prepost_procedural_berkheimer <- alice_patents() %>%
        filter(case_stage != 'Other') %>%
        group_by(case_stage, prepost) %>%
        summarize(
          not_invalid = n_distinct(subset(patent,rpx_outcome == "Not Invalid")),
          mixed_by_claim = n_distinct(subset(patent,rpx_outcome == "Mixed by Claim")),
          invalid = n_distinct(subset(patent,rpx_outcome == "Invalid")),
          total = n_distinct(patent)) %>%
        mutate(percent_not_invalid = round(not_invalid / total,2),
               percent_mixed_by_claim = round(mixed_by_claim / total,2),
               percent_invalid = round(invalid / total,2)) %>%
        ungroup() %>%
        arrange(desc(case_stage), prepost)
      excel_patents_prepost_procedural_berkheimer

      # Patents with Alice Orders Addressing Berkheimer by Procedural Stage
      excel_patents_procedural_stage <- alice_patents() %>%
        filter(case_stage != 'Other' & !is.na(berkheimer_aatrix) & sufficient_facts != 'N/A') %>%
        group_by(case_stage) %>%
        summarize(early_resolution = n_distinct(subset(patent,sufficient_facts == "Early Resolution Premature")),
                  sufficient_facts = n_distinct(subset(patent,sufficient_facts == "Sufficient Facts for Early Resolution")),
                  total = n_distinct(patent)) %>%
        mutate(percent_early_resolution = round(early_resolution / total,2),
               percent_sufficient_facts = round(sufficient_facts / total,2)) %>%
        ungroup() %>%
        arrange(desc(case_stage))
      excel_patents_procedural_stage

      # Patents with Alice Orders Addressing Berkheimer by Procedural Stage and Plaintiff Type
      excel_patents_case_stage_and_plaintiff_type <- alice_patents() %>%
        filter(case_stage != 'Other' & !is.na(berkheimer_aatrix) & sufficient_facts != 'N/A') %>%
        mutate(sufficient_facts = factor(sufficient_facts, levels=c("Sufficient Facts for Early Resolution","Early Resolution Premature"))) %>%
        group_by(case_type, case_stage) %>%
        summarize(early_resolution = n_distinct(subset(patent,sufficient_facts == "Early Resolution Premature")),
                  sufficient_facts = n_distinct(subset(patent,sufficient_facts == "Sufficient Facts for Early Resolution")),
                  total = n_distinct(patent)) %>%
        mutate(percent_early_resolution = round(early_resolution / total,2),
               percent_sufficient_facts = round(sufficient_facts / total,2)) %>%
        ungroup() %>%
        arrange(desc(case_type), desc(case_stage))
      excel_patents_case_stage_and_plaintiff_type

      # Patents Invalidated Under Alice Since the Decision’s Issuance
      excel_patents_casetype_outcome <- alice_patents() %>%
        filter(case_type != 'N/A') %>%
        group_by(case_type) %>%
        summarize(
          not_invalid = n_distinct(subset(patent,rpx_outcome == "Not Invalid")),
          mixed_by_claim = n_distinct(subset(patent,rpx_outcome == "Mixed by Claim")),
          invalid = n_distinct(subset(patent,rpx_outcome == "Invalid")),
          total = n_distinct(patent)) %>%
        mutate(percent_not_invalid = round(not_invalid / total,2),
               percent_mixed_by_claim = round(mixed_by_claim / total,2),
               percent_invalid = round(invalid / total,2)) %>%
        ungroup() %>%
        arrange(desc(case_type))
      excel_patents_casetype_outcome

      # Alice Invalidation Rates by Market Sector
      excel_patents_market_sector <- alice_patents() %>%
        filter(market_sector != 'N/A') %>%
        group_by(market_sector) %>%
        summarize(
          not_invalid = n_distinct(subset(patent,rpx_outcome == "Not Invalid")),
          mixed_by_claim = n_distinct(subset(patent,rpx_outcome == "Mixed by Claim")),
          invalid = n_distinct(subset(patent,rpx_outcome == "Invalid")),
          total = n_distinct(patent)) %>%
        mutate(percent_not_invalid = round(not_invalid / total,2),
               percent_mixed_by_claim = round(mixed_by_claim / total,2),
               percent_invalid = round(invalid / total,2)) %>%
        ungroup() %>%
        arrange(market_sector)
      excel_patents_market_sector

      # Alice Invalidation Rates by District
      excel_patents_district <- alice_patents() %>%
        filter(court_common != 'N/A') %>%
        group_by(court_common) %>%
        summarize(
          not_invalid = n_distinct(subset(patent,rpx_outcome == "Not Invalid")),
          mixed_by_claim = n_distinct(subset(patent,rpx_outcome == "Mixed by Claim")),
          invalid = n_distinct(subset(patent,rpx_outcome == "Invalid")),
          total = n_distinct(patent)) %>%
        mutate(percent_not_invalid = round(not_invalid / total,2),
               percent_mixed_by_claim = round(mixed_by_claim / total,2),
               percent_invalid = round(invalid / total,2)) %>%
        ungroup() %>%
        arrange(desc(total))
      excel_patents_district

      # Alice Invalidation Rates by District (National)
      excel_patents_district_national <- alice_patents() %>%
        filter(court_common != 'N/A') %>%
        summarize(
          national = "National",
          not_invalid = n_distinct(subset(patent,rpx_outcome == "Not Invalid")),
          mixed_by_claim = n_distinct(subset(patent,rpx_outcome == "Mixed by Claim")),
          invalid = n_distinct(subset(patent,rpx_outcome == "Invalid")),
          total = n_distinct(patent)) %>%
        mutate(percent_not_invalid = round(not_invalid / total,2),
               percent_mixed_by_claim = round(mixed_by_claim / total,2),
               percent_invalid = round(invalid / total,2)) %>%
        ungroup() %>%
        arrange(desc(total))
      excel_patents_district_national

      max_alice_date <- max(as.Date(alice_patents()$decision_date))

      # Patents Invalidated Under Alice Pre/Post Berkheimer
      writeData(alice_export, sheet = "Patents PrePost Berkheimer", x = excel_patents_prepost_berkheimer, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(alice_export, sheet = "Patents PrePost Berkheimer", x = format(max_alice_date, format = "%m/%d/%Y"), startCol = 2, startRow = 2)
      writeData(alice_export, sheet = "Patents PrePost Berkheimer", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(alice_export, sheet = "Patents PrePost Berkheimer", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Patents PrePost Berkheimer", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Patents PrePost Berkheimer", style = percentage_format, rows = 8:100, cols = 6:100, gridExpand = TRUE)
      
      # Patents Invalidated Under Alice Before and After Berkheimer by Procedural Stage
      writeData(alice_export, sheet = "Alice by Berkheimer and Stage", x = excel_patents_prepost_procedural_berkheimer, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(alice_export, sheet = "Alice by Berkheimer and Stage", x = format(max_alice_date, format = "%m/%d/%Y"), startCol = 2, startRow = 2)
      writeData(alice_export, sheet = "Alice by Berkheimer and Stage", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(alice_export, sheet = "Alice by Berkheimer and Stage", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Berkheimer and Stage", style = data_label_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Berkheimer and Stage", style = percentage_format, rows = 8:100, cols = 7:100, gridExpand = TRUE)
      
      # Patents with Alice Orders Addressing Berkheimer by Procedural Stage
      writeData(alice_export, sheet = "Alice by Case Stage", x = excel_patents_procedural_stage, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(alice_export, sheet = "Alice by Case Stage", x = format(max_alice_date, format = "%m/%d/%Y"), startCol = 2, startRow = 2)
      writeData(alice_export, sheet = "Alice by Case Stage", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(alice_export, sheet = "Alice by Case Stage", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Case Stage", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Case Stage", style = percentage_format, rows = 8:100, cols = 5:100, gridExpand = TRUE)
    
      # Patents with Alice Orders Addressing Berkheimer by Procedural Stage and Plaintiff Type
      writeData(alice_export, sheet = "Alice by Case Type and Stage", x = excel_patents_case_stage_and_plaintiff_type, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(alice_export, sheet = "Alice by Case Type and Stage", x = format(max_alice_date, format = "%m/%d/%Y"), startCol = 2, startRow = 2)
      writeData(alice_export, sheet = "Alice by Case Type and Stage", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(alice_export, sheet = "Alice by Case Type and Stage", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Case Type and Stage", style = data_label_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Case Type and Stage", style = percentage_format, rows = 8:100, cols = 6:100, gridExpand = TRUE)
      
      # Patents Invalidated Under Alice Since the Decision’s Issuance
      writeData(alice_export, sheet = "Alice by Case Type", x = excel_patents_casetype_outcome, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(alice_export, sheet = "Alice by Case Type", x = format(max_alice_date, format = "%m/%d/%Y"), startCol = 2, startRow = 2)
      writeData(alice_export, sheet = "Alice by Case Type", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(alice_export, sheet = "Alice by Case Type", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Case Type", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Case Type", style = percentage_format, rows = 8:100, cols = 6:100, gridExpand = TRUE)
      
      # Alice Invalidation Rates by Market Sector
      writeData(alice_export, sheet = "Alice by Market Sector", x = excel_patents_market_sector, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(alice_export, sheet = "Alice by Market Sector", x = format(max_alice_date, format = "%m/%d/%Y"), startCol = 2, startRow = 2)
      writeData(alice_export, sheet = "Alice by Market Sector", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(alice_export, sheet = "Alice by Market Sector", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Market Sector", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by Market Sector", style = percentage_format, rows = 8:100, cols = 6:100, gridExpand = TRUE)
      
      # Alice Invalidation Rates by District
      writeData(alice_export, sheet = "Alice by District", x = excel_patents_district_national, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(alice_export, sheet = "Alice by District", x = excel_patents_district, startCol = 1, startRow = 12, colNames = FALSE)
      writeData(alice_export, sheet = "Alice by District", x = format(max_alice_date, format = "%m/%d/%Y"), startCol = 2, startRow = 2)
      writeData(alice_export, sheet = "Alice by District", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(alice_export, sheet = "Alice by District", style = x_axis_format, rows = 8:8, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by District", style = data_label_format, rows = 8:8, cols = 2:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by District", style = percentage_format, rows = 8:8, cols = 6:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by District", style = x_axis_format, rows = 12:100, cols = 1:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by District", style = data_label_format, rows = 12:100, cols = 2:100, gridExpand = TRUE)
      addStyle(alice_export, sheet = "Alice by District", style = percentage_format, rows = 12:100, cols = 6:100, gridExpand = TRUE)
      
      saveWorkbook(alice_export, file, overwrite = TRUE)
    })
  
  
}

shinyApp(ui,server)

