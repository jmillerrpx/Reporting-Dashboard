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
    
    # District Court Defendants by Quarter (With Design Patent Litigation)
    if(input$defs_by_quarter_design == TRUE) {
      
      defs_by_quarter <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_quarter_dates[1] & defendant_started <= input$defs_by_quarter_dates[2]) %>%
        mutate(case_type = factor(case_type, levels=c("Design Patent","Operating Company","NPE"))) %>%
        group_by(quarter_started_ggplot, case_type) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        ungroup()
      defs_by_quarter
      
    } else {
      
      # District Court Defendants by Quarter
      defs_by_quarter <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_quarter_dates[1] & defendant_started <= input$defs_by_quarter_dates[2], case_type != "Design Patent") %>%
        mutate(case_type = factor(case_type, levels=c("Design Patent","Operating Company","NPE"))) %>%
        group_by(quarter_started_ggplot, case_type) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        ungroup()
      defs_by_quarter
      
    }
    
  })
  
  output$defs_by_quarter_chart <- renderPlot({

    
    if(input$defs_by_quarter_design == TRUE) {
    
    # District Court Defendants by Quarter Chart (With Design Patent Litigation)
    defs_by_quarter_chart <-ggplot(defs_by_quarter_filtered(), aes(quarter_started_ggplot, defendant_count, fill = case_type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
      coord_cartesian(clip = "off") +
      scale_color_manual(values = c("Design Patent" = "black", "NPE" = "white", "Operating Company" = "white")) +
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
  
    if(input$defs_by_year_design == TRUE) {

        # District Court Defendants by Year (With Design Patent Litigation)
        defs_by_year <- dc_defendants() %>%
          filter(defendant_started >= input$defs_by_year_dates[1] & defendant_started <= input$defs_by_year_dates[2]) %>%
          mutate(case_type = factor(case_type, levels=c("Design Patent","Operating Company","NPE"))) %>%
          group_by(year, case_type) %>%
          summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
          ungroup()
        defs_by_year
  
    } else {
  
      # District Court Defendants by Year
      defs_by_year <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_year_dates[1] & defendant_started <= input$defs_by_year_dates[2], case_type != "Design Patent") %>%
        mutate(case_type = factor(case_type, levels=c("Design Patent","Operating Company","NPE"))) %>%
        group_by(year, case_type) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        ungroup()
      defs_by_year
  
    }
    
  })
  
  output$defs_by_year_chart <- renderPlot({
    
    if(input$defs_by_year_design == TRUE) {

  # District Court Defendants by Year Chart (With Design Patent Litigation)
      defs_by_year_chart <-ggplot(defs_by_year_filtered(), aes(year, defendant_count, fill = case_type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
        coord_cartesian(clip = "off") +
        scale_color_manual(values = c("Design Patent" = "black", "NPE" = "white", "Operating Company" = "white")) +
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
      filter(defendant_started_in_district >= input$top_districts_overall_dates[1] & defendant_started_in_district <= input$top_districts_overall_dates[2], case_type != "Design Patent") %>%
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
      filter(defendant_started_in_district >= input$top_judges_overall_dates[1] & defendant_started_in_district <= input$top_judges_overall_dates[2], case_type != "Design Patent") %>%
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
          design = sum(case_type == "Design Patent"),
          total = n(),
          total_no_design = sum(case_type != "Design Patent")
        )
      
      # District Court Defendants by Year
      excel_defs_by_year <- dc_defendants() %>%
        filter(defendant_started >= input$defs_by_year_dates[1] & defendant_started <= input$defs_by_year_dates[2]) %>%
        group_by(year) %>%
        summarize(
          npe = sum(case_type == "NPE"),
          opco = sum(case_type == "Operating Company"),
          design = sum(case_type == "Design Patent"),
          total = n(),
          total_no_design = sum(case_type != "Design Patent")
        )

      # Top Districts (Overall)
      excel_top_districts_overall <- dc_defendants() %>%
        filter(defendant_started >= input$top_districts_overall_dates[1] & defendant_started <= input$top_districts_overall_dates[2], case_type != "Design Patent") %>%
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
      
      # PTAB filings by Quarter
      excel_ptab_by_quarter <- ptab_petitions() %>%
        filter(ptab_filing_date >= input$ptab_by_quarter_dates[1] & ptab_filing_date <= input$ptab_by_quarter_dates[2]) %>%
        group_by(quarter_filed) %>%
        summarize(
          npe = sum(ptab_case_type == "IPR"),
          opco = sum(ptab_case_type == "CBM"),
          design = sum(ptab_case_type == "PGR"),
          total = n()
        )
      
      # PTAB filings by Year
      excel_ptab_by_year <- ptab_petitions() %>%
        filter(ptab_filing_date >= input$ptab_by_year_dates[1] & ptab_filing_date <= input$ptab_by_year_dates[2]) %>%
        group_by(year) %>%
        summarize(
          npe = sum(ptab_case_type == "IPR"),
          opco = sum(ptab_case_type == "CBM"),
          design = sum(ptab_case_type == "PGR"),
          total = n()
        )
      
      #### DISTRICT COURT ####
      # Defendants Added by Quarter
      if(input$defs_by_quarter_export == TRUE) {
        writeData(custom_export, sheet = "Defs Added by Quarter", x = excel_defs_by_quarter, startCol = 1, startRow = 8, colNames = FALSE)
        writeData(custom_export, sheet = "Defs Added by Quarter", x = paste0(format(input$defs_by_quarter_dates[1], format = "%m/%d/%Y"), " - ", format(input$defs_by_quarter_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
        writeData(custom_export, sheet = "Defs Added by Quarter", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
        addStyle(custom_export, sheet = "Defs Added by Quarter", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Defs Added by Quarter", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      } else {
        removeWorksheet(custom_export, sheet = "Defs Added by Quarter")
      }
      
      # Defendants Added by Year
      if(input$defs_by_year_export == TRUE) {
        writeData(custom_export, sheet = "Defs Added by Year", x = excel_defs_by_year, startCol = 1, startRow = 8, colNames = FALSE)
        writeData(custom_export, sheet = "Defs Added by Year", x = paste0(format(input$defs_by_year_dates[1], format = "%m/%d/%Y"), " - ", format(input$defs_by_year_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
        writeData(custom_export, sheet = "Defs Added by Year", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
        addStyle(custom_export, sheet = "Defs Added by Year", style = x_axis_format, rows = 8:20, cols = 1:20, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Defs Added by Year", style = data_label_format, rows = 8:20, cols = 2:20, gridExpand = TRUE)
      } else {
        removeWorksheet(custom_export, sheet = "Defs Added by Year")
      }
      
      # Top Districts (Overall)
      if(input$top_districts_overall_check == TRUE) {
        writeData(custom_export, sheet = "Top Districts (Overall)", x = excel_top_districts_overall, startCol = 1, startRow = 8, colNames = FALSE)
        writeData(custom_export, sheet = "Top Districts (Overall)", x = paste0(format(input$top_districts_overall_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_districts_overall_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
        writeData(custom_export, sheet = "Top Districts (Overall)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
        addStyle(custom_export, sheet = "Top Districts (Overall)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Top Districts (Overall)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Top Districts (Overall)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      } else {
        removeWorksheet(custom_export, sheet = "Top Districts (Overall)")
      }
      
      # Top Districts (NPE)
      if(input$top_districts_npe_check == TRUE) {
        writeData(custom_export, sheet = "Top Districts (NPE)", x = excel_top_districts_npe, startCol = 1, startRow = 8, colNames = FALSE)
        writeData(custom_export, sheet = "Top Districts (NPE)", x = paste0(format(input$top_districts_npe_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_districts_npe_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
        writeData(custom_export, sheet = "Top Districts (NPE)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
        addStyle(custom_export, sheet = "Top Districts (NPE)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Top Districts (NPE)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Top Districts (NPE)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      } else {
        removeWorksheet(custom_export, sheet = "Top Districts (NPE)")
      }
      
      # Top Districts (Operating Company)
      if(input$top_districts_opco_check == TRUE) {
        writeData(custom_export, sheet = "Top Districts (OpCo)", x = excel_top_districts_opco, startCol = 1, startRow = 8, colNames = FALSE)
        writeData(custom_export, sheet = "Top Districts (OpCo)", x = paste0(format(input$top_districts_opco_dates[1], format = "%m/%d/%Y"), " - ", format(input$top_districts_opco_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
        writeData(custom_export, sheet = "Top Districts (OpCo)", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
        addStyle(custom_export, sheet = "Top Districts (OpCo)", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Top Districts (OpCo)", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "Top Districts (OpCo)", style = percentage_format, rows = 8:100, cols = 3:100, gridExpand = TRUE)
      } else {
        removeWorksheet(custom_export, sheet = "Top Districts (OpCo)")
      }
      
      #### PTAB ####
      # Petitions Filed by Quarter
      if(input$ptab_by_quarter_check == TRUE) {
        writeData(custom_export, sheet = "PTAB Filings by Quarter", x = excel_ptab_by_quarter, startCol = 1, startRow = 8, colNames = FALSE)
        writeData(custom_export, sheet = "PTAB Filings by Quarter", x = paste0(format(input$ptab_by_quarter_dates[1], format = "%m/%d/%Y"), " - ", format(input$ptab_by_quarter_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
        writeData(custom_export, sheet = "PTAB Filings by Quarter", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
        addStyle(custom_export, sheet = "PTAB Filings by Quarter", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "PTAB Filings by Quarter", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      } else {
        removeWorksheet(custom_export, sheet = "PTAB Filings by Quarter")
      }
      
      # Petitions Filed by Year
      if(input$ptab_by_year_check == TRUE) {
        writeData(custom_export, sheet = "PTAB Filings by Year", x = excel_ptab_by_year, startCol = 1, startRow = 8, colNames = FALSE)
        writeData(custom_export, sheet = "PTAB Filings by Year", x = paste0(format(input$ptab_by_year_dates[1], format = "%m/%d/%Y"), " - ", format(input$ptab_by_year_dates[2], format = "%m/%d/%Y")), startCol = 2, startRow = 2)
        writeData(custom_export, sheet = "PTAB Filings by Year", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
        addStyle(custom_export, sheet = "PTAB Filings by Year", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
        addStyle(custom_export, sheet = "PTAB Filings by Year", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      } else {
        removeWorksheet(custom_export, sheet = "PTAB Filings by Year")
      }
      
      saveWorkbook(custom_export, file, overwrite = TRUE)

    })
  
  output$blog_post_export <- downloadHandler(
    
    filename = function() {paste0("Reporting Dashboard - Blog Post Export ", " (", format(Sys.Date(), format = "%m-%d-%Y"), ").xlsx")},
    content = function(file) {
      
      load_template <- loadWorkbook(file = "BlogPostExport.xlsx")
      blog_post_export <- copyWorkbook(load_template)
      
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
      
      last_quarter_start_date <- reactive({ 
        last_quarter <- today() - months(3)
        last_quarter_start <- floor_date(last_quarter, unit = "quarter")
        last_quarter_start
      })
      
      last_quarter_end_date <- reactive({ 
        last_quarter_end <- round_date(ceiling_date(last_quarter, unit = "quarter"))
        last_quarter_end
      })
      
      sixteen_quarters_ago_date <- reactive ({
        sixteen_quarters_ago <- today() - years(4)
        sixteen_quarters_ago_start <- floor_date(sixteen_quarters_ago, unit = "quarter")
        sixteen_quarters_ago_start
      })
      
      
      # District Court Defendants by Quarter
      excel_defs_by_quarter_bp <- dc_defendants() %>%
        filter(defendant_started >= sixteen_quarters_ago_date() & defendant_started <= last_quarter_end_date()) %>%
        group_by(quarter_started) %>%
        summarize(
          npe = sum(case_type == "NPE"),
          opco = sum(case_type == "Operating Company"),
          design = sum(case_type == "Design Patent"),
          total = n(),
          total_no_design = sum(case_type != "Design Patent")
        )
      
      # Top Districts (Overall)
      excel_top_districts_overall_bp <- dc_defendants() %>%
        filter(defendant_started >= last_quarter_start_date() & defendant_started <= last_quarter_end_date(), case_type != "Design Patent") %>%
        group_by(court_abbrev) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        top_n(5) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(court_abbrev, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))

      # Top Districts (NPE)
      excel_top_districts_npe_bp <- dc_defendants() %>%
        filter(defendant_started >= last_quarter_start_date() & defendant_started <= last_quarter_end_date(), case_type == "NPE") %>%
        group_by(court_abbrev) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        top_n(5) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(court_abbrev, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))

      # Top Districts (Operating Company)
      excel_top_districts_opco_bp <- dc_defendants() %>%
        filter(defendant_started >= last_quarter_start_date() & defendant_started <= last_quarter_end_date(), case_type == "Operating Company") %>%
        group_by(court_abbrev) %>%
        summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
        top_n(5) %>%
        mutate(total_defs = sum(defendant_count)) %>%
        mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
        select(court_abbrev, defendant_count, percent_defs) %>%
        arrange(desc(defendant_count))

      # PTAB filings by Quarter
      excel_ptab_by_quarter_bp <- ptab_petitions() %>%
        filter(ptab_filing_date >= sixteen_quarters_ago_date() & ptab_filing_date <= last_quarter_end_date()) %>%
        group_by(quarter_filed) %>%
        summarize(
          npe = sum(ptab_case_type == "IPR"),
          opco = sum(ptab_case_type == "CBM"),
          design = sum(ptab_case_type == "PGR"),
          total = n()
        )
      
      # Defendants Added by Quarter
      writeData(blog_post_export, sheet = "Defs Added by Quarter", x = excel_defs_by_quarter_bp, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(blog_post_export, sheet = "Defs Added by Quarter", x = paste0(format(sixteen_quarters_ago_start, format = "%m/%d/%Y"), " - ", format(last_quarter_end, format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(blog_post_export, sheet = "Defs Added by Quarter", x = format(Sys.Date(), format = "%m/%d/%Y"), startCol = 2, startRow = 3)
      addStyle(blog_post_export, sheet = "Defs Added by Quarter", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "Defs Added by Quarter", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      
      # Petitions Filed by Quarter
      writeData(blog_post_export, sheet = "PTAB Filings by Quarter", x = excel_ptab_by_quarter_bp, startCol = 1, startRow = 8, colNames = FALSE)
      writeData(blog_post_export, sheet = "PTAB Filings by Quarter", x = paste0(format(sixteen_quarters_ago_start, format = "%m/%d/%Y"), " - ", format(last_quarter_end, format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(blog_post_export, sheet = "PTAB Filings by Quarter", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(blog_post_export, sheet = "PTAB Filings by Quarter", style = x_axis_format, rows = 8:100, cols = 1:100, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "PTAB Filings by Quarter", style = data_label_format, rows = 8:100, cols = 2:100, gridExpand = TRUE)
      
      # Top Districts
      writeData(blog_post_export, sheet = "Top Districts", x = excel_top_districts_overall_bp, startCol = 1, startRow = 9, colNames = FALSE)
      writeData(blog_post_export, sheet = "Top Districts", x = excel_top_districts_npe_bp, startCol = 1, startRow = 17, colNames = FALSE)
      writeData(blog_post_export, sheet = "Top Districts", x = excel_top_districts_opco_bp, startCol = 1, startRow = 25, colNames = FALSE)
      writeData(blog_post_export, sheet = "Top Districts", x = paste0(format(last_quarter_start, format = "%m/%d/%Y"), " - ", format(last_quarter_end, format = "%m/%d/%Y")), startCol = 2, startRow = 2)
      writeData(blog_post_export, sheet = "Top Districts", x = format(Sys.Date(), format = "%m/%d/%Y") , startCol = 2, startRow = 3)
      addStyle(blog_post_export, sheet = "Top Districts", style = x_axis_format, rows = 9:13, cols = 1:1, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "Top Districts", style = x_axis_format, rows = 17:21, cols = 1:1, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "Top Districts", style = x_axis_format, rows = 25:29, cols = 1:1, gridExpand = TRUE)
      
      addStyle(blog_post_export, sheet = "Top Districts", style = data_label_format, rows = 9:13, cols = 2:2, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "Top Districts", style = data_label_format, rows = 17:21, cols = 2:2, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "Top Districts", style = data_label_format, rows = 25:29, cols = 2:2, gridExpand = TRUE)
      
      addStyle(blog_post_export, sheet = "Top Districts", style = percentage_format, rows = 9:13, cols = 3:3, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "Top Districts", style = percentage_format, rows = 17:21, cols = 3:3, gridExpand = TRUE)
      addStyle(blog_post_export, sheet = "Top Districts", style = percentage_format, rows = 25:29, cols = 3:3, gridExpand = TRUE)
      
      saveWorkbook(blog_post_export, file, overwrite = TRUE)
    })
  
  
  
}

shinyApp(ui,server)

