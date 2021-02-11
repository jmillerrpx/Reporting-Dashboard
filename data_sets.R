library(dplyr)
source("sql_data.R")

#### GGPLOT DATASETS ####
  # District Court Defendants by Quarter
  defs_by_quarter <- dc_defendants() %>%
    filter(defendant_started > '2016-12-31' & defendant_started < '2021-01-01') %>%
    mutate(case_type = factor(case_type, levels=c("Design Patent","Operating Company","NPE"))) %>%
    group_by(quarter_started_ggplot, case_type) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
    ungroup()
  defs_by_quarter
  
  # District Court Defendants by Year
  defs_by_year <- dc_defendants() %>%
    # filter(defendant_started > '2009-12-31' & defendant_started < '2021-01-01') %>%
    mutate(case_type = factor(case_type, levels=c("Design Patent","Operating Company","NPE"))) %>%
    group_by(year, case_type) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>% 
    ungroup()
  defs_by_year
  
  # Top Districts (Overall)
  top_districts_overall <- dc_defendants() %>%
    filter(defendant_started > '2020-01-01' & defendant_started < '2020-12-31', case_type != "Design Patent") %>%
    group_by(court_abbrev) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
    top_n(5) %>%
    select(court_abbrev, defendant_count) %>%
    arrange(defendant_count)
  top_districts_overall
  
  # Top Districts (NPE)
  top_districts_npe <- dc_defendants() %>%
    filter(defendant_started > '2020-01-01' & defendant_started < '2020-12-31', case_type == "NPE") %>%
    group_by(court_abbrev) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
    top_n(5) %>%
    select(court_abbrev, defendant_count) %>%
    arrange(defendant_count)
  top_districts_npe
  
  # Top Districts (Operating Company)
  top_districts_opco <- dc_defendants() %>%
    filter(defendant_started > '2020-01-01' & defendant_started < '2020-12-31', case_type == "Operating Company") %>%
    group_by(court_abbrev) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
    top_n(5) %>%
    select(court_abbrev, defendant_count) %>%
    arrange(defendant_count)
  top_districts_opco
  
  # PTAB Petitions Filed by Quarter
  ptab_by_quarter <- ptab_petitions() %>%
    filter(ptab_filing_date > '2016-12-31' & ptab_filing_date < '2021-01-01') %>%
    mutate(ptab_case_type = factor(ptab_case_type, levels=c("PGR","CBM","IPR"))) %>%
    group_by(quarter_filed_ggplot, ptab_case_type) %>%
    summarize(petition_count = n_distinct(case_number)) %>%
    ungroup()
  ptab_by_quarter
  
  # PTAB Petitions Filed by Year
  ptab_by_year <- ptab_petitions() %>%
    filter(ptab_filing_date > '2012-12-31' & ptab_filing_date < '2021-01-01') %>%
    mutate(ptab_case_type = factor(ptab_case_type, levels=c("PGR","CBM","IPR"))) %>%
    group_by(year, ptab_case_type) %>%
    summarize(petition_count = n_distinct(case_number)) %>%
    ungroup()
  ptab_by_year
  
  
#### EXCEL EXPORT DATASETS ####
  # District Court Defendants by Quarter
  excel_defs_by_quarter <- dc_defendants() %>%
    filter(defendant_started > '2016-12-31' & defendant_started < '2021-01-01') %>%
    group_by(quarter_started) %>%
    summarize(
      npe = sum(case_type == "NPE"),
      opco = sum(case_type == "Operating Company"),
      design = sum(case_type == "Design Patent"),
      total = n()
    )
  excel_defs_by_quarter
  
  # District Court Defendants by Year
  excel_defs_by_year <- dc_defendants() %>%
    filter(defendant_started > '2009-12-31' & defendant_started < '2021-01-01') %>%
    group_by(year) %>%
    summarize(
      npe = sum(case_type == "NPE"),
      opco = sum(case_type == "Operating Company"),
      design = sum(case_type == "Design Patent"),
      total = n()
    )
  excel_defs_by_year
  
  # Top Districts (Overall)
  excel_top_districts_overall <- dc_defendants() %>%
    filter(defendant_started > '2020-01-01' & defendant_started < '2020-12-31', case_type != "Design Patent") %>%
    group_by(court_abbrev) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
    mutate(total_defs = sum(defendant_count)) %>%
    mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
    top_n(5, defendant_count) %>%
    select(court_abbrev, defendant_count, percent_defs) %>%
    arrange(defendant_count)
  excel_top_districts_overall 
  
  # Top Districts (NPE)
  excel_top_districts_npe <- dc_defendants() %>%
    filter(defendant_started > '2020-01-01' & defendant_started < '2020-12-31', case_type == "NPE") %>%
    group_by(court_abbrev) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
    mutate(total_defs = sum(defendant_count)) %>%
    mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
    top_n(5, defendant_count) %>%
    select(court_abbrev, defendant_count, percent_defs) %>%
    arrange(defendant_count)
  excel_top_districts_npe
  
  # Top Districts (Operating Company)
  excel_top_districts_opco <- dc_defendants() %>%
    filter(defendant_started > '2020-01-01' & defendant_started < '2020-12-31', case_type == "Operating Company") %>%
    group_by(court_abbrev) %>%
    summarize(defendant_count = n_distinct(campaign_id,defendant_ent_id)) %>%
    mutate(total_defs = sum(defendant_count)) %>%
    mutate(percent_defs = round(defendant_count / total_defs,2)) %>%
    top_n(5, defendant_count) %>%
    select(court_abbrev, defendant_count, percent_defs) %>%
    arrange(defendant_count)
  excel_top_districts_opco
  
  # PTAB filings by Quarter
  excel_ptab_by_quarter <- ptab_petitions() %>%
    filter(ptab_filing_date > '2016-12-31' & ptab_filing_date < '2021-01-01') %>%
    group_by(quarter_filed) %>%
    summarize(
      npe = sum(ptab_case_type == "IPR"),
      opco = sum(ptab_case_type == "CBM"),
      design = sum(ptab_case_type == "PGR"),
      total = n()
    )
  excel_ptab_by_quarter
  
  # PTAB filings by Year
  excel_ptab_by_year <- ptab_petitions() %>%
    filter(ptab_filing_date > '2012-12-31' & ptab_filing_date < '2021-01-01') %>%
    group_by(year) %>%
    summarize(
      npe = sum(ptab_case_type == "IPR"),
      opco = sum(ptab_case_type == "CBM"),
      design = sum(ptab_case_type == "PGR"),
      total = n()
    )
  excel_ptab_by_year