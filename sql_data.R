library(RPostgreSQL)

#### GGPLOT QUERIES ####
  # Defendants Added to Patent Campaigns
    dc_defendants <- function(){
      drv <- dbDriver("PostgreSQL")
      con <- dbConnect(drv,dbname="rpx",host="prod-coredb",
                       port=5432,user="mktintel_app",password="mktintel_app_pwd")
      query <- paste0(
        "SELECT 
        	campaign_id,
        	CASE WHEN case_type = 'Operating Company' THEN 'Operating Company' ELSE 'NPE' END AS case_type,
        	original_court,
        	court_abbreviations.court_abbrev,
        	market_sector,
        	defendant_ent_id,
        	normalized_defendant,
        	defendant_started,
        	CAST(EXTRACT(year FROM defendant_started) AS VARCHAR) AS year,
        	EXTRACT(quarter FROM defendant_started) AS quarter,
        	EXTRACT(year FROM defendant_started) || '\n' || 'Q' ||	EXTRACT(quarter FROM defendant_started) AS quarter_started_ggplot,
        	EXTRACT(year FROM defendant_started) || ' ' || 'Q' ||	EXTRACT(quarter FROM defendant_started) AS quarter_started
        FROM rpx_reporting.lits_campaigns
        	LEFT JOIN core.court_abbreviations 
            ON lits_campaigns.original_court = court_abbreviations.court_name
        WHERE dj = '0'
        	AND defendant_started > '2009-12-31'
        	AND all_design_pats IS FALSE
    
        UNION
    
        SELECT 
        	campaign_id,
        	'Design Patent' AS case_type,
        	original_court,
        	court_abbreviations.court_abbrev,
      		market_sector,
        	defendant_ent_id,
        	normalized_defendant,
        	defendant_started,
        	CAST(EXTRACT(year FROM defendant_started) AS VARCHAR) AS year,
        	EXTRACT(quarter FROM defendant_started) AS quarter,
        	EXTRACT(year FROM defendant_started) || '\n' || 'Q' ||	EXTRACT(quarter FROM defendant_started) AS quarter_started_ggplot,
	        EXTRACT(year FROM defendant_started) || ' ' || 'Q' ||	EXTRACT(quarter FROM defendant_started) AS quarter_started
        FROM rpx_reporting.lits_campaigns
          LEFT JOIN core.court_abbreviations 
            ON lits_campaigns.original_court = court_abbreviations.court_name
        WHERE dj = '0'
        	AND defendant_started > '2009-12-31'
        	AND all_design_pats IS TRUE
        ORDER BY defendant_started")
      executeQ <- dbGetQuery(con,query)
      dbDisconnect(con)
      return(executeQ)
    }
   dc_defendants()
  
  # PTAB Petitions Filed
    ptab_petitions <- function(){
      drv <- dbDriver("PostgreSQL")
      con <- dbConnect(drv,dbname="rpx",host="prod-coredb",
                       port=5432,user="mktintel_app",password="mktintel_app_pwd")
      query <- paste0(
        "SELECT DISTINCT 
        	case_number,
        	ptab_case_type,
        	ptab_filing_date,
        	stripped_patnum,
        	institution_decision_date,
        	institution_decision_outcome,
        	final_decision_date,
        	final_decision_outcome,
        	final_outcome_date,
        	final_outcome,
        	EXTRACT(year FROM ptab_filing_date) AS year,
        	EXTRACT(quarter FROM ptab_filing_date) AS quarter,
        	EXTRACT(year FROM ptab_filing_date) || '\n' || 'Q' || EXTRACT(quarter FROM ptab_filing_date) AS quarter_filed_ggplot,
        	EXTRACT(year FROM ptab_filing_date) || ' ' || 'Q' || EXTRACT(quarter FROM ptab_filing_date) AS quarter_filed
        FROM rpx_reporting.all_ptab
        WHERE ptab_case_type <> 'DER'")
      executeQ <- dbGetQuery(con,query)
      dbDisconnect(con)
      return(executeQ)
    }
    ptab_petitions()
    
    
