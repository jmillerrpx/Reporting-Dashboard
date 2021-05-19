library(RPostgreSQL)

# Defendants Added to Patent Campaigns
dc_defendants <- function(){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname="rpx",host="prod-coredb",
                   port=5432,user="mktintel_app",password="mktintel_app_pwd")
  query <- paste0(
    "SELECT DISTINCT
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
      	AND has_eseller_defendant IS FALSE
      	AND has_franchise_defendant IS FALSE
      
      UNION
      
      SELECT DISTINCT
      	campaign_id,
      	'PDPEF' AS case_type,
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
      WHERE (all_design_pats IS TRUE OR has_eseller_defendant IS TRUE OR has_franchise_defendant IS TRUE)
      	AND dj = '0'
      	AND defendant_started > '2009-12-31'
      ORDER BY defendant_started;")
  executeQ <- dbGetQuery(con,query)
  dbDisconnect(con)
  return(executeQ)
}
dc_defendants()

# Top Districts And Judges
top_districts_and_judges <- function(){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname="rpx",host="prod-coredb",
                   port=5432,user="mktintel_app",password="mktintel_app_pwd")
  query <- paste0(
    "SELECT DISTINCT 
        	court_common,
        	judge,
        	CASE WHEN case_type = 'Operating Company' THEN 'Operating Company' ELSE 'NPE' END AS case_type,
        	campaign_id,
        	defendant_ent_id,
        	defendant_started,
        	defendant_started_in_district
        FROM rpx_reporting.all_lits 
        WHERE dj = '0'
        	AND defendant_started > '2009-12-31'
        	AND defendant_started_in_district > '2009-12-31'
        	AND all_design_pats IS FALSE
        
        UNION
        
        SELECT DISTINCT 
        	court_common,
        	judge,
        	'Design Patent' AS case_type,
        	campaign_id,
        	defendant_ent_id,
        	defendant_started,
        	defendant_started_in_district
        FROM rpx_reporting.all_lits 
        WHERE dj = '0'
        	AND defendant_started > '2009-12-31'
        	AND defendant_started_in_district > '2009-12-31'
        	AND all_design_pats IS TRUE
        ORDER BY defendant_started")
  executeQ <- dbGetQuery(con,query)
  dbDisconnect(con)
  return(executeQ)
}
top_districts_and_judges()

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
        	CASE WHEN institution_decision_outcome = 'Some Claims Instituted' THEN 'Partial Institution' ELSE institution_decision_outcome END AS institution_decision_outcome,
        	final_decision_date,
        	final_decision_outcome,
        	final_outcome_date,
        	final_outcome,
        	EXTRACT(year FROM ptab_filing_date)::VARCHAR AS year,
        	EXTRACT(quarter FROM ptab_filing_date) AS quarter,
        	EXTRACT(year FROM ptab_filing_date) || '\n' || 'Q' || EXTRACT(quarter FROM ptab_filing_date) AS quarter_filed_ggplot,
        	EXTRACT(year FROM ptab_filing_date) || ' ' || 'Q' || EXTRACT(quarter FROM ptab_filing_date) AS quarter_filed,
        	EXTRACT(year FROM institution_decision_date) || ' ' || 'Q' || EXTRACT(quarter FROM institution_decision_date) AS quarter_instituted,
        	EXTRACT(year FROM institution_decision_date) || '\n' || 'Q' || EXTRACT(quarter FROM institution_decision_date) AS quarter_instituted_ggplot,
        	EXTRACT(YEAR FROM institution_decision_date)::VARCHAR as institution_year
        FROM rpx_reporting.all_ptab
        WHERE ptab_case_type <> 'DER'")
  executeQ <- dbGetQuery(con,query)
  dbDisconnect(con)
  return(executeQ)
}
ptab_petitions()

alice_patents <- function(){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname="rpx",host="prod-coredb",
                   port=5432,user="mktintel_app",password="mktintel_app_pwd")
  query <- paste0(
    "SELECT DISTINCT 
          CASE WHEN alice_tracker_dc_normalized.decision_date >= '2018-02-08' THEN 'Post-Berkheimer' ELSE 'Pre-Berkheimer' END AS prepost, 
          CASE WHEN alice_tracker_dc_normalized.rpx_outcome = 'win' THEN 'Invalid'
               WHEN alice_tracker_dc_normalized.rpx_outcome = 'mixed' THEN 'Mixed by Claim'
               WHEN alice_tracker_dc_normalized.rpx_outcome = 'loss' THEN 'Not Invalid' 
          END AS rpx_outcome,
          alice_tracker_dc_normalized.patent,
          CASE WHEN all_lits.case_type IS NULL THEN NULL
               WHEN all_lits.case_type = 'Operating Company' THEN 'Operating Company'
          ELSE 'NPE' END AS case_type,
          CASE WHEN alice_tracker_dc_normalized.rolled_stage ~ '12' THEN 'Rule 12' 
               WHEN alice_tracker_dc_normalized.rolled_stage ~'56' then 'Summary Judgment' 
          ELSE 'Other' END AS case_stage,
          CASE WHEN sufficient_facts = 'No' THEN 'Early Resolution Premature'
		           WHEN sufficient_facts = 'Yes' THEN 'Sufficient Facts for Early Resolution'
        	ELSE sufficient_facts END AS sufficient_facts,
          berkheimer_aatrix,
          CASE WHEN market_sector IN ('Other', 'Energy', 'Logistics', 'Manufacturing') THEN 'Other Sectors' ELSE market_sector END AS market_sector,
          all_lits.court_common,
          alice_tracker_dc_normalized.decision_date
      FROM rpx_reporting.alice_tracker_dc_normalized 
        LEFT JOIN rpx_reporting.all_lits 
         ON alice_tracker_dc_normalized.case_key = all_lits.case_key 
      WHERE all_lits.case_key IS NOT NULL
      ORDER BY alice_tracker_dc_normalized.decision_date DESC")
  executeQ <- dbGetQuery(con,query)
  dbDisconnect(con)
  return(executeQ)
}
alice_patents()
