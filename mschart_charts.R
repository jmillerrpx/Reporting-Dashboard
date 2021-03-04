library(mschart)
library(officer)
source("data_sets.R")

# RPX Style for Stacked Bar
v_stacked_bar <- mschart_theme(
  title_y_rot = 270,
  axis_text_x = fp_text(color = 'black', font.size = 12, font.family = "Arial"),
  axis_text_y = fp_text(color = 'black', font.size = 12, font.family = "Arial"),
  axis_title_y = fp_text(color = 'black', font.size = 13.3, font.family = "Arial"),
  legend_text = fp_text(color = 'black', font.size = 12, font.family = "Arial"),
  grid_major_line_x = fp_border(style = "none"),
  grid_major_line_y = fp_border(style = "none"),
  grid_minor_line_x = fp_border(style = "none"),
  grid_minor_line_y = fp_border(style = "none"),
  legend_position = "b"
)

# Stacked Bar Data Labels
ppt_defs_by_year_data_labels <- list(
  NPE = fp_text(font.size = 12, color = rgb(255,255,255, maxColorValue = 255), font.family = "Arial"),
  'Operating Company' = fp_text(font.size = 12, color = rgb(255,255,255, maxColorValue = 255), font.family = "Arial"),
  'Design Patent' = fp_text(font.size = 12, color = rgb(0,0,0, maxColorValue = 255), font.family = "Arial")
)


# Format Slide (Defendants Added to Patent Campaigns by Year)
  ppt_defs_by_year <- ms_barchart(
    defs_by_year, x = "year", y = "defendant_count", group = "case_type") %>%
    as_bar_stack(gap_width = 50) %>%
    chart_ax_x(major_tick_mark = "none", num_fmt = "0") %>%
    chart_ax_y(major_tick_mark = "none", display = 0) %>%
    chart_data_labels(num_fmt = '#,##0', position = "ctr", show_val = TRUE) %>%
    chart_labels_text(values = ppt_defs_by_year_data_labels) %>%
    chart_labels(xlab = NULL, ylab = NULL) %>%
    set_theme(v_stacked_bar) %>%
    chart_data_fill(
    values = c(NPE = rgb(246,138,29, maxColorValue = 255), 'Operating Company' = rgb(0,119,191, maxColorValue = 255),
               'Design Patent' = rgb(186,188,192, maxColorValue = 255)))

ppt <- read_pptx("rpx_widescreen.pptx") %>%
  add_slide(layout = "Title and Content with Subtitle", master = "RPX") %>%
    ph_with(value = "Defendants Added to Patent Campaigns", location = ph_location_type(type="title")) %>%
    ph_with(value = paste("Data as of: ", format(Sys.Date(), format = "%m/%d/%Y"), "\nSource: RPX Research; PACER"), location = ph_location_type(type="ftr")) %>%
    ph_with(value = ppt_defs_by_year, location = ph_location_type(type="body"))

# # # Format Slide (Top Districts - Overall)
#   ppt_top_districts_overall <- ms_barchart(top_districts_overall, x = "court_abbrev", y = "defendant_count")
#  
#   add_slide(ppt, layout = "Title and 3 Content", master = "RPX") %>%
#     ph_with(value = ppt_top_districts_overall, location = ph_location_fullsize())
# 
print(ppt, target = "template_example.pptx")


# layout_summary(ppt)

# 
# x <- read_pptx("rpx_widescreen.pptx")
# layout_properties ( x = x, layout = "Title and 3 Content", master = "RPX" )
