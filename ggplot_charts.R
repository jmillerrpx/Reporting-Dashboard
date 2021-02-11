library(ggplot2)
source("data_sets.R")

# Defendants Added by Quarter
defs_by_quarter_chart <-ggplot(defs_by_quarter, aes(quarter_started_ggplot, defendant_count, fill = case_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
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

# Defendants Added by Year
defs_by_year_chart <-ggplot(defs_by_year, aes(year, defendant_count, fill = case_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(as.numeric(defendant_count), nsmall = 0, big.mark = ","), color = case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
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

#Top Districts (Overall)
top_districts_overall_chart <- ggplot(top_districts_overall, aes(reorder(court_abbrev, defendant_count), defendant_count)) +
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

#Top Districts (NPE)
top_districts_npe_chart <- ggplot(top_districts_npe, aes(reorder(court_abbrev, defendant_count), defendant_count)) +
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

#Top Districts (Operating Company)
top_districts_opco_chart <- ggplot(top_districts_opco, aes(reorder(court_abbrev, defendant_count), defendant_count)) +
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

# PTAB Petitions Filed by Quarter
ptab_by_quarter_chart <-ggplot(ptab_by_quarter, aes(quarter_filed_ggplot, petition_count, fill = ptab_case_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(as.numeric(petition_count), nsmall = 0, big.mark = ","), color = ptab_case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
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



# PTAB Petitions Filed by Year
ptab_by_year_chart <-ggplot(ptab_by_year, aes(year, petition_count, fill = ptab_case_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(rgb(127,127,127,maxColorValue = 255),rgb(246,138,29,maxColorValue = 255), rgb(0,119,191,maxColorValue = 255))) +
  geom_text(aes(label = format(as.numeric(petition_count), nsmall = 0, big.mark = ","), color = ptab_case_type), position = position_stack(vjust = .5), size = 4, show.legend = FALSE) +
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




