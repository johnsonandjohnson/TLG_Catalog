################################################################
##### programs_external/gsfae02.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/gsfae02.R
################################################################


################################################################################
## Original Reporting Effort: Standards
## Program Name:              gsfae02.R
## R version:                 4.2.1
## Short Description:         Time to Adverse Events Leading to Permanent Treatment Discontinuation
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      March 25, 2024
## Input:
## Output:
## Remarks:
## R-functions:
## R-function Sample Call:
##
## Modification History:
##  Rev #:
##  Modified By:
##  Reporting Effort:
##  Date:
##  Description:
################################################################################

################################################################################
# Prep environment:
################################################################################

library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(ggplot2)
library(patchwork)
library(scales)
library(tidyr)
library(tidytlg)
library(junco)

################################################################################
# Define output ID:
################################################################################

tblid <- "GSFAE02"

################################################################################
# Get titles and footnotes:
################################################################################

title_footer <- get_titles_from_file(tblid)
string_map <- default_str_map

################################################################################
# Process data:
################################################################################

# reading data

adttesaf <- pharmaverseadamjnj::adttesaf %>%
  filter(PARAMCD == "TTAELPTD", SAFFL == "Y") %>%
  # add abbreviation to TRT01A
  mutate(
    TRT01A = forcats::fct_recode(
      TRT01A,
      "Xanomeline (High)" = "Xanomeline High Dose",
      "Xanomeline (Low)" = "Xanomeline Low Dose",
      "Placebo (PBO)" = "Placebo"
    )
  )

# fit KM model
fit <- survival::survfit(
  survival::Surv(AVAL, 1 - CNSR) ~ TRT01A,
  data = adttesaf
)


################################################################################
# Generate plot:
################################################################################

# define parameters for plotting:
# assign colorblind friendly palette: black(Xan Low), orange(Xan High), dark blue(PBO)
cbbPalette <- c("#000000", "#E69F00", "#0072B2")

x_label <- "Days from First Dose"
y_label <- "Cumulative incidence (%) \n AEs Leading to Treatment Discontinuation"

# present treatment group in y axis table from top to bottom (reverse)
table_text <- c("PBO", "Xan High", "Xan Low")


# survival plot ----------------------------------------------
## convert model object to tidy data frame for plotting step lines
kmdata <- broom::tidy(fit) %>%
  mutate(
    treatment = stringr::str_remove(strata, "TRT01A="),
    treatment = factor(
      treatment,
      levels = c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)")
    )
  ) %>%
  select(-strata) %>%
  arrange(treatment)

## calculate the percentage of cumulative counts
cum <- kmdata %>%
  group_by(treatment) %>%
  mutate(cum_count = cumsum(n.event)) %>%
  ungroup()

total <- adttesaf %>%
  group_by(TRT01A) %>%
  summarise(total = n_distinct(USUBJID)) %>%
  rename(treatment = TRT01A) %>%
  ungroup()

cumdata <- cum %>%
  left_join(total, by = "treatment") %>%
  mutate(cum = (cum_count / total) * 100)


## create step lines plot
plot <- cumdata %>%
  ggplot(aes(x = time, y = cum, color = treatment)) +
  # plot step_lines
  geom_step(aes(linetype = treatment)) +

  # assign colorblind friendly palette
  scale_color_manual(values = cbbPalette) +

  # Use scales package for automatic breaks calculation
  scale_x_continuous(
    breaks = scales::extended_breaks(n = 10),
    labels = scales::label_number(accuracy = 1)
  ) +
  # Use scales package for automatic breaks calculation
  scale_y_continuous(
    breaks = scales::extended_breaks(n = 5),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    x = x_label,
    y = y_label
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 9, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    panel.grid = element_blank()
  )

## get breaks from the plot so they match the table
built_surv_plot <- ggplot_build(plot)
breaks <- built_surv_plot$layout$panel_params[[1]]$x$breaks


# Subjects at risk table -------------------------------------
## create risk table for table plot
risk_table <- summary(fit, times = breaks, extend = TRUE) %>%
  with(., data.frame(time, strata, n.risk)) %>%
  mutate(
    treatment = stringr::str_remove(strata, "TRT01A="),
    treatment = factor(
      treatment,
      levels = c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)")
    )
  ) %>%
  select(-strata) %>%
  arrange(treatment)

## create table plot
table_plot <- ggplot(aes(y = treatment), data = risk_table) +
  geom_text(aes(x = time, label = n.risk), size = 3) +

  # reverse y-scale, so the 1st level of trt is on top
  # abbreviate table text label
  scale_y_discrete(limits = rev, labels = table_text) +
  coord_cartesian(xlim = c(0, max(breaks))) +
  labs(title = "Subjects at risk") +
  theme_bw() +
  theme(
    title = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black", hjust = 0.9),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
  )

# Cumulative number of Subjects with Event -------------------------------------
## create cumulative number of subjects with event for table plot
cum_table <- summary(fit, times = breaks, extend = TRUE) %>%
  with(., data.frame(time, strata, n.event)) %>%
  mutate(
    treatment = stringr::str_remove(strata, "TRT01A="),
    treatment = factor(
      treatment,
      levels = c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)")
    )
  ) %>%
  select(-strata) %>%
  arrange(treatment)

## calculate cumulative counts per each treatment
cum_table <- cum_table %>%
  group_by(treatment) %>%
  mutate(cum_sum = cumsum(n.event)) %>%
  ungroup()

## create table plot
table_plot2 <- ggplot(aes(y = treatment), data = cum_table) +
  geom_text(aes(x = time, label = cum_sum), size = 3) +

  # reverse y-scale, so the 1st level of trt is on top
  # abbreviate table text label
  scale_y_discrete(limits = rev, labels = table_text) +
  coord_cartesian(xlim = c(0, max(breaks))) +
  labs(title = "Cumulative Number of Subjects with Events") +
  theme_bw() +
  theme(
    title = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black", hjust = 0.9),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
  )

# compose final object by putting plot, table together ------------
final <- plot /
  table_plot /
  table_plot2 +
  plot_layout(heights = c(7.5, 1.3, 1.3))


################################################################################
# Create png and output file:
################################################################################

# create png file and output figure
pname <- paste0(tolower(tblid), ".png")

png(
  write_path(opath, pname),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname)) ### print png path and name in log
print(final)
dev.off()

if (length(title_footer$main_footer) == 0) {
  title_footer$main_footer <- NULL
}

tidytlg::gentlg(
  tlf = "g",
  plotnames = write_path(opath, pname),
  plotwidth = 8,
  orientation = "landscape",
  opath = write_path(opath),
  file = tblid,
  title = title_footer$title,
  footers = title_footer$main_footer
)
