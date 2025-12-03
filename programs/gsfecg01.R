################################################################
##### programs_external/gsfecg01.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/gsfecg01.R
################################################################


################################################################################
## Original Reporting Effort: Standards
## Program Name:              gsfecg01.R
## R version:                 4.2.1
## Short Description:         Mean and 95% Confidence Interval of ECG parameters Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      March 11, 2024
## Input:
## Output:
## Remarks:                   Include parameters:
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

tblid <- "GSFECG01"

################################################################################
# Get titles and footnotes:
################################################################################

title_footer <- get_titles_from_file(tblid)
string_map <- default_str_map

################################################################################
# Process data:
################################################################################

# define function for data processing:
dt <- function(param) {
  adeg <- pharmaverseadamjnj::adeg %>%
    filter(PARAM == param & SAFFL == "Y") %>%
    mutate(
      # add abbreviation to TRT01A
      TRT01A = forcats::fct_recode(
        TRT01A,
        "Xanomeline (High)" = "Xanomeline High Dose",
        "Xanomeline (Low)" = "Xanomeline Low Dose",
        "Placebo (PBO)" = "Placebo"
      )
    ) %>%
    group_by(TRT01A, AVISITN, AVISIT) %>%
    summarize(
      n = n(),
      mean = mean(AVAL, na.rm = TRUE),
      sd = sd(AVAL, na.rm = TRUE),
      se = sd(AVAL, na.rm = TRUE) / sqrt(n())
    ) %>%
    ungroup() %>%
    # fill in missing values for time points which don't have data
    tidyr::complete(
      TRT01A,
      tidyr::nesting(AVISIT),
      fill = list(n = 0, mean = 0.0, sd = 0.0, se = 0.0)
    ) %>%
    # filling zero in decimal place for table display
    # mutate(meanC = sprintf('%.1f', mean)) %>%
    mutate(meanC = tidytlg::roundSAS(mean, digits = 1, as_char = TRUE)) %>%
    # reverse treatment group order: present from top to bottom in table
    mutate(trt_rev = forcats::fct_relevel(TRT01A, rev(levels(TRT01A))))

  # Include time points only for subjects at least 10% left
  avisit_n <- adeg %>%
    group_by(AVISIT) %>%
    summarize(n = sum(n)) %>%
    ungroup()

  baseline_n <- avisit_n %>%
    filter(AVISIT == "Baseline") %>%
    select(total = n)

  avisit_n <- cbind(avisit_n, baseline_n) %>%
    mutate(prop = (n / total)) %>%
    # remove time points of subjects less than 10%
    filter(prop >= 0.1) %>%
    select(AVISIT)

  time_point <- as.vector(unlist(avisit_n["AVISIT"]))

  adveg <- adeg %>%
    filter(AVISIT %in% time_point)
}


# generate data set for each parameter
adeg_hr <- dt("ECG Mean Heart Rate (beats/min)")
adeg_pr <- dt("PR Interval, Aggregate (msec)")
adeg_rr <- dt("RR Interval, Aggregate (msec)")
adeg_qrs <- dt("QRS Duration, Aggregate (msec)")
adeg_qt <- dt("QT Interval, Corrected (msec)")
adeg_qtcf <- dt("QTcF Interval, Aggregate (msec)")
adeg_qtcb <- dt("QTcB Interval, Aggregate (msec)")


################################################################################
# Generate plot:
################################################################################

# define function for creating the body of graphic:
g_line <- function(df) {
  # line plot -------------------------------------------------------
  plot <- ggplot(
    df,
    aes(
      x = .data$AVISIT,
      y = .data$mean,
      group = .data$TRT01A,
      color = .data$TRT01A,
      linetype = .data$TRT01A,
      shape = .data$TRT01A
    )
  ) +
    geom_errorbar(
      aes(
        ymin = .data$mean - 1.96 * .data$se,
        ymax = .data$mean + 1.96 * .data$se
      ),
      width = 0.1,
      position = pd
    ) +
    geom_line(aes(x = AVISIT, y = mean), position = pd) +
    geom_point(position = pd) +
    # Use scales package for automatic breaks calculation
    scale_y_continuous(
      breaks = scales::extended_breaks(n = 5),
      labels = scales::label_number(accuracy = 1)
    ) +

    # assign colorblind friendly palette
    scale_color_manual(values = cbbPalette) +
    labs(x = x_label, y = y_label) +
    theme_bw() +
    theme(
      text = element_text(size = 9, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9, face = "bold")
    )

  # mean value table ----------------------------------------
  table_mean <- df %>%
    ggplot(aes(x = .data$AVISIT, y = .data$trt_rev, label = .data$meanC)) +
    geom_text(size = 2.5) +

    # abbreviate table text label
    scale_y_discrete(labels = table_text) +
    theme_bw() +
    theme(
      title = element_text(size = 8, face = "bold"),
      axis.text = element_text(size = 8),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
    ) +
    labs(title = "Mean Value")

  # number of patients table ----------------------------------------
  table_n <- df %>%
    ggplot(aes(x = .data$AVISIT, y = .data$trt_rev, label = .data$n)) +
    geom_text(size = 2.5) +

    # abbreviate table text label
    scale_y_discrete(labels = table_text) +
    theme_bw() +
    theme(
      title = element_text(size = 8, face = "bold"),
      axis.text = element_text(size = 8),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
    ) +
    labs(title = "Number of Subjects With Data")

  # compose final object by putting plot, table, legend together --------
  final <- plot /
    table_mean /
    table_n +
    plot_layout(heights = c(7.5, 1.3, 1.3)) +
    plot_annotation(
      title = param_title,
      subtitle = "",
      theme = theme(plot.title = element_text(size = 9))
    )
}


# define parameters for plotting:
# present treatment group in y axis table from top to bottom (reverse)
table_text <- c("PBO", "Xan High", "Xan Low")

# assign colorblind friendly palette to treatment groups: black(Xan Low), orange(Xan High), dark blue(PBO)
cbbPalette <- c("#000000", "#E69F00", "#0072B2")

pd <- position_dodge(0.3)


# ECG Mean Heart Rate
x_label <- ""
y_label <- "Mean Value (95% CI) \n ECG Mean Heart Rate (beats/min)"
param_title <- "ECG Parameter: ECG Mean Heart Rate (beats/min)"

# call for plot generation
pt_hr <- g_line(df = adeg_hr)


# PR Interval, Aggregate
x_label <- ""
y_label <- "Mean Value (95% CI) \n PR Interval, Aggregate (msec)"
param_title <- "ECG Parameter: PR Interval, Aggregate (msec)"

# call for plot generation
pt_pr <- g_line(df = adeg_pr)


# RR Interval, Aggregate
x_label <- ""
y_label <- "Mean Value (95% CI) \n RR Interval, Aggregate (msec)"
param_title <- "ECG Parameter: RR Interval, Aggregate (msec)"

# call for plot generation
pt_rr <- g_line(df = adeg_rr)


# QRS Duration, Aggregate
x_label <- ""
y_label <- "Mean Value (95% CI) \n QRS Duration, Aggregate (msec)"
param_title <- "ECG Parameter: QRS Duration, Aggregate (msec)"

# call for plot generation
pt_qrs <- g_line(df = adeg_qrs)


# QT Interval, Corrected
x_label <- ""
y_label <- "Mean Value (95% CI) \n QT Interval, Corrected (msec)"
param_title <- "ECG Parameter: QT Interval, Corrected (msec)"

# call for plot generation
pt_qt <- g_line(df = adeg_qt)


# QTcF Interval, Aggregate
x_label <- ""
y_label <- "Mean Value (95% CI) \n QTcF Interval, Aggregate (msec)"
param_title <- "ECG Parameter: QTcF Interval, Aggregate (msec)"

# call for plot generation
pt_qtcf <- g_line(df = adeg_qtcf)


# QTcB Interval, Aggregate
x_label <- ""
y_label <- "Mean Value (95% CI) \n QTcB Interval, Aggregate (msec)"
param_title <- "ECG Parameter: QTcB Interval, Aggregate (msec)"

# call for plot generation
pt_qtcb <- g_line(df = adeg_qtcb)


################################################################################
# Create png and output file:
################################################################################

# create png file and output figure
pname_hr <- paste0(tolower(tblid), "_hr", ".png")

png(
  write_path(opath, pname_hr),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_hr)) ### print png path and name in log
print(pt_hr)
dev.off()

pname_pr <- paste0(tolower(tblid), "_pr", ".png")

png(
  write_path(opath, pname_pr),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_pr)) ### print png path and name in log
print(pt_pr)
dev.off()

pname_rr <- paste0(tolower(tblid), "_rr", ".png")

png(
  write_path(opath, pname_rr),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_rr)) ### print png path and name in log
print(pt_rr)
dev.off()

pname_qrs <- paste0(tolower(tblid), "_qrs", ".png")

png(
  write_path(opath, pname_qrs),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_qrs)) ### print png path and name in log
print(pt_qrs)
dev.off()

pname_qt <- paste0(tolower(tblid), "_qt", ".png")

png(
  write_path(opath, pname_qt),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_qt)) ### print png path and name in log
print(pt_qt)
dev.off()

pname_qtcf <- paste0(tolower(tblid), "_qtcf", ".png")

png(
  write_path(opath, pname_qtcf),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_qtcf)) ### print png path and name in log
print(pt_qtcf)
dev.off()

pname_qtcb <- paste0(tolower(tblid), "_qtcb", ".png")

png(
  write_path(opath, pname_qtcb),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_qtcb)) ### print png path and name in log
print(pt_qtcb)
dev.off()

tidytlg::gentlg(
  tlf = "g",
  plotnames = write_path(
    opath,
    c(pname_hr, pname_pr, pname_rr, pname_qrs, pname_qt, pname_qtcf, pname_qtcb)
  ),
  plotwidth = 8,
  orientation = "landscape",
  opath = write_path(opath),
  file = tblid,
  title = title_footer$title,
  footers = title_footer$main_footer
)
