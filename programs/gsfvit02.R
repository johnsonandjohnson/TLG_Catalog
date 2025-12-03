################################################################
##### programs_external/gsfvit02.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/gsfvit02.R
################################################################


################################################################################
## Original Reporting Effort: Standards
## Program Name:              gsfvit02.R
## R version:                 4.2.1
## Short Description:         Median and Interquartile Range of Blood Pressure Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      Feb 12, 2024
## Input:
## Output:
## Remarks:                   Include parameters: Systolic BP, Diastolic BP
##                            per FDA guidance box plots should not be used if 3 or more groups, so only present 2 treatment groups
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

tblid <- "GSFVIT02"


################################################################################
# Get titles and footnotes:
################################################################################

title_footer <- get_titles_from_file(tblid)
string_map <- default_str_map

################################################################################
# Process data:
################################################################################

# define function for data processing:
dt <- function(param_cd) {
  df <- pharmaverseadamjnj::advs %>%
    filter(PARAMCD == param_cd & SAFFL == "Y" & ANL01FL == "Y") %>%
    filter(!grepl("Unscheduled", AVISIT)) %>%
    # per FDA guidance box plots should not be used if 3 or more groups
    # so only present 2 treatment groups here
    filter(TRT01A %in% c("Xanomeline High Dose", "Placebo")) %>%
    mutate(
      # shorten "End of Treatment" as EOT
      AVISIT = forcats::fct_recode(
        AVISIT,
        "EOT" = "End Of Treatment",
        "Baseline" = "Screening"
      ),

      # add abbreviation to TRT01A
      TRT01A = forcats::fct_recode(
        TRT01A,
        "Xanomeline (High)" = "Xanomeline High Dose",
        # "Xanomeline (Low)" = "Xanomeline Low Dose",
        "Placebo (PBO)" = "Placebo"
      )
    )

  # drop level for TRT01A not to be presented in graphic
  df$TRT01A <- droplevels(df$TRT01A)

  df2 <- df %>%
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
  avisit_n <- df2 %>%
    group_by(AVISIT) %>%
    summarize(n = sum(n)) %>%
    ungroup()

  base_n <- avisit_n %>%
    filter(AVISIT == "Baseline") %>%
    select(total = n)

  avisit_n <- cbind(avisit_n, base_n) %>%
    mutate(prop = (n / total)) %>%
    # remove time points of subjects less than 10%
    filter(prop >= 0.1) %>%
    select(AVISIT)

  time_point <- as.vector(unlist(avisit_n["AVISIT"]))

  df <- df %>%
    filter(AVISIT %in% time_point)

  df2 <- df2 %>%
    filter(AVISIT %in% time_point)

  list(df, df2)
}


# generate data set for Systolic blood pressure
advs_sys <- dt("SYSBP")
advs_sys_pt <- advs_sys[[1]]
advs_sys_tb <- advs_sys[[2]]


# split data in two graphics
time_point_1 <- c(
  "Baseline",
  "Cycle 02",
  "Cycle 03",
  "Cycle 04",
  "Cycle 05",
  "Cycle 06",
  "Cycle 07"
)

time_point_2 <- c(
  "Cycle 08",
  "Cycle 09",
  "Cycle 10",
  "Cycle 11",
  "Cycle 12",
  "Cycle 13",
  "Cycle 15"
)

time_point_3 <- c(
  "Cycle 17",
  "Cycle 19",
  "Cycle 21",
  "Cycle 23",
  "Cycle 25",
  "Cycle 29",
  "EOT"
)


advs_sys_pt_1 <- advs_sys_pt %>%
  filter(AVISIT %in% time_point_1)

advs_sys_pt_2 <- advs_sys_pt %>%
  filter(AVISIT %in% time_point_2)

advs_sys_pt_3 <- advs_sys_pt %>%
  filter(AVISIT %in% time_point_3)

advs_sys_tb_1 <- advs_sys_tb %>%
  filter(AVISIT %in% time_point_1)

advs_sys_tb_2 <- advs_sys_tb %>%
  filter(AVISIT %in% time_point_2)

advs_sys_tb_3 <- advs_sys_tb %>%
  filter(AVISIT %in% time_point_3)

# generate data set for Diastolic blood pressure
advs_dia <- dt("DIABP")
advs_dia_pt <- advs_dia[[1]]
advs_dia_tb <- advs_dia[[2]]

advs_dia_pt_1 <- advs_dia_pt %>%
  filter(AVISIT %in% time_point_1)

advs_dia_pt_2 <- advs_dia_pt %>%
  filter(AVISIT %in% time_point_2)

advs_dia_pt_3 <- advs_dia_pt %>%
  filter(AVISIT %in% time_point_3)

advs_dia_tb_1 <- advs_dia_tb %>%
  filter(AVISIT %in% time_point_1)

advs_dia_tb_2 <- advs_dia_tb %>%
  filter(AVISIT %in% time_point_2)

advs_dia_tb_3 <- advs_dia_tb %>%
  filter(AVISIT %in% time_point_3)

################################################################################
# Generate plot:
################################################################################

# define function for creating the body of graphic:

g_box <- function(df, df2) {
  # boxplot -------------------------------------------------------
  plot <- ggplot(
    df,
    aes(x = .data$AVISIT, y = .data$AVAL, fill = .data$TRT01A)
  ) +
    geom_boxplot(color = "#E69F00") +
    scale_y_continuous(
      breaks = scales::extended_breaks(n = 5),
      labels = scales::label_number(accuracy = 1)
    ) +

    # assign colorblind friendly palette
    scale_fill_manual(values = cbbPalette) +
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
  table_mean <- df2 %>%
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
  table_n <- df2 %>%
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
table_text <- c("PBO", "Xan Low")

# assign colorblind friendly palette to treatment groups: black(Xan Low), dark blue(PBO)
cbbPalette <- c("#000000", "#0072B2")


# Systolic Blood Pressure
x_label <- ""
y_label <- "Value \n Systolic Blood Pressure (mmHg)"
param_title <- "Vital sign parameter: Systolic Blood Pressure (mmHg)"

# call for Systolic blood pressure part 1
pt_sys_1 <- g_box(
  df = advs_sys_pt_1,
  df2 = advs_sys_tb_1
)

# call for Systolic blood pressure part 2
pt_sys_2 <- g_box(
  df = advs_sys_pt_2,
  df2 = advs_sys_tb_2
)

# call for Systolic blood pressure part 3
pt_sys_3 <- g_box(
  df = advs_sys_pt_3,
  df2 = advs_sys_tb_3
)

# Diastolic Blood Pressure
x_label <- ""
y_label <- "Value \n Diastolic Blood Pressure (mmHg)"
param_title <- "Vital sign parameter: Diastolic Blood Pressure (mmHg)"


# call for Diastolic blood pressure part 1
pt_dia_1 <- g_box(
  df = advs_dia_pt_1,
  df2 = advs_dia_tb_1
)

# call for Diastolic blood pressure part 2
pt_dia_2 <- g_box(
  df = advs_dia_pt_2,
  df2 = advs_dia_tb_2
)

# call for Diastolic blood pressure part 3
pt_dia_3 <- g_box(
  df = advs_dia_pt_3,
  df2 = advs_dia_tb_3
)


################################################################################
# Create png and output file:
################################################################################

# create png file and output figure
pname_sys_1 <- paste0(tolower(tblid), "_sys_1", ".png")

png(
  write_path(opath, pname_sys_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_sys_1)) ### print png path and name in log
print(pt_sys_1)
dev.off()

pname_sys_2 <- paste0(tolower(tblid), "_sys_2", ".png")

png(
  write_path(opath, pname_sys_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_sys_2)) ### print png path and name in log
print(pt_sys_2)
dev.off()

pname_sys_3 <- paste0(tolower(tblid), "_sys_3", ".png")

png(
  write_path(opath, pname_sys_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_sys_3)) ### print png path and name in log
print(pt_sys_3)
dev.off()


pname_dia_1 <- paste0(tolower(tblid), "_dia_1", ".png")

png(
  write_path(opath, pname_dia_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_dia_1)) ### print png path and name in log
print(pt_dia_1)
dev.off()

pname_dia_2 <- paste0(tolower(tblid), "_dia_2", ".png")

png(
  write_path(opath, pname_dia_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_dia_2)) ### print png path and name in log
print(pt_dia_2)
dev.off()

pname_dia_3 <- paste0(tolower(tblid), "_dia_3", ".png")

png(
  write_path(opath, pname_dia_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_dia_3)) ### print png path and name in log
print(pt_dia_3)
dev.off()

tidytlg::gentlg(
  tlf = "g",
  plotnames = write_path(
    opath,
    c(
      pname_sys_1,
      pname_sys_2,
      pname_sys_3,
      pname_dia_1,
      pname_dia_2,
      pname_dia_3
    )
  ),
  plotwidth = 8,
  orientation = "landscape",
  opath = write_path(opath),
  file = tblid,
  title = title_footer$title,
  footers = title_footer$main_footer
)
