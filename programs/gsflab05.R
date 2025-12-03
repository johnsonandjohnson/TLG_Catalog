################################################################
##### programs_external/gsflab05.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/gsflab05.R
################################################################


################################################################################
## Original Reporting Effort: Standards
## Program Name:              gsflab05.R
## R version:                 4.2.1
## Short Description:         Proportion of Subjects Remaining With Missing and Existing Data
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      March 14, 2024
## Input:
## Output:
## Remarks:                   Include parameters: Liver Function laboratory tests-ALT,AST,ALP,BILI,INR,CREAT
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

tblid <- "GSFLAB05"

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
  df <- pharmaverseadamjnj::adlb %>%
    filter(SAFFL == "Y" & ANL02FL == "Y") %>%
    filter(!grepl("Unscheduled", AVISIT)) %>%
    filter(!(AVISIT %in% c("Screening", "Cycle 01", "Endpoint"))) %>%
    mutate(
      # shorten "End of Treatment" as EOT
      AVISIT = forcats::fct_recode(AVISIT, "EOT" = "End Of Treatment"),

      # add abbreviation to TRT01A
      TRT01A = forcats::fct_recode(
        TRT01A,
        "Xanomeline (High)" = "Xanomeline High Dose",
        "Xanomeline (Low)" = "Xanomeline Low Dose",
        "Placebo (PBO)" = "Placebo"
      )
    )

  df_1 <- df %>%
    filter(PARAMCD == param_cd) %>%
    group_by(TRT01A, AVISITN, AVISIT) %>%
    # subjects with existing data for parameter: LBSTRESC^=.
    # assume each subject collecting only one record in each time point
    summarize(n = length(!is.na(LBSTRESC))) %>%
    mutate(type = "with_data") %>%
    ungroup()

  df_2 <- df %>%
    group_by(TRT01A, AVISITN, AVISIT) %>%
    # subjects remaining with missing data for parameter
    # assume subjects collecting any lab tests in each time points remaining in study
    summarize(n = n_distinct(USUBJID)) %>%
    mutate(type = "in_trial") %>%
    ungroup()

  df_3 <- df_1 %>%
    select(-type) %>%
    rename(e = n) %>%
    right_join(df_2) %>%
    # calculate subjects in trial but no data to be presented as white in bar
    mutate(n = n - e) %>%
    select(-e)

  df <- bind_rows(df_1, df_3) %>%
    mutate(visit = as.numeric(factor(AVISIT))) %>%
    select(TRT01A, AVISIT, visit, type, n) %>%
    # fill in missing values for time points which don't have data
    tidyr::complete(
      TRT01A,
      tidyr::nesting(AVISIT, visit, type),
      fill = list(n = 0)
    )

  # get total subjects per each treatment group
  adsl <- pharmaverseadamjnj::adsl %>%
    filter(SAFFL == "Y") %>%
    mutate(
      TRT01A = forcats::fct_recode(
        TRT01A,
        "Xanomeline (High)" = "Xanomeline High Dose",
        "Xanomeline (Low)" = "Xanomeline Low Dose",
        "Placebo (PBO)" = "Placebo"
      )
    ) %>%
    group_by(TRT01A) %>%
    summarize(total = n()) %>%
    ungroup()

  df <- df %>%
    left_join(adsl, by = "TRT01A") %>%
    # merge in total subjects to calculate percent of subjects
    mutate(n = tidytlg::roundSAS(n / total * 100), digits = 0) %>%
    select(-total, -digits)

  df2 <- bind_rows(df_1, df_2) %>%
    mutate(visit = as.numeric(factor(AVISIT))) %>%
    tidyr::pivot_wider(
      names_from = type,
      values_from = n
    ) %>%
    # fill in missing values for time points which don't have data
    tidyr::complete(
      TRT01A,
      tidyr::nesting(AVISIT, visit),
      fill = list(with_data = 0, in_trial = 0)
    ) %>%
    mutate(prop = paste0(with_data, "/", in_trial)) %>%
    select(TRT01A, AVISIT, visit, prop) %>%
    # reverse treatment group order: present from top to bottom in table
    mutate(trt_rev = forcats::fct_relevel(TRT01A, rev(levels(TRT01A))))

  list(df, df2)
}


# generate data set for ALT
adlb_alt <- dt("ALT")
adlb_alt_pt <- adlb_alt[[1]]
adlb_alt_tb <- adlb_alt[[2]]


# split data in two graphics
# AVISIT has been converted as numeric value "visit" for plotting purpose
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
  "Cycle 33",
  "Cycle 37",
  "EOT"
)

adlb_alt_pt_1 <- adlb_alt_pt %>%
  filter(AVISIT %in% time_point_1) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_alt_pt_2 <- adlb_alt_pt %>%
  filter(AVISIT %in% time_point_2) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_alt_pt_3 <- adlb_alt_pt %>%
  filter(AVISIT %in% time_point_3) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_alt_tb_1 <- adlb_alt_tb %>%
  filter(AVISIT %in% time_point_1)

adlb_alt_tb_2 <- adlb_alt_tb %>%
  filter(AVISIT %in% time_point_2)

adlb_alt_tb_3 <- adlb_alt_tb %>%
  filter(AVISIT %in% time_point_3)

# generate data set for AST
adlb_ast <- dt("AST")
adlb_ast_pt <- adlb_ast[[1]]
adlb_ast_tb <- adlb_ast[[2]]

adlb_ast_pt_1 <- adlb_ast_pt %>%
  filter(AVISIT %in% time_point_1) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_ast_pt_2 <- adlb_ast_pt %>%
  filter(AVISIT %in% time_point_2) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_ast_pt_3 <- adlb_ast_pt %>%
  filter(AVISIT %in% time_point_3) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_ast_tb_1 <- adlb_ast_tb %>%
  filter(AVISIT %in% time_point_1)

adlb_ast_tb_2 <- adlb_ast_tb %>%
  filter(AVISIT %in% time_point_2)

adlb_ast_tb_3 <- adlb_ast_tb %>%
  filter(AVISIT %in% time_point_3)

# generate data set for ALP
adlb_alp <- dt("ALP")
adlb_alp_pt <- adlb_alp[[1]]
adlb_alp_tb <- adlb_alp[[2]]

adlb_alp_pt_1 <- adlb_alp_pt %>%
  filter(AVISIT %in% time_point_1) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_alp_pt_2 <- adlb_alp_pt %>%
  filter(AVISIT %in% time_point_2) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_alp_pt_3 <- adlb_alp_pt %>%
  filter(AVISIT %in% time_point_3) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_alp_tb_1 <- adlb_alp_tb %>%
  filter(AVISIT %in% time_point_1)

adlb_alp_tb_2 <- adlb_alp_tb %>%
  filter(AVISIT %in% time_point_2)

adlb_alp_tb_3 <- adlb_alp_tb %>%
  filter(AVISIT %in% time_point_3)

# generate data set for BILI
adlb_bili <- dt("BILI")
adlb_bili_pt <- adlb_bili[[1]]
adlb_bili_tb <- adlb_bili[[2]]

adlb_bili_pt_1 <- adlb_bili_pt %>%
  filter(AVISIT %in% time_point_1) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_bili_pt_2 <- adlb_bili_pt %>%
  filter(AVISIT %in% time_point_2) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_bili_pt_3 <- adlb_bili_pt %>%
  filter(AVISIT %in% time_point_3) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_bili_tb_1 <- adlb_bili_tb %>%
  filter(AVISIT %in% time_point_1)

adlb_bili_tb_2 <- adlb_bili_tb %>%
  filter(AVISIT %in% time_point_2)

adlb_bili_tb_3 <- adlb_bili_tb %>%
  filter(AVISIT %in% time_point_3)

# generate data set for INR
adlb_inr <- dt("ALP")
adlb_inr_pt <- adlb_inr[[1]]
adlb_inr_tb <- adlb_inr[[2]]

adlb_inr_pt_1 <- adlb_inr_pt %>%
  filter(AVISIT %in% time_point_1) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_inr_pt_2 <- adlb_inr_pt %>%
  filter(AVISIT %in% time_point_2) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_inr_pt_3 <- adlb_inr_pt %>%
  filter(AVISIT %in% time_point_3) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_inr_tb_1 <- adlb_inr_tb %>%
  filter(AVISIT %in% time_point_1)

adlb_inr_tb_2 <- adlb_inr_tb %>%
  filter(AVISIT %in% time_point_2)

adlb_inr_tb_3 <- adlb_inr_tb %>%
  filter(AVISIT %in% time_point_3)

# generate data set for CREAT
adlb_creat <- dt("CREAT")
adlb_creat_pt <- adlb_creat[[1]]
adlb_creat_tb <- adlb_creat[[2]]

adlb_creat_pt_1 <- adlb_creat_pt %>%
  filter(AVISIT %in% time_point_1) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_creat_pt_2 <- adlb_creat_pt %>%
  filter(AVISIT %in% time_point_2) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_creat_pt_3 <- adlb_creat_pt %>%
  filter(AVISIT %in% time_point_3) %>%
  select(-AVISIT) %>%
  tidyr::nest(data = c(n, type, visit))

adlb_creat_tb_1 <- adlb_creat_tb %>%
  filter(AVISIT %in% time_point_1)

adlb_creat_tb_2 <- adlb_creat_tb %>%
  filter(AVISIT %in% time_point_2)

adlb_creat_tb_3 <- adlb_creat_tb %>%
  filter(AVISIT %in% time_point_3)


################################################################################
# Generate plot:
################################################################################

# define function for creating the body of graphic:

g_bar <- function(df, df2) {
  plot <- ggplot() +
    geom_bar(
      data = df$data[[1]],
      aes(
        x = .data$visit,
        y = .data$n,
        fill = factor(
          ifelse(.data$type == "with_data", "Xanomeline (High)", "InTrial"),
          levels = c("InTrial", "Xanomeline (High)")
        )
      ),
      position = "stack",
      stat = "identity",
      width = barwidth,
      color = "black"
    ) +
    geom_bar(
      data = df$data[[2]],
      aes(
        x = .data$visit + barwidth,
        y = .data$n,
        fill = factor(
          ifelse(.data$type == "with_data", "Xanomeline (Low)", "InTrial"),
          levels = c("InTrial", "Xanomeline (Low)")
        )
      ),
      position = "stack",
      stat = "identity",
      width = barwidth,
      color = "black"
    ) +
    geom_bar(
      data = df$data[[3]],
      aes(
        x = .data$visit + barwidth + barwidth,
        y = .data$n,
        fill = factor(
          ifelse(.data$type == "with_data", "Placebo (PBO)", "InTrial"),
          levels = c("InTrial", "Placebo (PBO)")
        )
      ),
      position = "stack",
      stat = "identity",
      width = barwidth,
      color = "black"
    ) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +

    # assign colorblind friendly palette to treatment groups: black(Xan Low), orange(Xan High), dark blue(PBO)
    scale_fill_manual(
      values = c(
        "Xanomeline (High)" = "#000000",
        "Xanomeline (Low)" = "#E69F00",
        "Placebo (PBO)" = "#0072B2",
        "InTrial" = "#FFFFFF"
      ),
      breaks = c("Xanomeline (High)", "Xanomeline (Low)", "Placebo (PBO)")
    ) +
    labs(x = x_label, y = y_label) +
    theme_bw() +
    theme(
      text = element_text(size = 9, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9, face = "bold")
    )

  ## get x limit from the plot so they match the table
  built_plot <- ggplot_build(plot)
  plot_limit <- built_plot$layout$panel_params[[1]]$x$limits

  df2 <- df2 %>%
    mutate(visit = visit + barwidth)

  table_prop <- df2 %>%
    ggplot(aes(x = .data$visit, y = .data$trt_rev, label = .data$prop)) +
    geom_text(size = 2.5) +

    # abbreviate table text label
    scale_y_discrete(labels = table_text) +
    coord_cartesian(xlim = plot_limit) +
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
    labs(
      title = "Number of Subjects With Data (Solid Bar)/Number of Subjects Remaining in Study (Open Bar)"
    )

  final <- plot /
    table_prop +
    plot_layout(heights = c(7.5, 1.5)) +
    plot_annotation(
      title = param_title,
      theme = theme(plot.title = element_text(size = 9))
    )
}


# define parameters for plotting:
# present treatment group in y axis table from top to bottom (reverse)
table_text <- c("PBO", "Xan High", "Xan Low")

x_label <- ""
y_label <- "Percent of Subjects"

barwidth <- 0.3

y_breaks <- seq(0, 100, by = 25)
y_limits <- c(0, 100)


# ALT
param_title <- "Laboratory Test: Alanine Aminotransferase (U/L)"

# call for part 1
x_breaks <- c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3, 7.3)
x_labels <- time_point_1
pt_alt_1 <- g_bar(
  df = adlb_alt_pt_1,
  df2 = adlb_alt_tb_1
)

# call for part 2
x_breaks <- c(8.3, 9.3, 10.3, 11.3, 12.3, 13.3, 14.3)
x_labels <- time_point_2
pt_alt_2 <- g_bar(
  df = adlb_alt_pt_2,
  df2 = adlb_alt_tb_2
)

# call for part 3
x_breaks <- c(15.3, 16.3, 17.3, 18.3, 19.3, 20.3, 21.3, 22.3, 23.3)
x_labels <- time_point_3
pt_alt_3 <- g_bar(
  df = adlb_alt_pt_3,
  df2 = adlb_alt_tb_3
)


# AST
param_title <- "Laboratory Test: Aspartate Aminotransferase (U/L)"

# call for part 1
x_breaks <- c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3, 7.3)
x_labels <- time_point_1
pt_ast_1 <- g_bar(
  df = adlb_ast_pt_1,
  df2 = adlb_ast_tb_1
)

# call for part 2
x_breaks <- c(8.3, 9.3, 10.3, 11.3, 12.3, 13.3, 14.3)
x_labels <- time_point_2
pt_ast_2 <- g_bar(
  df = adlb_ast_pt_2,
  df2 = adlb_ast_tb_2
)

# call for part 3
x_breaks <- c(15.3, 16.3, 17.3, 18.3, 19.3, 20.3, 21.3, 22.3, 23.3)
x_labels <- time_point_3
pt_ast_3 <- g_bar(
  df = adlb_ast_pt_3,
  df2 = adlb_ast_tb_3
)


# ALP
param_title <- "Laboratory Test: Alkaline Phosphatase (U/L)"

# call for part 1
x_breaks <- c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3, 7.3)
x_labels <- time_point_1
pt_alp_1 <- g_bar(
  df = adlb_alp_pt_1,
  df2 = adlb_alp_tb_1
)

# call for part 2
x_breaks <- c(8.3, 9.3, 10.3, 11.3, 12.3, 13.3, 14.3)
x_labels <- time_point_2
pt_alp_2 <- g_bar(
  df = adlb_alp_pt_2,
  df2 = adlb_alp_tb_2
)

# call for part 3
x_breaks <- c(15.3, 16.3, 17.3, 18.3, 19.3, 20.3, 21.3, 22.3, 23.3)
x_labels <- time_point_3
pt_alp_3 <- g_bar(
  df = adlb_alp_pt_3,
  df2 = adlb_alp_tb_3
)


# BILI
param_title <- "Laboratory Test: Bilirubin (umol/L)"

# call for part 1
x_breaks <- c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3, 7.3)
x_labels <- time_point_1
pt_bili_1 <- g_bar(
  df = adlb_bili_pt_1,
  df2 = adlb_bili_tb_1
)

# call for part 2
x_breaks <- c(8.3, 9.3, 10.3, 11.3, 12.3, 13.3, 14.3)
x_labels <- time_point_2
pt_bili_2 <- g_bar(
  df = adlb_bili_pt_2,
  df2 = adlb_bili_tb_2
)

# call for part 3
x_breaks <- c(15.3, 16.3, 17.3, 18.3, 19.3, 20.3, 21.3, 22.3, 23.3)
x_labels <- time_point_3
pt_bili_3 <- g_bar(
  df = adlb_bili_pt_3,
  df2 = adlb_bili_tb_3
)


# INR
param_title <- "Laboratory Test: Prothrombin Intl. Normalized Ratio (RATIO)"

# call for part 1
x_breaks <- c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3, 7.3)
x_labels <- time_point_1
pt_inr_1 <- g_bar(
  df = adlb_inr_pt_1,
  df2 = adlb_inr_tb_1
)
# call for part 2
x_breaks <- c(8.3, 9.3, 10.3, 11.3, 12.3, 13.3, 14.3)
x_labels <- time_point_2
pt_inr_2 <- g_bar(
  df = adlb_inr_pt_2,
  df2 = adlb_inr_tb_2
)

# call for part 3
x_breaks <- c(15.3, 16.3, 17.3, 18.3, 19.3, 20.3, 21.3, 22.3, 23.3)
x_labels <- time_point_3
pt_inr_3 <- g_bar(
  df = adlb_inr_pt_3,
  df2 = adlb_inr_tb_3
)


# CREATININE
param_title <- "Laboratory Test: Creatinine (umol/L)"

# call for part 1
x_breaks <- c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3, 7.3)
x_labels <- time_point_1
pt_creat_1 <- g_bar(
  df = adlb_creat_pt_1,
  df2 = adlb_creat_tb_1
)

# call for part 2
x_breaks <- c(8.3, 9.3, 10.3, 11.3, 12.3, 13.3, 14.3)
x_labels <- time_point_2
pt_creat_2 <- g_bar(
  df = adlb_creat_pt_2,
  df2 = adlb_creat_tb_2
)

# call for part 3
x_breaks <- c(15.3, 16.3, 17.3, 18.3, 19.3, 20.3, 21.3, 22.3, 23.3)
x_labels <- time_point_3
pt_creat_3 <- g_bar(
  df = adlb_creat_pt_3,
  df2 = adlb_creat_tb_3
)


################################################################################
# Create png and output file:
################################################################################

# create png file and output figure
pname_alt_1 <- paste0(tolower(tblid), "_alt_1", ".png")

png(
  write_path(opath, pname_alt_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_alt_1)) ### print png path and name in log
print(pt_alt_1)
dev.off()

pname_alt_2 <- paste0(tolower(tblid), "_alt_2", ".png")

png(
  write_path(opath, pname_alt_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_alt_2)) ### print png path and name in log
print(pt_alt_2)
dev.off()

pname_alt_3 <- paste0(tolower(tblid), "_alt_3", ".png")

png(
  write_path(opath, pname_alt_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_alt_3)) ### print png path and name in log
print(pt_alt_3)
dev.off()


pname_ast_1 <- paste0(tolower(tblid), "_ast_1", ".png")

png(
  write_path(opath, pname_ast_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_ast_1)) ### print png path and name in log
print(pt_ast_1)
dev.off()

pname_ast_2 <- paste0(tolower(tblid), "_ast_2", ".png")

png(
  write_path(opath, pname_ast_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_ast_2)) ### print png path and name in log
print(pt_ast_2)
dev.off()

pname_ast_3 <- paste0(tolower(tblid), "_ast_3", ".png")

png(
  write_path(opath, pname_ast_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_ast_3)) ### print png path and name in log
print(pt_ast_3)
dev.off()


pname_alp_1 <- paste0(tolower(tblid), "_alp_1", ".png")

png(
  write_path(opath, pname_alp_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_alp_1)) ### print png path and name in log
print(pt_alp_1)
dev.off()

pname_alp_2 <- paste0(tolower(tblid), "_alp_2", ".png")

png(
  write_path(opath, pname_alp_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_alp_2)) ### print png path and name in log
print(pt_alp_2)
dev.off()

pname_alp_3 <- paste0(tolower(tblid), "_alp_3", ".png")

png(
  write_path(opath, pname_alp_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_alp_3)) ### print png path and name in log
print(pt_alp_3)
dev.off()


pname_bili_1 <- paste0(tolower(tblid), "_bili_1", ".png")

png(
  write_path(opath, pname_bili_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_bili_1)) ### print png path and name in log
print(pt_bili_1)
dev.off()

pname_bili_2 <- paste0(tolower(tblid), "_bili_2", ".png")

png(
  write_path(opath, pname_bili_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_bili_2)) ### print png path and name in log
print(pt_bili_2)
dev.off()

pname_bili_3 <- paste0(tolower(tblid), "_bili_3", ".png")

png(
  write_path(opath, pname_bili_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_bili_3)) ### print png path and name in log
print(pt_bili_3)
dev.off()


pname_inr_1 <- paste0(tolower(tblid), "_inr_1", ".png")

png(
  write_path(opath, pname_inr_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_inr_1)) ### print png path and name in log
print(pt_inr_1)
dev.off()

pname_inr_2 <- paste0(tolower(tblid), "_inr_2", ".png")

png(
  write_path(opath, pname_inr_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_inr_2)) ### print png path and name in log
print(pt_inr_2)
dev.off()

pname_inr_3 <- paste0(tolower(tblid), "_inr_3", ".png")

png(
  write_path(opath, pname_inr_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_inr_3)) ### print png path and name in log
print(pt_inr_3)
dev.off()


pname_creat_1 <- paste0(tolower(tblid), "_creat_1", ".png")

png(
  write_path(opath, pname_creat_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_creat_1)) ### print png path and name in log
print(pt_creat_1)
dev.off()

pname_creat_2 <- paste0(tolower(tblid), "_creat_2", ".png")

png(
  write_path(opath, pname_creat_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_creat_2)) ### print png path and name in log
print(pt_creat_2)
dev.off()

pname_creat_3 <- paste0(tolower(tblid), "_creat_3", ".png")

png(
  write_path(opath, pname_creat_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_creat_3)) ### print png path and name in log
print(pt_creat_3)
dev.off()


tidytlg::gentlg(
  tlf = "g",
  plotnames = write_path(
    opath,
    c(
      pname_alt_1,
      pname_alt_2,
      pname_alt_3,
      pname_ast_1,
      pname_ast_2,
      pname_ast_3,
      pname_alp_1,
      pname_alp_2,
      pname_alp_3,
      pname_bili_1,
      pname_bili_2,
      pname_bili_3,
      pname_inr_1,
      pname_inr_2,
      pname_inr_3,
      pname_creat_1,
      pname_creat_2,
      pname_creat_3
    )
  ),
  plotwidth = 8,
  orientation = "landscape",
  opath = write_path(opath),
  file = tblid,
  title = title_footer$title,
  footers = title_footer$main_footer
)
