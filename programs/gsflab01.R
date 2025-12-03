################################################################
##### programs_external/gsflab01.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/gsflab01.R
################################################################


################################################################################
## Original Reporting Effort: Standards
## Program Name:              gsflab01.R
## R version:                 4.2.1
## Short Description:         Mean Change From Baseline for [Laboratory Category] Laboratory Data Over Time
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      April 11, 2024
## Input:
## Output:
## Remarks:                   Include categories: GC, KF, LV, LP, HM
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

tblid <- "GSFLAB01"

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
  adlb <- pharmaverseadamjnj::adlb %>%
    filter(
      PARAMCD == param_cd,
      !is.na(PARCAT3),
      ANL02FL == "Y",
      SAFFL == "Y"
    ) %>%
    filter(!grepl("Unscheduled", AVISIT)) %>%
    filter(!(AVISIT %in% c("Endpoint", "Screening"))) %>%
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
    ) %>%
    group_by(TRT01A, AVISITN, AVISIT) %>%
    summarize(
      n = n(),
      m = mean(AVAL, na.rm = TRUE),
      mean = mean(CHG, na.rm = TRUE),
      sd = sd(CHG, na.rm = TRUE),
      se = sd(CHG, na.rm = TRUE) / sqrt(n())
    ) %>%
    ungroup() %>%
    # fill in missing values for time points which don't have data
    tidyr::complete(
      TRT01A,
      tidyr::nesting(AVISIT),
      fill = list(n = 0, m = 0.0, mean = 0.0, sd = 0.0, se = 0.0)
    ) %>%
    # filling zero in decimal place for table display
    # mutate(meanC = sprintf('%.1f', mean)) %>%
    mutate(
      meanC = paste0(
        tidytlg::roundSAS(mean, digits = 1, as_char = TRUE),
        "/",
        tidytlg::roundSAS(m, digits = 1, as_char = TRUE)
      )
    ) %>%
    # reverse treatment group order: present from top to bottom in table
    mutate(trt_rev = forcats::fct_relevel(TRT01A, rev(levels(TRT01A)))) %>%
    # assing EOT as NA (avoid to connect line to last time point)
    mutate(new_avisit = na_if(AVISIT, "EOT"))

  # Include time points only for subjects at least 10% left
  avisit_n <- adlb %>%
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

  adlb <- adlb %>%
    filter(AVISIT %in% time_point)
}


# split data in two graphics
time_point_1 <- c(
  "Baseline",
  "Cycle 02",
  "Cycle 03",
  "Cycle 04",
  "Cycle 05",
  "Cycle 06"
)

time_point_2 <- c(
  "Cycle 07",
  "Cycle 08",
  "Cycle 09",
  "Cycle 10",
  "Cycle 11",
  "Cycle 12",
  "Cycle 13"
)

time_point_3 <- c(
  "Cycle 15",
  "Cycle 17",
  "Cycle 19",
  "Cycle 21",
  "Cycle 23",
  "Cycle 25",
  "Cycle 29",
  "EOT"
)


# list of laboratory tests
lb_param <- c(
  "SODIUM",
  "K",
  "GLUC",
  "CA",
  "PROT",
  "ALB",
  "CREAT",
  "ALP",
  "ALT",
  "AST",
  "BILI",
  "LDL",
  "WBC",
  "HGB",
  "PLAT",
  "NEUT"
)


# generate split data sets

adlb1 <- list()
adlb2 <- list()
adlb3 <- list()

for (param in lb_param) {
  df <- dt(param)

  adlb1[[param]] <- df %>%
    filter(AVISIT %in% time_point_1)

  adlb2[[param]] <- df %>%
    filter(AVISIT %in% time_point_2)

  adlb3[[param]] <- df %>%
    filter(AVISIT %in% time_point_3)
}


# generate data sets for below of parameters
# only 4 time points - baseline, cycle 12, cycle 25, cycle 29
adlb_chol <- dt("CHOL")
adlb_hdl <- dt("HDL")
adlb_trig <- dt("TRIG")

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
    geom_line(
      data = df[!is.na(df$new_avisit), ],
      aes(x = new_avisit, y = mean),
      position = pd
    ) +
    geom_point(position = pd) +
    # Use scales package for automatic breaks calculation
    scale_y_continuous(
      breaks = scales::extended_breaks(n = 5),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +

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
    labs(title = "Change From Baseline/Mean Value")

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

x_label <- ""

# General chemistry *************************
# Sodium (mmol/L) - SODIUM ---------
y_label <- "Mean Change From Baseline (95% CI) \n Sodium (mmol/L)"
param_title <- "Labortary test: Sodium (mmol/L)"

pt1_sodium <- g_line(df = adlb1$SODIUM)
pt2_sodium <- g_line(df = adlb2$SODIUM)
pt3_sodium <- g_line(df = adlb3$SODIUM)

# Potassium (mmol/L) - K ------------
y_label <- "Mean Change From Baseline (95% CI) \n Potassium (mmol/L)"
param_title <- "Labortary test: Potassium (mmol/L)"

pt1_k <- g_line(df = adlb1$K)
pt2_k <- g_line(df = adlb2$K)
pt3_k <- g_line(df = adlb3$K)

# Glucose (mmol/L) - GLUC ------------
y_label <- "Mean Change From Baseline (95% CI) \n Glucose (mmol/L)"
param_title <- "Labortary test: Glucose (mmol/L)"

# too few subjects
# pt1_gluc <- g_line(df = adlb1$GLUC)
# pt2_gluc <- g_line(df = adlb2$GLUC)
# pt3_gluc <- g_line(df = adlb3$GLUC)

# Calcium (mmol/L) - CA ------------
y_label <- "Mean Change From Baseline (95% CI) \n Calcium (mmol/L)"
param_title <- "Labortary test: Calcium (mmol/L)"

pt1_ca <- g_line(df = adlb1$CA)
pt2_ca <- g_line(df = adlb2$CA)
pt3_ca <- g_line(df = adlb3$CA)

# Protein (g/L) - PROT ------------
y_label <- "Mean Change From Baseline (95% CI) \n Protein (g/L)"
param_title <- "Labortary test: Protein (g/L)"

pt1_prot <- g_line(df = adlb1$PROT)
pt2_prot <- g_line(df = adlb2$PROT)
pt3_prot <- g_line(df = adlb3$PROT)

# Albumin (g/L) - ALB -------------
y_label <- "Mean Change From Baseline (95% CI) \n Albumin (g/L)"
param_title <- "Labortary test: Albumin (g/L)"

# too few subjects
# pt1_alb <- g_line(df = adlb1$ALB)
# pt2_alb <- g_line(df = adlb2$ALB)
# pt3_alb <- g_line(df = adlb3$ALB)

# Kidney function ****************************
# Creatinine (umol/L) - CREAT -------------
y_label <- "Mean Change From Baseline (95% CI) \n Creatinine (umol/L)"
param_title <- "Labortary test: Creatinine (umol/L)"

pt1_creat <- g_line(df = adlb1$CREAT)
pt2_creat <- g_line(df = adlb2$CREAT)
pt3_creat <- g_line(df = adlb3$CREAT)

# Liver biochemistry *************************
# Alkaline Phosphatase (U/L) - ALP ---------
y_label <- "Mean Change From Baseline (95% CI) \n Alkaline Phosphatase (U/L)"
param_title <- "Labortary test: Alkaline Phosphatase (U/L)"

pt1_alp <- g_line(df = adlb1$ALP)
pt2_alp <- g_line(df = adlb2$ALP)
pt3_alp <- g_line(df = adlb3$ALP)

# Alanine Aminotransferase (U/L) - ALT -----
y_label <- "Mean Change From Baseline (95% CI) \n Alanine Aminotransferase (U/L)"
param_title <- "Labortary test: Alanine Aminotransferase (U/L)"

pt1_alt <- g_line(df = adlb1$ALT)
pt2_alt <- g_line(df = adlb2$ALT)
pt3_alt <- g_line(df = adlb3$ALT)

# Aspartate Aminotransferase (U/L) - AST ---
y_label <- "Mean Change From Baseline (95% CI) \n Aspartate Aminotransferase (U/L)"
param_title <- "Labortary test: Aspartate Aminotransferase (U/L)"

pt1_ast <- g_line(df = adlb1$AST)
pt2_ast <- g_line(df = adlb2$AST)
pt3_ast <- g_line(df = adlb3$AST)

# Bilirubin (umol/L) - BILI ----------------
y_label <- "Mean Change From Baseline (95% CI) \n Bilirubin (umol/L)"
param_title <- "Labortary test: Bilirubin (umol/L)"

pt1_bili <- g_line(df = adlb1$BILI)
pt2_bili <- g_line(df = adlb2$BILI)
pt3_bili <- g_line(df = adlb3$BILI)

# Lipids *************************************
# Cholesterol (mmol/L) - CHOL -------------
y_label <- "Mean Change From Baseline (95% CI) \n Cholesterol (mmol/L)"
param_title <- "Labortary test: Cholesterol (mmol/L)"

pt_chol <- g_line(df = adlb_chol)

# HDL Cholesterol (mmol/L) - HDL ----------
y_label <- "Mean Change From Baseline (95% CI) \n HDL Cholesterol (mmol/L)"
param_title <- "Labortary test: HDL Cholesterol (mmol/L)"

pt_hdl <- g_line(df = adlb_hdl)

# LDL Cholesterol (mmol/L) - LDL ----------
y_label <- "Mean Change From Baseline (95% CI) \n LDL Cholesterol (mmol/L)"
param_title <- "Labortary test: LDL Cholesterol (mmol/L)"

# too few subjects
# pt1_ldl <- g_line(df = adlb1$LDL)
# pt2_ldl <- g_line(df = adlb2$LDL)
# pt3_ldl <- g_line(df = adlb3$LDL)

# Triglycerides (mmol/L) - TRIG -----------
y_label <- "Mean Change From Baseline (95% CI) \n Triglycerides (mmol/L)"
param_title <- "Labortary test: Triglycerides (mmol/L)"

pt_trig <- g_line(df = adlb_trig)


# Complete blood count ************************
# Leukocytes (x10E9/L) - WBC -------------
y_label <- "Mean Change From Baseline (95% CI) \n Leukocytes (x10E9/L)"
param_title <- "Labortary test: Leukocytes (x10E9/L)"

pt1_wbc <- g_line(df = adlb1$WBC)
pt2_wbc <- g_line(df = adlb2$WBC)
pt3_wbc <- g_line(df = adlb3$WBC)

# Hemoglobin (g/L) - HGB -----------------
y_label <- "Mean Change From Baseline (95% CI) \n Hemoglobin (g/L)"
param_title <- "Labortary test: Hemoglobin (g/L)"

pt1_hgb <- g_line(df = adlb1$HGB)
pt2_hgb <- g_line(df = adlb2$HGB)
pt3_hgb <- g_line(df = adlb3$HGB)

# Platelets (x10E9/L) - PLAT -------------
y_label <- "Mean Change From Baseline (95% CI) \n Platelets (x10E9/L)"
param_title <- "Labortary test: Platelets (x10E9/L)"

pt1_plat <- g_line(df = adlb1$PLAT)
pt2_plat <- g_line(df = adlb2$PLAT)
pt3_plat <- g_line(df = adlb3$PLAT)

# WBC differential ***************************
# Neutrophils (x10E9/L) - NEUT -----------
y_label <- "Mean Change From Baseline (95% CI) \n Neutrophils (x10E9/L)"
param_title <- "Labortary test: Neutrophils (x10E9/L)"

# too few subjects
# pt1_neut <- g_line(df = adlb1$NEUT)
# pt2_neut <- g_line(df = adlb2$NEUT)
# pt3_neut <- g_line(df = adlb3$NEUT)

################################################################################
# Create png and output file:
################################################################################

# create png file and output figure
pname_sodium_1 <- paste0(tolower(tblid), "_sodium_1", ".png")

png(
  write_path(opath, pname_sodium_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_sodium_1)) ### print png path and name in log
print(pt1_sodium)
dev.off()

pname_sodium_2 <- paste0(tolower(tblid), "_sodium_2", ".png")

png(
  write_path(opath, pname_sodium_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_sodium_2)) ### print png path and name in log
print(pt2_sodium)
dev.off()

pname_sodium_3 <- paste0(tolower(tblid), "_sodium_3", ".png")

png(
  write_path(opath, pname_sodium_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_sodium_3)) ### print png path and name in log
print(pt3_sodium)
dev.off()

pname_k_1 <- paste0(tolower(tblid), "_k_1", ".png")

png(
  write_path(opath, pname_k_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_k_1)) ### print png path and name in log
print(pt1_k)
dev.off()

pname_k_2 <- paste0(tolower(tblid), "_k_2", ".png")

png(
  write_path(opath, pname_k_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_k_2)) ### print png path and name in log
print(pt2_k)
dev.off()

pname_k_3 <- paste0(tolower(tblid), "_k_3", ".png")

png(
  write_path(opath, pname_k_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_k_3)) ### print png path and name in log
print(pt3_k)
dev.off()

pname_ca_1 <- paste0(tolower(tblid), "_ca_1", ".png")

png(
  write_path(opath, pname_ca_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_ca_1)) ### print png path and name in log
print(pt1_ca)
dev.off()

pname_ca_2 <- paste0(tolower(tblid), "_ca_2", ".png")

png(
  write_path(opath, pname_ca_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_ca_2)) ### print png path and name in log
print(pt2_ca)
dev.off()

pname_ca_3 <- paste0(tolower(tblid), "_ca_3", ".png")

png(
  write_path(opath, pname_ca_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_ca_3)) ### print png path and name in log
print(pt3_ca)
dev.off()

pname_prot_1 <- paste0(tolower(tblid), "_prot_1", ".png")

png(
  write_path(opath, pname_prot_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_prot_1)) ### print png path and name in log
print(pt1_prot)
dev.off()

pname_prot_2 <- paste0(tolower(tblid), "_prot_2", ".png")

png(
  write_path(opath, pname_prot_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_prot_2)) ### print png path and name in log
print(pt2_prot)
dev.off()

pname_prot_3 <- paste0(tolower(tblid), "_prot_3", ".png")

png(
  write_path(opath, pname_prot_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_prot_3)) ### print png path and name in log
print(pt3_prot)
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
print(pt1_creat)
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
print(pt2_creat)
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
print(pt3_creat)
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
print(pt1_alp)
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
print(pt2_alp)
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
print(pt3_alp)
dev.off()

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
print(pt1_alt)
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
print(pt2_alt)
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
print(pt3_alt)
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
print(pt1_ast)
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
print(pt2_ast)
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
print(pt3_ast)
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
print(pt1_bili)
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
print(pt2_bili)
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
print(pt3_bili)
dev.off()

pname_chol <- paste0(tolower(tblid), "_chol", ".png")

png(
  write_path(opath, pname_chol),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_chol)) ### print png path and name in log
print(pt_chol)
dev.off()

pname_hdl <- paste0(tolower(tblid), "_hdl", ".png")

png(
  write_path(opath, pname_hdl),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_hdl)) ### print png path and name in log
print(pt_hdl)
dev.off()

pname_trig <- paste0(tolower(tblid), "_trig", ".png")

png(
  write_path(opath, pname_trig),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_trig)) ### print png path and name in log
print(pt_trig)
dev.off()

pname_wbc_1 <- paste0(tolower(tblid), "_wbc_1", ".png")

png(
  write_path(opath, pname_wbc_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_wbc_1)) ### print png path and name in log
print(pt1_wbc)
dev.off()

pname_wbc_2 <- paste0(tolower(tblid), "_wbc_2", ".png")

png(
  write_path(opath, pname_wbc_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_wbc_2)) ### print png path and name in log
print(pt2_wbc)
dev.off()

pname_wbc_3 <- paste0(tolower(tblid), "_wbc_3", ".png")

png(
  write_path(opath, pname_wbc_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_wbc_3)) ### print png path and name in log
print(pt3_wbc)
dev.off()

pname_hgb_1 <- paste0(tolower(tblid), "_hgb_1", ".png")

png(
  write_path(opath, pname_hgb_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_hgb_1)) ### print png path and name in log
print(pt1_hgb)
dev.off()

pname_hgb_2 <- paste0(tolower(tblid), "_hgb_2", ".png")

png(
  write_path(opath, pname_hgb_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_hgb_2)) ### print png path and name in log
print(pt2_hgb)
dev.off()

pname_hgb_3 <- paste0(tolower(tblid), "_hgb_3", ".png")

png(
  write_path(opath, pname_hgb_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_hgb_3)) ### print png path and name in log
print(pt3_hgb)
dev.off()

pname_plat_1 <- paste0(tolower(tblid), "_plat_1", ".png")

png(
  write_path(opath, pname_plat_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_plat_1)) ### print png path and name in log
print(pt1_plat)
dev.off()

pname_plat_2 <- paste0(tolower(tblid), "_plat_2", ".png")

png(
  write_path(opath, pname_plat_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_plat_2)) ### print png path and name in log
print(pt2_plat)
dev.off()

pname_plat_3 <- paste0(tolower(tblid), "_plat_3", ".png")

png(
  write_path(opath, pname_plat_3),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_plat_3)) ### print png path and name in log
print(pt3_plat)
dev.off()


tidytlg::gentlg(
  tlf = "g",
  plotnames = write_path(
    opath,
    c(
      pname_sodium_1,
      pname_sodium_2,
      pname_sodium_3,
      pname_k_1,
      pname_k_2,
      pname_k_3,
      pname_ca_1,
      pname_ca_2,
      pname_ca_3,
      pname_prot_1,
      pname_prot_2,
      pname_prot_3,
      pname_creat_1,
      pname_creat_2,
      pname_creat_3,
      pname_alp_1,
      pname_alp_2,
      pname_alp_3,
      pname_alt_1,
      pname_alt_2,
      pname_alt_3,
      pname_ast_1,
      pname_ast_2,
      pname_ast_3,
      pname_bili_1,
      pname_bili_2,
      pname_bili_3,
      pname_chol,
      pname_hdl,
      pname_trig,
      pname_wbc_1,
      pname_wbc_2,
      pname_wbc_3,
      pname_hgb_1,
      pname_hgb_2,
      pname_hgb_3,
      pname_plat_1,
      pname_plat_2,
      pname_plat_3
    )
  ),
  plotwidth = 8,
  orientation = "landscape",
  opath = write_path(opath),
  file = tblid,
  title = title_footer$title,
  footers = title_footer$main_footer
)
