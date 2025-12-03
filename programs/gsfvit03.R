################################################################
##### programs_external/gsfvit03.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/gsfvit03.R
################################################################


################################################################################
## Original Reporting Effort: Standards
## Program Name:              gsfvit03.R
## R version:                 4.2.1
## Short Description:         Baseline vs. Maximum Postbaseline [Vital Sign Parameter]
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      Jan 16, 2024
## Input:
## Output:
## Remarks:                   Include parameters: Systolic BP, Diastolic BP
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

tblid <- "GSFVIT03"

################################################################################
# Get titles and footnotes:
################################################################################

title_footer <- get_titles_from_file(tblid)
string_map <- default_str_map

################################################################################
# Process data:
################################################################################

# define function for data processing
dt <- function(param_cd) {
  advs <- pharmaverseadamjnj::advs %>%
    filter(PARAMCD == param_cd & SAFFL == "Y") %>%
    # add abbreviation to TRT01A
    mutate(
      TRT01A = forcats::fct_recode(
        TRT01A,
        "Xanomeline (High)" = "Xanomeline High Dose",
        "Xanomeline (Low)" = "Xanomeline Low Dose",
        "Placebo (PBO)" = "Placebo"
      )
    )

  baseline <- advs %>%
    filter(ABLFL == "Y") %>%
    select(USUBJID, TRT01A, PARAMCD, AVAL) %>%
    rename(baseline = AVAL)

  post_baseline <- advs %>%
    filter(ANL03FL == "Y" & APOBLFL == "Y") %>%
    select(USUBJID, TRT01A, PARAMCD, AVAL) %>%
    rename(postvalue = AVAL)

  vs <- full_join(baseline, post_baseline) %>%
    filter(!is.na(baseline) & !is.na(postvalue))
}

# generate data set for Systolic blood pressure
advs_sys <- dt("SYSBP")

# generate data set for Diastolic blood pressure
advs_dia <- dt("DIABP")


################################################################################
# Generate plot:
################################################################################

# define function for multiple calls:
g_scatter <- function(df, x_label, y_label) {
  # scatter plot ---------------------------------------------------
  plot <- ggplot(
    df,
    aes(
      x = .data$baseline,
      y = .data$postvalue,
      shape = .data$TRT01A,
      color = .data$TRT01A,
      linetype = .data$TRT01A
    )
  ) +

    # Make each dot partially transparent, with 0.5 opacity on alpha
    geom_point(alpha = 0.5, size = 1.2) +
    # alternative - apply jitter or dodge point with random noise as follows
    # geom_point(alpha = 0.75,
    #          position = position_dodge(width = 2)) +

    # Use a hollow circle, triangle, and cross as choices for shape
    scale_shape_manual(values = c(1, 2, 3)) +

    # Add linear regression line, w/o confidence region
    geom_smooth(method = lm, se = FALSE) +

    # Add dotted reference line for no increase
    geom_abline(intercept = 0, slope = 1, linetype = 3) +
    scale_x_continuous(
      breaks = scales::extended_breaks(n = 5),
      labels = scales::label_number(accuracy = 1)
    ) +
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
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9, face = "bold")
    )
}


# define parameters for plotting:
# assign colorblind friendly palette: black(Xan Low), orange(Xan High), dark blue(PBO)
cbbPalette <- c("#000000", "#E69F00", "#0072B2")

# call for Systolic blood pressure
pt_sys <- g_scatter(
  df = advs_sys,
  x_label = "Baseline Systolic Blood Pressure (mmHg)",
  y_label = "Maximum Post-baseline Systolic Blood Pressure (mmHg)"
)

# call for Diastolic blood pressure
pt_dia <- g_scatter(
  df = advs_dia,
  x_label = "Baseline Diastolic Blood Pressure (mmHg)",
  y_label = "Maximum Post-baseline Diastolic Blood Pressure (mmHg)"
)


################################################################################
# Create png and output file:
################################################################################

# create png file and output figure
pname_sys_max <- paste0(tolower(tblid), "_sys_max", ".png")

png(
  write_path(opath, pname_sys_max),
  width = 16,
  height = 18,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_sys_max)) ### print png path and name in log
print(pt_sys)
dev.off()

pname_dia_max <- paste0(tolower(tblid), "_dia_max", ".png")

png(
  write_path(opath, pname_dia_max),
  width = 16,
  height = 18,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_dia_max)) ### print png path and name in log
print(pt_dia)
dev.off()

tidytlg::gentlg(
  tlf = "g",
  plotnames = write_path(opath, c(pname_sys_max, pname_dia_max)),
  # plotwidth   = 10,
  orientation = "portrait",
  opath = write_path(opath),
  file = tblid,
  title = title_footer$title,
  footers = title_footer$main_footer
)
