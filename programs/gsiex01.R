################################################################
##### programs_external/gsiex01.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/gsiex01.R
################################################################


################################################################################
## Original Reporting Effort: Standards
## Program Name:              gsiex01.R
## R version:                 4.2.1
## Short Description:         Duration of Treatment
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      March 18, 2024
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

tblid <- "GSIEX01"

################################################################################
# Get titles and footnotes:
################################################################################

title_footer <- get_titles_from_file(tblid)
string_map <- default_str_map

################################################################################
# Process data:
################################################################################

# reading data

adexsum <- pharmaverseadamjnj::adexsum %>%
  filter(PARAMCD == "TRTDURM", SAFFL == "Y") %>%
  mutate(
    cat = forcats::fct_reorder(AVALCAT1, AVALCA1N),
    # abbreviate treatment group
    trt_abb = factor(case_when(
      TRT01A == "Xanomeline High Dose" ~ "Xan Low",
      TRT01A == "Xanomeline Low Dose" ~ "Xan High",
      TRT01A == "Placebo" ~ "PBO"
    ))
  ) %>%
  # add abbreviation to TRT01A
  mutate(
    TRT01A = forcats::fct_recode(
      TRT01A,
      "Xanomeline (High)" = "Xanomeline High Dose",
      "Xanomeline (Low)" = "Xanomeline Low Dose",
      "Placebo (PBO)" = "Placebo"
    )
  )

# drop levels not associated with TRTDURM
adexsum$cat <- droplevels(adexsum$cat)

# calculate percentage of count per each treatment group
df1 <- adexsum %>%
  group_by(cat, TRT01A, trt_abb) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  # fill in missing values for time points which don't have data
  tidyr::complete(cat, tidyr::nesting(TRT01A, trt_abb), fill = list(n = 0))

df2 <- df1 %>%
  group_by(TRT01A) %>%
  summarise(total = sum(n)) %>%
  ungroup()

df3 <- df1 %>%
  left_join(df2, by = "TRT01A") %>%
  mutate(
    pern = tidytlg::roundSAS((n / total) * 100, digit = 1),
    perc = tidytlg::roundSAS((n / total) * 100, digit = 1, as_char = TRUE)
  )

# check month group having no subjects in any treatment group

ck <- df3 %>%
  group_by(cat) %>%
  summarise(total = sum(pern)) %>%
  filter(total == 0)

df <- anti_join(df3, ck, by = "cat")

# split data in two graphics
time_point_1 <- c(
  "0 to <3 months",
  "3 to <6 months",
  "6 to <9 months",
  "9 to <12 months",
  "12 to <15 months",
  "15 to <18 months",
  "18 to <21 months"
)

time_point_2 <- c(
  "21 to <24 months",
  "24 to <27 months",
  "27 to <30 months",
  "30 to <33 months",
  "33 to <36 months",
  "36 to <39 months"
)

adex_1 <- df %>%
  filter(cat %in% time_point_1)

adex_2 <- df %>%
  filter(cat %in% time_point_2)


################################################################################
# Generate plot:
################################################################################

# define parameters for plotting:
# assign colorblind friendly palette: black(Xan Low), orange(Xan High), dark blue(PBO)
cbbPalette <- c("#000000", "#E69F00", "#0072B2")

x_label <- " "
y_label <- "Percentage of Subjects"


# Bar plot ---------------------------------------------------

g_facet <- function(df) {
  # bar plot in facet -------------------------------------
  plot1 <- df %>%
    ggplot(aes(x = .data$trt_abb, y = .data$pern, fill = .data$TRT01A)) +
    geom_col(position = position_dodge(0.5)) +
    geom_text(aes(label = perc), position = position_dodge(0.5), vjust = -0.5) +
    facet_wrap(~ .data$cat, nrow = 1, strip.position = "bottom") +

    # assign colorblind friendly palette
    scale_fill_manual(values = cbbPalette) +
    labs(x = x_label, y = y_label) +
    theme_bw() +
    theme(
      text = element_text(size = 9, color = "black"),
      strip.background = element_rect(fill = NA, color = "black"),
      strip.placement = "outside",
      strip.text = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.size = unit(5, "mm"),
      legend.box.background = element_rect(colour = "black", size = 0.5),
      legend.text = element_text(size = 9),
      panel.spacing.x = unit(0, "line"),
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.text.x = element_text(angle = 320, vjust = -1),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 9)
    )
}

plot1 <- g_facet(adex_1)
plot2 <- g_facet(adex_2)


################################################################################
# Create png and output file:
################################################################################

# create png file and output figure
pname_1 <- paste0(tolower(tblid), "_1", ".png")

png(
  write_path(opath, pname_1),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_1)) ### print png path and name in log
print(plot1)
dev.off()

pname_2 <- paste0(tolower(tblid), "_2", ".png")

png(
  write_path(opath, pname_2),
  width = 22,
  height = 14,
  units = "cm",
  res = 300,
  type = "cairo"
)
print(write_path(opath, pname_2)) ### print png path and name in log
print(plot2)
dev.off()

if (length(title_footer$main_footer) == 0) {
  title_footer$main_footer <- NULL
}

tidytlg::gentlg(
  tlf = "g",
  plotnames = write_path(opath, c(pname_1, pname_2)),
  plotwidth = 8,
  orientation = "landscape",
  opath = write_path(opath),
  file = tblid,
  title = title_footer$title,
  footers = title_footer$main_footer
)
