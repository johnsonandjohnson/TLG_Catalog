################################################################
##### programs_external/lsids04.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/lsids04.R
################################################################


###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsids04.R
## R version:                 4.2.1
## Short Description:         Create LSIDS04: Listing of Subjects Who Were
##                            Randomized but Never Treated
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-17
## Input:                     ADSL
## Output:                    lsids04.rtf
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
###############################################################################

###############################################################################
# Prep environment
###############################################################################

library(envsetup)
library(tern)
library(dplyr)
library(rtables)
library(rlistings)
library(junco)

###############################################################################
# Define script level parameters
###############################################################################

tblid <- "LSIDS04"
fileid <- write_path(opath, tblid)
trtvar <- "TRT01P"
key_cols <- c("COL0", "COL1")
disp_cols <- paste0("COL", 0:3)
concat_sep <- " / "
tab_titles <- get_titles_from_file(tblid)
string_map <- default_str_map


###############################################################################
# Process data
###############################################################################

adsl <- pharmaverseadamjnj::adsl %>%
  filter(RANDFL == "Y" & SAFFL == "N")

lsting <- adsl %>%
  mutate(
    AGE = explicit_na(as.character(AGE), ""),
    SEX = explicit_na(SEX, ""),
    RACE_DECODE = explicit_na(RACE_DECODE, ""),
    COL0 = explicit_na(.data[[trtvar]], ""),
    COL1 = explicit_na(USUBJID, ""),
    COL2 = paste(AGE, SEX, RACE_DECODE, sep = concat_sep),
    # Optional Column: COL3/DCTREAS
    COL3 = explicit_na(DCTREAS, ""),
  ) %>%
  arrange(COL0, COL1)

lsting <- var_relabel(
  lsting,
  COL0 = "Treatment Group",
  COL1 = "Subject ID",
  COL2 = paste("Age (years)", "Sex", "Race", sep = concat_sep),
  # Optional Column: COL3/DCTREAS
  COL3 = "Primary Reason for no Treatment"
)

###############################################################################
# Build listing
###############################################################################

result <- rlistings::as_listing(
  df = lsting,
  key_cols = key_cols,
  disp_cols = disp_cols
)

###############################################################################
# Add titles and footnotes
###############################################################################

result <- set_titles(result, tab_titles)

###############################################################################
# Output listing
###############################################################################

tt_to_tlgrtf(string_map = string_map, tt = result, file = fileid, orientation = "landscape")
