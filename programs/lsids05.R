################################################################
##### programs_external/lsids05.R
#### DO NOT MODIFY.
#### This file was automatically generated.
#### Changes should be made to individual template scripts in programs/
################################################################




################################################################
### Source: programs/lsids05.R
################################################################


###############################################################################
## Original Reporting Effort: Standards
## Program Name:              lsids05.R
## R version:                 4.2.1
## Short Description:         Create LSIDS05: Listing of Overall Study Start
##                            and End Dates
## Author:                    Johnson & Johnson Innovative Medicine
## Date:                      2024-01-17
## Input:                     ADSL
## Output:                    lsids05.rtf
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

tblid <- "LSIDS05"
fileid <- write_path(opath, tblid)
popfl <- "SCRNFL"
key_cols <- c("COL1")
disp_cols <- paste0("COL", 1:3)
tab_titles <- get_titles_from_file(tblid)
string_map <- default_str_map

###############################################################################
# Process data
###############################################################################

adsl <- pharmaverseadamjnj::adsl %>%
  filter(!!rlang::sym(popfl) == "Y") %>%
  summarise(
    rficdt_min = toupper(format(min(RFICDT, na.rm = TRUE), "%d%b%Y")),
    lstsvdt_max = toupper(format(max(LSTSVDT, na.rm = TRUE), "%d%b%Y")),
    rfpendtc_max = toupper(format(
      max(as.Date(RFPENDTC), na.rm = TRUE),
      "%d%b%Y"
    ))
  )

lsting <- adsl %>%
  mutate(
    COL1 = rficdt_min,
    COL2 = lstsvdt_max,
    # Optional Column: COL3/rfpendtc_max
    COL3 = rfpendtc_max
  )

lsting <- var_relabel(
  lsting,
  COL1 = "First Contact in the Study~[super a]",
  COL2 = "Last Visit in the Study~[super b]",
  # Optional Column: COL3/rfpendtc_max
  COL3 = "Last Contact in the Study~[super c]"
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
