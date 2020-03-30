# Description
# This is a script to read in the data from How Couples Meet and Stay Together.
#
# The data is taken from this link: https://data.stanford.edu/hcmst#download-data.
# I acknowledge core funding support from the U.S. National Science Foundation,
# and supplementary funding from Stanford's Institute for Research in the Social
# Sciences, and the UPS endowment at Stanford University. For research assistance,
# I thank Reuben Jasper Thomas, Elizabeth McClintock, Esra Burak, Kate Weisshaar,
# Taylor Orth, Ariela Schachter, Maja Falcon, and Sonia Hausen. For Web design and
# assistance, I am grateful to Ron Nakao and the Stanford Library. The following
# consultants contributed to the development of the survey instrument and the
# research design: Gary Gates, Jon Krosnick, Brian Powell, Daniel Lichter,
# Matthijs Kalmijn, Timothy Biblarz, and the staff of Knowledge Networks/GfK.

# Author: Angela Zhao
# Version: 2020-01-31

# Libraries

.libPaths(
  new =
    c(
      "/Users/angel/Documents/R/win-library/3.6",
      "/Program Files/R/R-3.6.2/library"
    )
)
library(tidyverse)

library(haven)

# Parameters

file_raw <-
  here::here("data-raw/HCMST_2017_public_data_v1.1_stata/HCMST_2017_v1.1.dta")

file_out <- "/Users/angel/GitHub/interracial_dating/data/hcmst_2017.rds"

#===============================================================================

Sys.time()


file_raw %>%
  read_dta() %>%
  mutate_if(is.labelled, as_factor) %>%
  write_dta(file_out)


Sys.time()

