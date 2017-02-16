# script to export help man content as txt for Anki

# Anki can import html if it is in a plain txt format 
# anki_dir <- "/Users/jbaron/Library/Application Support/Anki2"

source('~/Documents/DataScience/MyProjects/Anki_cards/man_extract.R')
source('~/Documents/DataScience/MyProjects/Anki_cards/list_functions.R')
source('~/Documents/DataScience/MyProjects/Anki_cards/anki_Rcards.R')

# create base set of cards
base_pkgs <- c("stats", "graphics", "grDevices", "utils", "methods", "base")
base_content <- anki_Rcards(base_pkgs)
funcs_wError <- subset(base_content, error == TRUE)
base_content <- subset(base_content, error == FALSE)
write.table(base_content, file = "base_cards.txt", sep = "\t", row.names = FALSE)

# create cards from tidyverse packages (problem with 'lubridate')
tidyverse_pkgs <- tidyverse::tidyverse_packages(include_self = FALSE)
tidyverse_content <- anki_Rcards(tidyverse_pkgs)
funcs_wError <- rbind(funcs_wError, subset(tidyverse_content, error == TRUE))
tidyverse_content <- subset(tidyverse_content, error == FALSE)[, -tidyverse_content$error]
write.table(tidyverse_content, file = "tidyverse_cards.txt", sep = "\t",
            row.names = FALSE)
write.table(funcs_wError, file = "fxns_wo_cards.txt", sep = "\t",
            row.names = FALSE)

# packages of interest not yet extracted
other_pkgs <- c("reshape2")

