# script to export help man content as txt for Anki

# Anki can import html if it is in a plain txt format 
# anki_dir <- "/Users/jbaron/Library/Application Support/Anki2"

source('~/Documents/DataScience/MyProjects/Anki_cards/man_extract.R')
source('~/Documents/DataScience/MyProjects/Anki_cards/list_functions.R')
source('~/Documents/DataScience/MyProjects/Anki_cards/anki_Rcards.R')

# create initial set of cards
initial_pkgs <- c("base", "dplyr", "ggplot2", "utils", "tidyr", "stats")
initial_content <- anki_Rcards(pkgs, warn = FALSE)
write.table(initial_content, file = "initial_cards.txt", sep = "\t", row.names = FALSE)

# create cards from tidyverse packages (problem with 'lubridate')
tidyverse_pkgs <- tidyverse::tidyverse_packages(include_self = FALSE)
tidy_content <- anki_Rcards(tidyverse_pkgs[!grepl("lubridate", tidyverse_pkgs)],
                            warn = FALSE)
write.table(tidy_content, file = "tidyverse_cards.txt", sep = "\t",
            row.names = FALSE)
    # check for name conflicts
    y <- tidy_content$name[duplicated(tidy_content$name)]
    tidy_content[tidy_content$name %in% y, 1:3]

other_pkgs <- c("graphics", "grDevices", "lubridate", "reshape2")