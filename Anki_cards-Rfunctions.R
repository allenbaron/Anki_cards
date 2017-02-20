# Automatically add man pages for functions from specified packages

# (Potentially) Useful resources:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Processing-documentation-files
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/RdUtils.html
# https://www.r-project.org/help.html

# All packages are located in this directory:
#   "file:///Library/Frameworks/R.framework/Versions/3.3/Resources/library/"
# Index of functions for each package is located in:
#   ".../library/<pkg name>/html/00index.html
# Function man pages are NOT located in this directory

# To do:
#   1. Find location & format of pkg man pages
#   2. Determine how libraries can be automatically added to ANKI (saw this on mobile app
#       in play store)

# INDEX = /Library/Frameworks/R.framework/Versions/3.3/Resources/library/dplyr/INDEX
#   but no links in txt file

###### USE HTML VERSIONS I CAME ACROSS ##########
library(rvest)

# computer connection to open html (not sure what this is exactly)
#connection <- "http://127.0.0.1:11435/"
connection <- "http://127.0.0.1:15288/"

# html listing all packages
Rpkg_index <- "doc/html/packages.html"

# R index html
"file:///Library/Frameworks/R.framework/Versions/3.3/Resources/doc/html/packages.html"
    # only includes standard packages

# base pkg
"file:///Library/Frameworks/R.framework/Versions/3.3/Resources/library/base/html/00Index.html"

# dplyr pkg
"file:///Library/Frameworks/R.framework/Versions/3.3/Resources/library/dplyr/html/00Index.html"


Rpkg_selector <- "td a"
Rpkg_index_html <- read_html(paste(connection, Rpkg_index, sep = ""))

# list package paths, names, and parent html file (lists all functions)
pkg_paths <- html_nodes(Rpkg_index_html, Rpkg_selector) %>%
    html_attr('href') %>%
    sub("(../../)(.*)(00Index.html)", "\\2", .)
pkg_names <- sub("(^library/)(.*)(/html/$)", "\\2", pkg_paths)
pkg_index_file <- "00Index.html"

# prep df for output to anki (as csv?)
anki <- data.frame(name = "", package = "", summary = "", description = "",
                   usage = "", arguments = "")

pos <- 0


pkgs <- c("base", "dplyr", "ggplot", "utils")

pkg_fxns <- function(pkg_paths) {
    # Returns path to function documentation for specified packages
    #   pkgs: names of packages to convert to Anki cards (character vector)
    
    for (i in pkgs) { #seq_along(pkg_paths)
        # open pkg parent html
        pkg_html <- read_html(paste("http://127.0.0.1:11435/", pkg_paths[i],
                                    "00Index.html", sep = ""))
        
        # list path for all functions within given package
        fxn_selector <- "td a" # maybe add :nth-child(n+1) to avoid overall pkg man
        fxn_paths <- html_nodes(pkg_html, fxn_selector) %>%
            html_attr('href')
        fxn_names <- sub("(^.*)(.html$)", "\\1", fxn_paths)
    }
    
        for (j in seq_along(fxn_paths)) {
            if (fxn_names[j] == pkg_names[i]) {
                next
            }
            fxn_html <- read_html(paste(connection, pkg_path[i], fxn_paths[j], sep = ""))
            pos <- pos + 1
            anki$name[pos] <- fxn_names[j]
            anki$package[pos] <- pkg_names[i]
            anki$summary <- html_nodes(fxn_html, "h2") %>%
                html_text()
            anki$description <- html_nodes(fxn_html, "p:nth-child(4)") %>%
                html_text()
            anki$usage <- as.character(html_nodes(fxn_html, "pre:first-of-type"))
            anki$arguments <- as.character(html_nodes(fxn_html, "[summary='R argblock']"))
            
            ## scrape function file (as text)
            #fxn_info <- readLines(paste(connection, pkg_path[28], fxn_paths[160], sep = ""))
            #description_start <- grep("Description", fxn_info) + 2
            #description <- fxn_info[grep("Description", fxn_info) + 2]
        }
}
extract_Rfunction_doc <- function(html_man) {
    anki$name <- fxn_names
    anki$package <- pkg_names[i]
    anki$summary <- html_nodes(fxn_html, "h2") %>%
        html_text()
    anki$description <- html_nodes(fxn_html, "p:nth-child(4)") %>%
        html_text()
    anki$usage <- as.character(html_nodes(fxn_html, "pre:first-of-type"))
    anki$arguments <- as.character(html_nodes(fxn_html, "[summary='R argblock']"))
    # post-processing
    #   tables
    #       1. replace "\n" with " "; some may need to be replaced with "<br>"
    #           in order to force a break (maybe "\n\n")
    #       2. <p> and </p> tags are OKAY!
    #       3. fix "valign=.*>"
    
    # export as txt for Anki
    # Anki can import html if it is in a plain txt format 
    # anki_dir <- "/Users/jbaron/Library/Application Support/Anki2"
    
    paths <- unique(paths)
    attributes(paths) <- list(call = match.call(), topic = topic, 
                              tried_all_packages = tried_all_packages, type = help_type)
    class(paths) <- "help_files_with_topic"
    paths