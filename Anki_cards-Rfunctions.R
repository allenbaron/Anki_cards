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


help_binding <- help(topic = function_name) # need to get function name from list of package 00index.html
    
Rman_extract <- function(help_binding,
                         fields = c("Description", "Usage", "Arguments")) {
    # Extract the content (in html format) from the specified "fields", as well as,
    # the header info (function name, package name, and summary) from the
    # man page
    # Arguments:
    #   help_binding = e.g. x <- help(topic = "abbreviate")
    
    # test if required packages installed
    req_pkgs <- c("rvest", "xml2", "magrittr")
    if (!all(req_pkgs %in% installed.packages()[,"Package"])) {
        stop("required pkgs (rvest, xml2, magrittr) must be installed!")
    }
    
    library(rvest)
    library(magrittr)
    function_name <- basename(help_binding)
    pkg_name <- basename(dirname(dirname(help_binding)))
    
    # reload man page as html for desired function
    help_Rd <- utils:::.getHelpFile(help_binding)
    tools::Rd2HTML(help_Rd, out = "temp.html")
    help_html <- read_html("temp.html")
    file.remove("temp.html")
    
    # remove non-html components
    elements <- html_nodes(help_html, "h3, h3 ~ *") %>%
        gsub("\n\n", "<br>", .) %>%
        gsub("\n", " ", .) %>%
        gsub('\"',"'", .)
    
    # extract help info from help_html for card
    summary <- html_nodes(help_html, "h2") %>%
        html_text()
    h3_index <- grep("^<h3>", elements)
    field_vals <- list(name = function_name, package = pkg_name,
                       summary = summary)
    for (i in fields) {
        start <- grep(paste0("^<h3>", i), elements)
        end <- h3_index[which(h3_index %in% start) + 1]
        field_vals[[i]] <- elements[(start + 1):(end - 1)]
    }
    field_vals
}

x <- data.frame(x)

    
    #usage <- as.character(html_nodes(fxn_html, "pre:first-of-type"))
    #anki$arguments <- as.character(html_nodes(fxn_html, "[summary='R argblock']"))
    
    t <- html_nodes(help_html, "h3, h3 ~ p, h3 ~ pre, [summary='R argblock']")

    t1 <- html_nodes(help_html, "*")
    t2 <- html_nodes(help_html, "h3, p, pre, [summary='R argblock']")
    t3 <- html_nodes(help_html, "title") %>%
        html_text()
    #title <- html_nodes(help_html, "[summary='page for abbreviate'] td:first-of-type") %>%
    #    html_text()

    #help_char <- readLines("temp.html")
    
    # extract help info from help_char for card
    #start <- grep("<h3>Description", help_char) + 2
    #start <- help_char[start:length(help_char)]
    #details <- help_char[(grep("<p>", start)[1] + :grep("</p>", start)[1]
    
    
    # post-processing
    #   tables
    #       1. replace "\n" with " "; some may need to be replaced with "<br>"
    #           in order to force a break (maybe "\n\n")
    #       2. <p> and </p> tags are OKAY!
    #       3. fix "valign=.*>"
    
    # export as txt for Anki
    # Anki can import html if it is in a plain txt format 
    # anki_dir <- "/Users/jbaron/Library/Application Support/Anki2"