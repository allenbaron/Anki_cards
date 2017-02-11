man_extract <- function(help_binding,
                         fields = c("Description", "Usage", "Arguments")) {
    # Extract the content (in html format) from the specified "fields",
    # as well as, the header info (function name, package name, and summary)
    # from the function man page
    # Arguments:
    #   help_binding = e.g. x <- help(topic = "abbreviate")
    #   fields = heading names of content from man page to extract
    #       (correct capitalization necessary)
    
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
        if (length(start) != 1) {
            warning(paste0("'", i, "' field not present on function man page"),
                    call. = FALSE)
            next
        }
        end <- if (start == tail(h3_index, n = 1)) {
            length(elements) + 1
        } else {
        end <- h3_index[which(h3_index %in% start) + 1]
        }
        field_vals[[i]] <- elements[(start + 1):(end - 1)]
    }
    field_vals
}