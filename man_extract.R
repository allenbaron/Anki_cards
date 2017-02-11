man_extract <- function(function_name, package = NULL, ...,
                        fields = c("Description", "Usage", "Arguments"),
                        warn = TRUE) {
    # Extract the content (in html format) from the specified "fields",
    # as well as, the header info (function name, package name, and summary)
    # from the function man page
    # Arguments:
    #   function_name = length-one character vector
    #   package = NULL or length-one character vector
    #   ... = other arguments passed to help()
    #   fields = character vector; heading names of content from man page to 
    #       extract (correct capitalization necessary)
    #   warn = logical; whether to warn if a field does not exist
    #       for specified function
    
    # test for required packages and install/load, as necessary
    req_pkgs <- c("rvest", "xml2", "magrittr")
    pkgs_installed <- basename(find.package(req_pkgs, quiet = TRUE))
    new_pkgs <- req_pkgs[!(req_pkgs %in% pkgs_installed)]
    if (length(new_pkgs)) {
        install.packages(new_pkgs)
    }
    pkgs_to_attach <- req_pkgs[!req_pkgs %in% (.packages())]
    for (i in pkgs_to_attach) {
        library(i, character.only = TRUE)
    }
    
    # load man page (e.g. help()) as html for desired function
    help_binding <- help(topic = eval(function_name),
                         package = eval(package), ...)
    if (length(help_binding) < 1 & is.null(package)) {
        stop(paste0("No documentation for ‘", function_name,
                    "’ in specified packages and libraries:",
                    "\n  specify package or other arguments to help()"))
    }
    help_Rd <- utils:::.getHelpFile(help_binding)
    tools::Rd2HTML(help_Rd, out = "temp.html")
    help_html <- read_html("temp.html")
    file.remove("temp.html")
    
    # remove non-html components
    elements <- html_nodes(help_html, "h3, h3 ~ *") %>%
        gsub("\n\n", "<br />", .) %>%
        gsub("\n", " ", .) %>%
        gsub('\"', "'", .) %>%
        gsub("( {3,}|\t)", "<br />\\1", .) %>%
        gsub("<pre> +", "<pre>", .)
    
    # extract html man content
    function_name <- basename(help_binding)
    pkg_name <- basename(dirname(dirname(help_binding)))
    summary <- html_nodes(help_html, "h2") %>%
        html_text() %>%
        gsub("\n", "", .)
    h3_index <- grep("^<h3>", elements)
    field_vals <- data.frame(name = function_name, package = pkg_name,
                       summary = summary, stringsAsFactors = FALSE)
    for (i in fields) {
        start <- grep(paste0("^<h3>", i), elements)
        if (length(start) != 1) {
            if (warn) {
                warning(paste0("'", i, "' field not present on function man page"))
            }
            field_vals[[i]] <- NA
            next
        }
        end <- if (start == tail(h3_index, n = 1)) {
            length(elements) + 1
        } else {
            h3_index[which(h3_index %in% start) + 1]
        }
        field_vals[[i]] <- paste0(elements[(start + 1):(end - 1)],
                                  collapse = " ")
    }
    field_vals
}