man_extract <- function(function_name, package = NULL, ...,
                         fields = c("Description", "Usage", "Arguments")) {
    # Extract the content (in html format) from the specified "fields",
    # as well as, the header info (function name, package name, and summary)
    # from the function man page
    # Arguments:
    #   function_name = should be a name, length-one character vector or reserved word
    #   package = name of package that function is in
    #   ... = other arguments passed to help()
    #   fields = heading names of content from man page to extract
    #       (correct capitalization necessary)
    
    # test for required packages and load
    req_pkgs <- c("rvest", "xml2", "magrittr")
    if (!all(req_pkgs %in% installed.packages()[,"Package"])) {
        stop("required pkgs (rvest, xml2, magrittr) must be installed!")
    }
    library(rvest)
    library(magrittr)
    
    # allow for variables in name or character format
    if (is.name(y <- substitute(function_name))) {
        function_name <- as.character(y)
    }
    if (!missing(package)) {
        if (is.name(y <- substitute(package))) {
            package <- as.character(y)
        }
    }
    
    # load man page (e.g. help()) as html for desired function
    help_binding <- help(topic = eval(function_name), package = eval(package), ...)
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
        gsub("\n\n", "<br>", .) %>%
        gsub("\n", " ", .) %>%
        gsub('\"',"'", .)
    
    # extract html man content
    function_name <- basename(help_binding)
    pkg_name <- basename(dirname(dirname(help_binding)))
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

z <- function(function_name) {
    if (is.name(y <- substitute(function_name))) {
        function_name <- as.character(y)
    }
    print(y)
    print(function_name)
    help(topic = eval(function_name))
}
    