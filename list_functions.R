list_functions <- function(package) {
    # Lists all functions contained in package index
    # Arguments:
    #   package = length-one character vector

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
    
    # load package index
    library <- "/Library/Frameworks/R.framework/Versions/Current/Resources/library"
    path <- file.path(library, package, "html/00Index.html")
    pkg_html <- read_html(path)
    
    # list all functions within package
    function_name <- html_nodes(pkg_html, "td a") %>%
            html_text()
    list(function_name = function_name, package = package)
}