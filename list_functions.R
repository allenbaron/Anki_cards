list_functions <- function(package) {
    library <- "/Library/Frameworks/R.framework/Versions/Current/Resources/library"
    path <- file.path(library, package, "html/00Index.html")
    pkg_html <- read_html(path)
    
    # list all functions within package
    functions <- html_nodes(pkg_html, "td a") %>%
            html_text()
    data.frame(function_name = functions, package = package)
}