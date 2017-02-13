anki_Rcards <- function(packages, ..., warn = FALSE, programmatically = TRUE, 
                        progress = TRUE) {
    # Extract man pages for functions from specified packages
    # Arguments:
    #   packages = character vector naming packages
    #   ... = other arguments to man_extract()
    #   progress = logical; display progress?
    
    pkg_cards <- list()
    for (i in packages) {
        functions <- list_functions(i)
        if (progress) {
            message("'", i, "' extraction started")
        }
        
        for (n in functions) {
            content <- lapply(functions$function_name, man_extract, 
                          package = i, ...)
        }
        pkg_cards[[i]] <- do.call(rbind, content)
        if (progress) {
            message("COMPLETE!")
        }
    }
    cards <- do.call(rbind, pkg_cards) %>%
        subset(., !duplicated(.))
    cards
}