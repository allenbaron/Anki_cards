anki_Rcards <- function(packages, ..., quietly = FALSE) {
    # Extract man pages for functions from specified packages
    # Arguments:
    #   packages = character vector naming packages
    #   ... = other arguments to man_extract() and help()
    #   quietly = logical; display progress?
    
    cards <- list()
    for (i in packages) {
        functions <- list_functions(i)
        if (!quietly) {
            message("'", i, "' extraction started")
        }
        content <- lapply(functions$function_name, man_extract, 
                          package = i, ...)
        cards[[i]] <- do.call(rbind, content)
        if (!quietly) {
            message("COMPLETE!")
        }
    }
    anki <- do.call(rbind, cards)
    anki <- anki[!duplicated(anki), ]
    anki
}