# quick find of function that causes an error (so I can set up error handling)
y <- list_functions("lubridate")
for (n in y$function_name) {
    x <- list()
    x[[which(y$function_name %in% n)]] <- man_extract(n, package = "lubridate")
    print(n) # prints the last function that worked correctly
}

