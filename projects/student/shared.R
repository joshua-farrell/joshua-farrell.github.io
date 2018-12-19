
expand_grid_unique__ <- function(x, y, include_equals = FALSE) {
    x <- unique(x)
    y <- unique(y)
    g <- function(i) {
        z <- setdiff(y, x[seq_len(i - include_equals)])
        if(length(z)) cbind(x[i], z, deparse.level = 0)
    }
    do.call(rbind, lapply(seq_along(x), g))
}

progress__ <- function(i, n, msg) {
    print(paste(
        "[", i, "/", n, "] ", "(", round(100 * (i / n), 2), "%) : ",
        msg,
        sep = ""
    ))
}

select_data__ <- function(data) {
    if (class(data) == "list") {
        return(data[["full"]])
    } else {
        return(data)
    }
}
