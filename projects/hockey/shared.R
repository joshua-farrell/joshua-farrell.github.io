
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
