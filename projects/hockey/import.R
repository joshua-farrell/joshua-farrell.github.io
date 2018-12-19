
install_if_missing_and_load("plyr")

import <- function(directory) {
    # writeLines("\nImporting 2017...\n")
    # data_2017 <- import_data_for_year__(directory, "2017")
    # error_if_vars_with_string__(data_2017, "\\.x")
    # error_if_vars_with_string__(data_2017, "\\.y")
    writeLines("\nImporting 2018...\n")
    data_2018 <- import_data_for_year__(directory, "2018")
    error_if_vars_with_string__(data_2018, "\\.x")
    error_if_vars_with_string__(data_2018, "\\.y")
    # writeLines("\nMerging 2017 and 2018...\n")
    # data_merged <- merge_across_years__(data_2017, data_2018)
    # error_if_vars_with_string__(data_merged, "\\.x")
    # error_if_vars_with_string__(data_merged, "\\.y")
    # data_merged <- order_variables__(data_merged)
    data_merged <- order_variables__(data_2018)
    return(data_merged)
}

import_data_for_year__ <- function(directory, year) {
    data <- NULL
    players <- list()
    for(file in list.files(path = directory, full.names = TRUE)) {
        if (!grepl(year, file)) {
            # If the year is not in the file name, skip
            next
        }
        print(paste("Importing:", file))
        data_new <- read.csv(
            file,
            na.strings = NA_STRINGS,
            stringsAsFactors = FALSE
        )
        data_new <- add_file_identifiers__(data_new, file)
        if(!is.null(data)) {
            data <- merge_within_year__(data, data_new, ID_VARS)
        } else {
            data <- data_new
        }
    }
    return(data)
}

add_file_identifiers__ <- function(data, file) {
    #
    # In a previous approach I was adding "Performance" as an identifier as
    # well, but it later turned out to be problematic and created logical-NAs
    # (impossibilities) since it was being used both as an observation
    # identifier as well as a column name (through prefixes), so I dropped it,
    # but I'm leaving here the implementation (commented out) in case it's
    # useful later.
    #
    data <- prefix_variable_names__(data, file)
    # data <- cbind(performance__(file), data)
    data <- cbind(year__(file), data)
    # colnames(data)[1:2] <- c("Year", "Performance")
    colnames(data)[1] <- c("Year")
    return(data)
}

error_if_vars_with_string__ <- function(data, string) {
    #
    # This function is used to check that the `.x` and `.y` suffixes from using
    # the `merge()` function are not found. If they are, meaning that theere are
    # conflicting variables names not accounted for, the import process will
    # stop with an error.
    #
    c <- colnames(data)
    select <- unlist(lapply(c, function(x) {
        return(grepl(string, x))
    }))
    if (length(c[select]) > 0) {
        print(c[select])
        stop(paste("'", string, "' found (should not happen)"))
    }
}

prefix_variable_names__ <- function(data, file) {
    #
    # Add prefixes to variables names that need them, according to the file
    # being imported (e.g. `TOT`, `ES`, `PK`, and `PP`), which are taken through
    # the `performance__()` function.
    #
    prefix <- paste(performance__(file), "_", sep = "")
    new_var_names <- NULL
    for (name in colnames(data)) {
        if (!(name %in% NON_PREFIX_VARS)) {
            name <- paste(prefix, name, sep = "")
        }
        new_var_names <- c(new_var_names, name)
    }
    colnames(data) <- new_var_names
    return(data)
}

performance__ <- function(file) {
    #
    # Get the `TOT`, `ES`, `PK`, and `PP` strings out of the file names
    #
    return(gsub(".csv", "", strsplit(file, "_")[[1]][2]))
}

year__ <- function(file) {
    #
    # Get the year out of the file names
    #
    part_ <- strsplit(file, "_")[[1]][1]
    part_ <- strsplit(part_, "/")[[1]]
    return(as.integer(part_[length(part_)]))
}

merge_within_year__ <- function(data_1, data_2, id_vars) {
    #
    # Merge datasets from within the same year by using an "outer-join". What
    # this does is make sure we keep all variables from all datasets without
    # adding new rows where there's an "ID match", which is composed of the
    #
    return(merge(x = data_1, y = data_2, by = id_vars, all = TRUE))
}

merge_across_years__ <- function(data_1, data_2) {
    #
    # Merge datasets from distinct years by using a "filled-rbind", meaning that
    # no "ID matching" will take place, but new variable names will be created
    # if necessary.
    # thought I am excluding the 2016-2017 data
    return(unique(rbind.fill(data_1, data_2)))
}

order_variables__ <- function(data) {
    #
    # This is used so that when the exported data is explored we have the ID
    # variables and the salary at the beginning of the spreadsheet
    #
    vars_first <- NON_PREFIX_VARS
    vars_next <- colnames(data)[order(colnames(data))]
    vars_next <- vars_next[!(vars_next %in% vars_first)]
    return(data[, c(vars_first, vars_next)])
}
