
install_if_missing_and_load("mlbench")
install_if_missing_and_load("caret")

variable_selection <- function(data, dependent) {
    data <- remove_unnecessary_variables__(data)
    data <- remove_highly_correlated_variables__(data)
    data <- find_optimal_variable_combination__(data, dependent)
}

remove_unnecessary_variables__ <- function(data) {
    for (set in c("full", "train", "test")) {
        select <- which(colnames(data[[set]]) %in% UNNECESSARY_VARS)
        data[[set]] <- data[[set]][, -select]
    }
    return(data)
}

remove_highly_correlated_variables__ <- function(data) {
    #
    # When variables are too correlated among themselves, the information they
    # provide is redundant. To maximize the information while we minimize the
    # number of variables left in the analysis, we remove variables which are
    # highly correlated among themselves.
    #
    writeLines("\nRemoving variables with high correlations...\n")
    d <- select_data__(data)
    d_numeric <- d[, sapply(d, class) == "numeric"]
    d_numeric <- d_numeric[, -which(colnames(d_numeric) == "Year")]
    remove <- findCorrelation(cor(d_numeric), cutoff = CORRELATION_CUTOFF)
    remove_names <- colnames(d_numeric)[remove]
    keep <- sapply(colnames(d), function(x) { return(!(x %in% remove_names)) })
    keep_names <- names(keep)[keep]
    data <- data_variables_subset__(data, keep_names)
    writeLines(paste(
        "\nVariables removed (", length(remove_names), "):\n\n", sep = ""
    ))
    print(remove_names)
    writeLines(paste(
        "\nVariables kept (", length(keep_names), "):\n\n", sep = ""
    ))
    print(keep_names)
    return(data)
}

find_optimal_variable_combination__ <- function(data, dependent) {
    #
    # Variable selection with Recursive Feature Elimination (RFE)
    #
    # A list is returned with `data` which is the data that only has the
    # selected variables, `graph` which shows the accuracy change as more
    # variables are added, `n_variables` which is the number of variables
    # selected, and `variables` which are the variables actually selected.
    # Internally, Random Forests are used to measure variable importance.
    #
    writeLines("\nCreating selection data structure with RFE...\n")
    d <- select_data__(data)
    control <- rfeControl(
        functions = rfFuncs,
        allowParallel = TRUE,
        verbose = TRUE,
        method = "cv",
        number = 2
    )
    results <- rfe(
        x = d[, -which(names(d) == dependent)],
        y = d[, dependent],
        rfeControl = control,
        # sizes = seq(5, ncol(d), 20)
        sizes = seq(5, 200, 20)
    )
    print(results)
    n_vars <- results$bestSubset
    vars <- results$control$functions$selectVar(results$variables, n_vars)
    return(list(
        "data" = data_variables_subset__(data, c(dependent, vars)),
        "graph" = plot(results, type = c("g", "o")),
        "n_variables" = n_vars,
        "variables" = vars
    ))
}

data_variables_subset__ <- function(data, vars) {
    for (set in c("full", "train", "test")) {
        data[[set]] <- data[[set]][, vars]
    }
    return(data)
}
