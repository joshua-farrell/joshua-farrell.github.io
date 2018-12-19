
install_if_missing_and_load("mlbench")
install_if_missing_and_load("caret")

variable_selection <- function(data, dependent) {
    #
    # Variable selection with Recursive Feature Elimination (RFE)
    #
    # A list is returned with `data` which is the data that only has the
    # selected variables, `graph` which shows the accuracy change as more
    # variables are added, `n_variables` which is the number of variables
    # selected, and `variables` which are the variables actually selected.
    # Internally, Random Forests are used to measure variable importance.
    #
    d <- select_data__(data)
    dependent_names <- setdiff(names(DEPENDENT_VARS), c(dependent))
    d <- d[, -which(colnames(d) %in% dependent_names)]
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
        sizes = seq(5, ncol(d), 10),
        rfeControl = control
    )
    print(results)
    n_vars <- results$bestSubset
    vars <- results$control$functions$selectVar(results$variables, n_vars)
    return(list(
        "data" = data_variables_subset__(
            data,
            c(dependent, c(dependent_names, vars))
        ),
        "graph" = plot(results, type = c("g", "o")),
        "n_variables" = n_vars,
        "variables" = vars
    ))
}

separate_dependents__ <- function(data) {
    independents <- list()
    dependents <- list()
    for (set in c("full", "train", "test")) {
        dependents[[set]] <- data[[set]][, dependent_names]
        independents[[set]] <- data[[set]][, -select]
    }
    return(list(
        "independentes" = independents,
        "dependents" = dependents
    ))
}

merge_dependents_and_independents__ <- function(dependents, independents) {
    data <- list()
    for (set in c("full", "train", "test")) {
        data[[set]] <- cbind(dependents[[set]], independentes[[set]])
    }
    return(data)
}

data_variables_subset__ <- function(data, vars) {
    for (set in c("full", "train", "test")) {
        data[[set]] <- data[[set]][, vars]
    }
    return(data)
}
