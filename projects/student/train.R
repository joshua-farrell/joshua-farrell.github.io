
source("./constants.R")

install_if_missing_and_load("randomForest")
install_if_missing_and_load("ggplot2")
install_if_missing_and_load("caret")
install_if_missing_and_load("pROC")

category_proportions <- function(data, variable) {
    if (class(data) == "list") {
        d <- data[["full"]]
    } else {
        d <- data
    }
    print(table(d[, variable]) / nrow(d))
    print(paste("No. of observations:", nrow(d)))
}

best_results_per_year <- function(data, dependent, models) {
    results <- list()
    data <- remove_non_informative_variables__(data)
    data <- keep_single_dependent__(data, dependent)
    unique_years <- unique(data[["train"]]$YEAR_FS)
    for (year in unique_years[order(unique_years)]) {
        data_for_year <- data_for_year__(data, year)
        year_name <- paste("Y_", year, sep = "")
        print(paste("-", year))
        results[[year_name]] <- list()
        for (model in names(models)) {
            print(paste("    -", model))
            predictor <- find_best_predictor__(
                model = models[[model]],
                dependent = dependent,
                data = data_for_year[["train"]]
            )
            results[[year_name]][[model]] <- list(
                "metrics" = metrics__(predictor, data_for_year[["test"]], dependent),
                "graph" = predictor_graph__(predictor),
                "predictor" = predictor
            )
            models_with_variable_importance <- c("random_forest")
            if (model %in% models_with_variable_importance) {
                results[[year_name]][[model]][["importance"]] <- (
                    variable_importance_graph__(predictor)
                )
            }
        }
    }
    return(results)
}

roc <- function(predictor, data, dependent) {
    predictions <- predict(predictor, data)
    print(plot.roc(
        as.numeric(predictions),
        as.numeric(data[, dependent])
    ))
}

data_for_year__ <- function(data, year) {
    for (set in c("full", "train", "test")) {
        data[[set]] <- data[[set]][data[[set]]$YEAR_FS == year, ]
        data[[set]] <- data[[set]][, !(colnames(data[[set]]) == "YEAR_FS")]
    }
    return(data)
}

remove_non_informative_variables__ <- function(data) {
    non_informative_vars <- c("ID")
    for (set in c("full", "train", "test")) {
        keep <- !(colnames(data[[set]]) %in% non_informative_vars)
        data[[set]] <- data[[set]][, keep]
    }
    return(data)
}

keep_single_dependent__ <- function(data, dependent) {
    for (set in c("full", "train", "test")) {
        for (d in names(DEPENDENT_VARS)) {
            if (d != dependent) {
                data[[set]] <- data[[set]][, !(colnames(data[[set]]) == d)]
            }
        }
    }
    return(data)
}

find_best_predictor__ <- function(data, dependent, model) {
    #
    # This function trains the ML models with cross-validation. It could be
    # optimized with finer control on each model's parameters (they differ from
    # model to model), but I'm not doing that here to keep things simple. If we
    # wanted to, we can by adapting the `tuneGrid` parameter with the
    # `train_grid` object for each model separately. To increase the robustness
    # of the analysis, you may increase the `number` parameter which controls
    # the number of splits used for the cross-validation (normally it's between
    # 3 and 5). The higher the number the more time the process takes, but the
    # more robust it is. For this particular problem, I think 3 should suffice.
    #
    train_control <- trainControl(
        allowParallel = TRUE,
        verboseIter = TRUE,
        sampling = "smote",
        method = "cv",
        # To parameterize:
        # number = 2
        number = CROSS_VALIDATION_FOLDS # from constants.R file number = 3
    )
    # train_grid <- expand.grid(...)
    return(train(
        as.formula(paste(dependent, "~ .")),
        trControl = train_control,
        importance = TRUE,
        # tuneGrid = train_grid,
        method = model,
        data = data
    ))
}

metrics__ <- function(predictor, data, dependent) {
    predictions <- predict(predictor, data)
    return(confusionMatrix(predictions, data[, dependent]))
}
