
install_if_missing_and_load("randomForest")
install_if_missing_and_load("caret")

best_results <- function(data, dependent, models) {
    results <- list()
    for (model in names(models)) {
        print(paste("    -", model))
        predictor <- find_best_predictor__(
            model = models[[model]],
            dependent = dependent,
            data = data[["train"]]
        )
        results[[model]] <- list(
            "metrics" = metrics__(predictor, data[["test"]], dependent),
            "predictor" = predictor
        )
        models_with_variable_importance <- c("random_forest")
        if (model %in% models_with_variable_importance) {
            results[[model]][["importance"]] <- (
                variable_importance_graph__(predictor)
            )
        }
    }
    return(results)
}

find_best_predictor__ <- function(data, dependent, model) {
    #
    # This function trains the ML models with cross-validation. It could be
    # optimized with finer control on each model's parameters (they differ from
    # model to model), but I'm not doing that here to keep things simple. If you
    # wanted to, you could do so by adapting the `tuneGrid` parameter with the
    # `train_grid` object for each model separately. To increase the robustness
    # of the analysis, you may increase the `number` parameter which controls
    # the number of splits used for the cross-validation (normally it's between
    # 3 and 5). The higher the number the more time the process takes, but the
    # more robust it is. For this particular problem, I think 3 should suffice.
    #
    train_control <- trainControl(
        allowParallel = TRUE,
        verboseIter = TRUE,
        method = "cv",
        number = 3
    )
    # train_grid <- expand.grid(...)
    if (model == "rf") {
        return(train(
            as.formula(paste(dependent, "~ .")),
            trControl = train_control,
            importance = TRUE,
            # tuneGrid = train_grid,
            method = model,
            data = data
        ))
    } else {
        #
        # The `importance = TRUE` argument is problematic for GLM, so
        # we'll only send it if we're dealing with Random Forests
        #
        return(train(
            as.formula(paste(dependent, "~ .")),
            trControl = train_control,
            # tuneGrid = train_grid,
            method = model,
            data = data
        ))
    }
}

metrics__ <- function(predictor, data, dependent) {
    y <- data[, dependent]
    yp <- predict(predictor, data)
    return(list(
        "RMSE" = root_mean_square_error(y, yp),
        "MAPE" = mean_absolute_percentage_error(y, yp),
        "PPE10" = percentage_predicted_error_within_threshold(y, yp, 0.1)
    ))
}

mean_absolute_percentage_error <- function(y, yp) {
    return(mean(abs(y - yp) / y))
}

root_mean_square_error <- function(y, yp) {
    return(mean(sqrt((y - yp)^2)))
}

percentage_predicted_error_within_threshold <- function(y , yp, threshold) {
    return(sum((abs(y - yp) / y) < threshold) / length(y))
}
