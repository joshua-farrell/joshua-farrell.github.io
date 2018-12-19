
install_if_missing_and_load("ggplot2")
install_if_missing_and_load("scales")
install_if_missing_and_load("gplots")
install_if_missing_and_load("graphics")
install_if_missing_and_load("FactoMineR")
install_if_missing_and_load("factoextra")

create_and_save_all_graphs <- function(data, type) {
    reset_directory__(paste("./graphs/", type, "/", sep = ""))
    categorical_vars <- c(names(CATEGORICAL_VARS), names(ORDINAL_VARS))
    grid <- expand_grid_unique__(categorical_vars, categorical_vars)
    for (i in 1:nrow(grid)) {
        var_1 <- as.character(grid[i, 1])
        var_2 <- as.character(grid[i, 2])
        fname <- file_name__(type, two_var_name__(var_1, var_2))
        progress__(i, nrow(grid), fname)
        png(
            filename = fname,
            width = GRAPH_WIDTH,
            height = GRAPH_HEIGHT,
            pointsize = GRAPH_POINTSIZE
        )
        #
        # The graphing function represented by `type` is an interface which
        # requires three objects: `data`, `var_1`, `var_2`, that means that any
        # graphing function that can receive those three arguments and follows
        # the assumptions of the `mosaic()`, `balloon()`, and `correspondence()`
        # functions, can also be used as the `graph_function()`, without having
        # to change anything in this `create_and_save_all_graphs()` function's
        # code. The only restriction is that the name of the graphing function
        # must be the same as the value in the `type` variable.
        #
        do.call(type, list(data, var_1, var_2))
        dev.off()
    }
}

correspondence <- function(data, var_1, var_2) {
    d <- select_data__(data)
    table_ <- table(d[, var_1], d[, var_2])
    table_ <- remove_empty_rows_and_columns__(table_)
    # graph <- fviz_ca_biplot(CA(table_, graph = FALSE), repel = TRUE) +
    #     labs(title = graph_title__(var_1, var_2))
    graph <- tryCatch({
        #
        # Sometimes this process fails because for correspondence analysis it's
        # required that the categorical variables used have at least three
        # different categories each, and if that's not the case, the following
        # code throws an error. To avoid stopping the execution of the full
        # automation we need to catch such errors in here, and let the rest of
        # the process continue smoothly, even if we were not able to create some
        # of these graphs.
        #
        graph <- fviz_ca_biplot(CA(table_, graph = FALSE), repel = TRUE) +
            labs(title = graph_title__(var_1, var_2))
        print(graph)
    }, error = function(err) {
        msg <- "Warning: could not create correspondence graph for:"
        print(paste(msg, var_1, "/", var_2, "(see original error below)"))
        print(err)
        return(NULL)
    })
    return(NULL)
}

balloon <- function(data, var_1, var_2) {
    d <- select_data__(data)
    return(balloonplot(
        table(d[, var_1], d[, var_2]),
        main = graph_title__(var_1, var_2)
    ))
}

mosaic <- function(data, var_1, var_2) {
    d <- select_data__(data)
    return(mosaicplot(
        table(d[, var_1], d[, var_2]),
        main = graph_title__(var_1, var_2),
        shade = TRUE,
        xlab = var_1,
        ylab = var_2,
        las = 2
    ))
}

graph_title__ <- function(var_1, var_2) {
    #
    # Even though this seems like a simple function which could be used in-line
    # with the code that calls it, this extra level of indirection allows us to
    # change the title format for all graphs in a single place, instead of
    # having to change it for every graph type.
    #
    return(paste(var_1, "vs.", var_2))
}

remove_empty_rows_and_columns__ <- function(table_) {
    rows_to_keep <- NULL
    for (i in 1:nrow(table_)) {
        if (sum(table_[i, ]) > 0) {
            rows_to_keep <- c(rows_to_keep, i)
        }
    }
    columns_to_keep <- NULL
    for (i in 1:ncol(table_)) {
        if (sum(table_[, i]) > 0) {
            columns_to_keep <- c(columns_to_keep, i)
        }
    }
    return(table_[rows_to_keep, columns_to_keep])
}

variable_importance_graph__ <- function(predictor) {
    imp <- importance(predictor$finalModel)
    data <- data.frame(
        "MeanDecreaseGini" = imp[, "MeanDecreaseGini"],
        "Variable" = rownames(imp)
    )
    data <- data[with(data, order(-MeanDecreaseGini)), ]
    data <- data[1:TOP_N_VAR_IMPORTANCE, ]
    # This line reverses the order for the top values, otherwise it's mixed
    data$Variable <- factor(data$Variable, levels = rev(data$Variable))
    return(
        ggplot(data, aes(MeanDecreaseGini, Variable)) +
        geom_point(size = 2)
    )
}

predictor_graph__ <- function(predictor) {
    return(ggplot(predictor, metric = ACCURACY_METRIC))
}

file_name__ <- function(type, name) {
    return(paste( "./graphs/", type, "/", name, ".png", sep = "" ))
}

two_var_name__ <- function(var_1, var_2) {
    return(paste(var_1, "-", var_2, sep = ""))
}

reset_directory__ <- function(directory) {
    unlink(directory, recursive = TRUE)
    dir.create(directory, showWarnings = FALSE)
}
