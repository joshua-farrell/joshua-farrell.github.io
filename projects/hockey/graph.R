
install_if_missing_and_load("ggplot2")
install_if_missing_and_load("corrplot")

generate_multiple_random_correlation_graphs <- function(
    data, n_variables, n_graphs
) {
    reset_directory__("./graphs/correlations/")
    for (i in 1:n_graphs) {
        progress__(i, n_graphs, "")
        fname <- file_name__("correlations", i)
        png(
            filename = fname,
            width = GRAPH_WIDTH,
            height = GRAPH_HEIGHT,
            pointsize = GRAPH_POINTSIZE
        )
        random_correlations_graph(data, n_variables)
        dev.off()
    }
}

random_correlations_graph <- function(data, n_variables) {
    d <- select_data__(data)
    d <- d[, sapply(d, class) == "numeric"]
    d <- d[, sample(1:ncol(d), n_variables)]
    p_values <- correlation_p_values__(d)
    return(corrplot(
        cor(d),
        method = "color",
        type = "upper",
        order = "hclust",
        addCoef.col = "black",
        tl.col = "black",
        tl.srt = 45,
        p.mat = p_values,
        sig.level = 0.01,
        diag = FALSE
    ))
}

correlation_p_values__ <- function(data) {
    n <- ncol(data)
    data <- as.matrix(data)
    p_values <- matrix(NA, n, n)
    diag(p_values) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            p_values[i, j] <- p_values[j, i] <- (
                cor.test(data[, i], data[, j])$p.value
            )
        }
    }
    colnames(p_values) <- rownames(p_values) <- colnames(data)
    return(p_values)
}

generate_multiple_random_scatter_graphs <- function(data, n_graphs) {
    reset_directory__("./graphs/scatters/")
    d <- select_data__(data)
    combinations <- matrix(sample(1:ncol(d), 3 * n_graphs), nrow = 3)
    for (i in 1:ncol(combinations)) {
        #
        # NOTE: It may be the case that randomly "Salary" is selected as one of
        #       these variables, but the chance is so low that I'm not
        #       considering that for now
        #
        x <- colnames(d)[combinations[1, i]]
        y <- colnames(d)[combinations[2, i]]
        s <- colnames(d)[combinations[3, i]]
        progress__(i, ncol(combinations), "")
        name <- paste(x, y, s, sep = "_")
        fname <- file_name__("scatters", name)
        png(
            filename = fname,
            width = GRAPH_WIDTH,
            height = GRAPH_HEIGHT,
            pointsize = GRAPH_POINTSIZE
        )
        print(scatter_graph_with_size_and_salary_color(d, x, y, s))
        dev.off()
    }
}

scatter_graph_with_size_and_salary_color <- function(
    data, x, y, size
) {
    d <- select_data__(data)
    return(ggplot(d, aes_string(x, y, size = size, color = "Salary")) +
        geom_point() +
        scale_color_gradient(low = "blue", high = "green") +
        geom_smooth(method='loess', color = "red", show.legend = FALSE)
    )

}

variable_importance_graph__ <- function(predictor) {
    imp <- importance(predictor$finalModel)
    data <- data.frame(
        "PerctIncMSE" = imp[, "%IncMSE"],
        "Variable" = rownames(imp)
    )
    data <- data[with(data, order(-PerctIncMSE)), ]
    data <- data[1:TOP_N_VAR_IMPORTANCE, ]
    # This line reverses the order for the top values, otherwise it's mixed
    data$Variable <- factor(data$Variable, levels = rev(data$Variable))
    return(
        ggplot(data, aes(PerctIncMSE, Variable)) +
        geom_point(size = 2)
    )
}

file_name__ <- function(type, name) {
    return(paste( "./graphs/", type, "/", name, ".png", sep = "" ))
}

reset_directory__ <- function(directory) {
    unlink(directory, recursive = TRUE)
    dir.create(directory, showWarnings = FALSE)
}
