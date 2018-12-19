
source("./constants.R")

install_if_missing_and_load("caret")
install_if_missing_and_load("DMwR")

clean <- function(data) {
    msg_1 <- "No. observations initial:"
    msg_2 <- "No. observations non-NAs:"
    print_n_obs_after_message__(data, msg_1)
    data <- invalid_values_as_missing__(data)
    data <- impute_missing_values__(data)
    data <- setup_ordinal_variables__(data)
    data <- setup_categorical_variables__(data)
    print_n_obs_after_message__(data, msg_2)
    data <- collapse_dependent_variables__(data)
    print_non_factor_variables__(data)

    #
    # Since there's no notion of "the" dependent variable at this point, and to
    # avoid overcomplicating the code, I checked how different the proportions
    # in the answers are among the three dependents to see if it would make
    # sense to use the same stratified sample for all three. Turns out that the
    # shared answer proportions are upward of 90% for all three combinations, so
    # I decided to go ahead and do simplify the process by doing a single
    # stratified sampling mechanism, after having rebalanced all classes.
    #
    # We need to execute this code at this point because the collapse of the
    # multiple categories into two categories for the dependent variables is
    # done up to this point. However, the effect of this result is used in the
    # `balance()` and `separate()` functions.
    #
    print_shared_answer_proportion__(data, "SAT_INSTRUCTION", "SAT_COMMUNITY")
    print_shared_answer_proportion__(data, "SAT_OVERALL", "SAT_INSTRUCTION")
    print_shared_answer_proportion__(data, "SAT_OVERALL", "SAT_COMMUNITY")

    return(data)
}

balance <- function(data) {
    #
    # When applying the `SMOTE()` function, it returns values that use the
    # underlying integer for each factor, but since it's using a kNN approach,
    # the values may be non-integers, which produces NAs for the factors. To fix
    # this issue, we first convert factors to integers, then apply the SMOTE
    # re-balances, then round the floats to the nearest integers, and then
    # recode ordinal and categorical variables, as we did in the `clean()`
    # function. Although it seems that the re-coding is being done twice
    # unncessarily, if we decide not to use the `balance()` function, we still
    # want the `clean()` function to return the factor variables for ordinal and
    # categorical variables, so it's not wasted.
    #
    data <- independents_as_integers__(data)
    data <- SMOTE(SAT_OVERALL ~ ., data)
    data <- SMOTE(SAT_COMMUNITY ~ ., data)
    data <- SMOTE(SAT_INSTRUCTION ~ ., data)
    data <- round_independents__(data)
    data <- setup_ordinal_variables__(data)
    data <- setup_categorical_variables__(data)
    data <- recode_dependent_variables__(data)
    return(data)
}

separate <- function(data) {
    #
    # A more common name for this operation is `split`, but in R that name is
    # alredy used for another function, so instead of _masking_ the other
    # function, we use a non-existent name in R, which is `separate` to avoid
    # problems.
    #
    sample_ <- createDataPartition(
        p = TRAINING_PROPORTION,
        y = data$SAT_OVERALL,
        list = FALSE
    )
    return(list(
        "train" = data[ sample_, ],
        "test" =  data[-sample_, ],
        "full" = data
    ))
}

recode_variable <- function(data, variable, new_value) {
    for (set in c("full", "train", "test")) {
        data[[set]][, variable] <- new_value
    }
    return(data)
}

independents_as_integers__ <- function(data) {
    for (v in colnames(data)) {
        if (!(v %in% names(DEPENDENT_VARS))) {
            data[, v] <- as.integer(data[, v])
        }
    }
    return(data)
}

round_independents__ <- function(data) {
    for (v in colnames(data)) {
        if (!(v %in% names(DEPENDENT_VARS))) {
            data[, v] <- round(data[, v])
        }
    }
    return(data)
}

recode_dependent_variables__ <- function(data) {
    for (v in names(DEPENDENT_VARS)) {
        data[, v] <- factor(
            as.integer(data[, v]),
            levels = COLLAPSED_VALUES[['levels']],
            labels = COLLAPSED_VALUES[['labels']]
        )
    }
    return(data)
}

print_shared_answer_proportion__ <- function(data, dependent_1, dependent_2) {
    t <- table(data[, dependent_1], data[, dependent_2])
    print(t)
    print(paste("Shared proportion:", sum(diag(t)) / sum(t)))
}

invalid_values_as_missing__ <- function(data) {
    #
    # It took me a long time to realize that there were variables that had
    # values that are invalid (meaning they do not appear in the specification),
    # and that was creating all sorts of problems in other places of the code.
    # To find them I used the `find_nas__()` function in this file.
    #
    # The value `0` was one of the problematic values which appeared in a couple
    # of variables, also in the case of `FCAREER` there was value `48` which
    # should not exis. I created this function to recode all of these invalid
    # values as NAs, that is marking them as if they were missing.
    #
    # Note that both 0 and 48 are outside the range of **all** variables, which
    # means that we don't have to worry about which variable we are removing
    # this values from. If it were not the case, we would have to proceed on a
    # case by case basis.
    #
    invalid_values <- c(0, 48)
    for (v in colnames(data)) {
        for (i in invalid_values) {
            # Be careful with the `|` and `||` operators, the
            # first one is vectorized, the seconds one is not
            data[is.na(data[, v]) | data[, v] == i, v] <- NA
        }
    }
    return(data)
}

impute_missing_values__ <- function(data) {
    #
    # This function imputes the most common value on the missing values for each
    # variable. More sophisticated techniques can be used, but I'm keeping
    # things simple at this point.
    #
    for (v in colnames(data)) {
        #
        # This assumes all variable values are numeric, which is fine for this
        # dataset, but may be problematic in others, if a warning message saying
        # "NAs introduced by coercion", then we probably did not have numeric
        # data, and we need to look into it deeper to undersstand what to do
        #
        nas <- is.na(data[, v])
        data[nas, v] <- as.numeric(
            names(which.max(table(data[, v])))
        )
        print(paste("Imputed", sum(nas), "for", v))
    }
    return(data)
}

setup_ordinal_variables__ <- function(data) {
    for (v in names(ORDINAL_VARS)) {
        before <- data
        data[, v] <- factor(
            as.integer(data[, v]),
            levels = ORDINAL_VARS[[v]][["levels"]],
            labels = ORDINAL_VARS[[v]][["labels"]]
        )
        find_nas__(before, data)
    }
    return(data)
}

setup_categorical_variables__ <- function(data) {
    for (v in names(CATEGORICAL_VARS)) {
        before <- data
        data[, v] <- factor(
            as.integer(data[, v]),
            levels = CATEGORICAL_VARS[[v]][["levels"]],
            labels = CATEGORICAL_VARS[[v]][["labels"]]
        )
        find_nas__(before, data)
    }
    return(data)
}

collapse_dependent_variables__ <- function(data) {
    # For each dependent
    for (var in names(DEPENDENT_VARS)) {
        before <- data
        # Convert to text to avoid problems
        data[, var] <- as.character(data[, var])
        # For each value for the dependent
        for (val in names(DEPENDENT_VARS[[var]])) {
            # Find rows with given value, and replace with new
            data[data[, var] == val, var] <- DEPENDENT_VARS[[var]][[val]]
        }
        # Convert back to factor
        data[, var] <- factor(
            data[, var],
            levels = COLLAPSED_VALUES[["levels"]],
            labels = COLLAPSED_VALUES[["labels"]]
        )
        find_nas__(before, data)
    }
    return(data)
}

print_non_factor_variables__ <- function(data) {
    non_factor_vars <- sapply(names(data), function(x) {
        return(!any(grepl("factor", class(data[, x]))))
    })
    print("Non-factor variables (check for correctness):")
    print(names(data)[non_factor_vars])
}

print_n_obs_after_message__ <- function(data, msg) {
    print(paste(msg, nrow(data)))
}

find_nas__ <- function(before, data) {
    #
    # This function helps find problematic values for a transformation. It looks
    # for NA values in the `data` object, and if it finds them it looks for what
    # their values were before the transformation, using the `before` object
    # (which should be the same data but before the transformation was applied).
    # It should be used as follows:
    #
    # ...
    # before <- data
    # data <- some_transformation(data)
    # find_nas__(before, data)
    # ...
    #
    for (v in colnames(data)) {
        if (any(is.na(data[, v]))) {
            print('----------------')
            print(v)
            print(is.na(data[, v]))
            print("BEFORE ---------")
            print(before[, v])
            print("AFTER ----------")
            print(data[, v])
            print("PROBLEMATIC ----")
            print(before[is.na(data[, v]), v])
            print('----------------')
            stop("Check problematic values")
        }
    }
}
