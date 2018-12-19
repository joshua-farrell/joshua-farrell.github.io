
source("./constants.R")

install_if_missing_and_load("progress")

clean <- function(data) {
    data <- clean_non_whitelisted_character_variables__(data)
    data <- keep_variables_with_acceptable_na_proportions__(data)
    data <- keep_observations_with_acceptable_na_proportions__(data)
    data <- keep_observations_with_non_nas_for_salary__(data)
    data <- disaggregate_due_to_multiple_teams__(data)
    data <- convert_character_variables_to_factors__(data)
    return(data)
}

impute <- function(data) {
    classes <- sapply(data, class)
    char_vars <- colnames(data)[classes %in% c('character', 'factor')]
    num_vars <- colnames(data)[classes %in% c('integer', 'numeric')]
    if (length(num_vars) + length(char_vars) != ncol(data)) {
        print(setdiff(c(num_vars, char_vars), colnames(data)))
        stop("We're missing some variables")
    }
    for (v in num_vars) {
        data[is.na(data[, v]), v] <- mean(data[, v], na.rm = TRUE)
    }
    for (v in char_vars) {
        data[, v] <- as.character(data[, v])
        tt <- table(data[, v])
        data[is.na(data[, v]), v] <- names(tt[which.max(tt)])
        data[, v] <- factor(data[, v])
    }
    return(data[complete.cases(data), ])
}

separate <- function(data) {
    n_for_prop <- floor(nrow(data) * TRAINING_PROPORTION)
    sample_ <- sample(1:nrow(data), n_for_prop)
    return(list(
        "train" = data[ sample_, ],
        "test" =  data[-sample_, ],
        "full" = data
    ))
}

clean_non_whitelisted_character_variables__ <- function(data) {
    writeLines("\nCleaning non-whitlisted character variables...\n")
    for (var in non_whitelisted_character_variables__(data)) {
        data <- fix_monetary_issues__(data, var)
        data <- fix_percent_issues__(data, var)
        data <- convert_to_numeric__(data, var)
    }
    return(data)
}

fix_monetary_issues__ <- function(data, var) {
    data[, var] <- gsub("\\$", "", data[, var])
    data[, var] <- gsub(",", "", data[, var])
    return(data)
}

fix_percent_issues__ <- function(data, var) {
    data[, var] <- gsub("%", "", data[, var])
    return(data)
}

convert_to_numeric__ <- function(data, var) {
    #
    # Some variables were not automatically loaded as numeric because they have
    # non-numeric values for some observations. For example, `ES_A.60`,
    # `ES_BLK.`, and others, have "#DIV/0!" as one of its values (probably to
    # scraping error?). All these non-numeric values are transformed to NAs.
    # There were other issues as well.
    #
    # Since there are various values that, when transformed to numeric, don't
    # have valid representations (e.g. "DIV/0!"), we get an "NAs introduced by
    # coercion" warning, which is annoying. To get rid of it temporarly (only
    # while this function executes), we use the `options()` function with the
    # `warn` param.
    #
    original_warning_level <- getOption("warn")
    options(warn = -1)
    data[, var] <- as.numeric(data[, var])
    options(warn = original_warning_level)
    return(data)
}

non_whitelisted_character_variables__ <- function(data) {
    char_vars <- sapply(data, class) == 'character'
    char_vars <- names(char_vars[char_vars])
    return(setdiff(char_vars, VALID_CHAR_VARS))
}

keep_variables_with_acceptable_na_proportions__ <- function(data) {
    writeLines("\nKeeping variables with acceptable NA proportions...\n")
    keep <- NULL
    props <- proportion_of_nas_per_variable(data)
    for (i in 1:length(props)) {
        if ((names(props[i]) %in% NA_VAR_WHITELIST) ||
            props[i] <= NA_VAR_PROPORTION_THRESHOLD) {
            keep <- c(keep, i)
        }
    }
    return(data[, keep])
}

keep_observations_with_acceptable_na_proportions__ <- function(data) {
    writeLines("\nKeeping observations with acceptable NA proportions...\n")
    keep <- NULL
    props <- proportion_of_nas_per_observation(data)
    for (i in 1:length(props)) {
        if (props[i] <= NA_OBS_PROPORTION_THRESHOLD) {
            keep <- c(keep, i)
        }
    }
    return(data[keep, ])
}

keep_observations_with_non_nas_for_salary__ <- function(data) {
    writeLines("\nKeeping observations non-NA salary...\n")
    return(data[!is.na(data$Salary), ])
}

proportion_of_nas_per_variable <- function(data) {
    return(colMeans(is.na(data)))
}

proportion_of_nas_per_observation <- function(data) {
    return(rowMeans(is.na(data)))
}

disaggregate_due_to_multiple_teams__ <- function(data) {
    #
    # Some team observations have multiple values separated by "/" (e.g.
    # "DET/FLA"), while others use ", " (e.g. "BOS, NYR"), which creates
    # problems when being parsed. To avoid those problems we make subsitutute
    # globally all ", " with "/", and theen we can assume a single separator. To
    # make sure these are all the available separators in the data, we use the
    # fact that the teams are represented by three leters, so any team value
    # with more than three characterse should include a separator, the results
    # that will be changed, are printed. This process moves the data from 1,755
    # to 1,824 observations.
    #
    # Since this process is expensive, I added a progress bar while it runs.
    # After I applied some optimizations the code ran much faster, but I left
    # the progress bar anyways so that you can see how that works.
    #
    if (!("Team" %in% colnames(data))) {
        return(data)
    }
    writeLines("\nDisaggregating due to players with multiple teams...\n")
    select <- unlist(lapply(data$Team, function(x) { return(nchar(x) > 3) }))
    print(unique(data[select, "Team"]))
    data$Team <- data$Team <- gsub(", ", "/", data$Team)
    n <- nrow(data)
    obs_to_delete <- NULL
    obs_to_add <- data.frame()
    #
    # Commented out progress to avoid too much output on knitted PDF
    #
    # pb <- progress_bar$new(total = n)
    #
    for (i in 1:n) {
        # pb$tick()
        team_split <- strsplit(data[i, "Team"], "/")[[1]]
        if (length(team_split) > 1) {
            obs_to_delete <- c(obs_to_delete, i)
            obs <- data[i, ]
            for (team in team_split) {
                obs$Team <- team
                obs_to_add <- rbind(obs_to_add, obs)
            }
        }
    }
    #
    # We avoid changing the data while processing the teams because it leads to
    # bugs due to non-synced data. That's why we apply all mutating operations
    # here, after the lookout process has finished.
    #
    data <- data[-obs_to_delete, ]
    data <- rbind(data, obs_to_add)
    return(data)
}

convert_character_variables_to_factors__ <- function(data) {
    char_vars <- colnames(data)[sapply(data, class) %in% c('character')]
    for (v in char_vars) {
        data[, v] <- factor(data[, v])
    }
    return(data)
}
