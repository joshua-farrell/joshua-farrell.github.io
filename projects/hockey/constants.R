
TOP_N_VAR_IMPORTANCE <- 50
TRAINING_PROPORTION <- 0.7
CORRELATION_CUTOFF <- 0.7

GRAPH_WIDTH <- 1000
GRAPH_HEIGHT <- 1000
GRAPH_POINTSIZE <- 12


#
# These are the maximum NA proportions allowed. The higher these numbers the
# more variables and observations that will be kept in the analysis, and
# viceversa.
#

NA_OBS_PROPORTION_THRESHOLD <- 0.5
NA_VAR_PROPORTION_THRESHOLD <- 0.5

#
# These are teh variables that are used to identify a observation
#

ID_VARS <- c(
    "Year",
    "Last.Name",
    "First.Name",
    "Position",
    "Team"
)

#
# This is the list of variables that are valid character variables. If a
# variable is imported by R as non-numeric and is not in this list, it will be
# coerced into numeric erronously, so this list must be updated with all valid
# character variables.
#

VALID_CHAR_VARS <- c(
    ID_VARS,
    "TOT_Status",
    "TOT_Cntry",
    "TOT_City"
)

#
# These variables are non-informative for the predictive models, so they are
# removed. In the case of `City` it could be somewhat useful but it has so many
# categories that it makes it harder to be useful (the ratio of number of
# categories to number of observations is very high). We could try converting to
# some kind of numeric variable where we account for a price-index per city to
# make this variable useful, but that's out of the scope for this analysis.
#



UNNECESSARY_VARS <- c(
    "First.Name",
    "Last.Name",
    "TOT_H.Ref.Name",
    "TOT_CorsicaID",
    "TOT_Helmet",
    "TOT_Skates",
    "TOT_Stick",
    "TOT_Pants",
    "TOT_Glove",
    "TOT_City",
    "TOT_Cap.Hit",
    "TOT_NHLid",
    "Team",
    "TOT_CHIP"
)

#
# These are the strings found so far in the data which should be imported as
# NAs. The first four are standard NA characters, but the all of the ones after
# those are the ones actually found in these datasets. If more are found, it's
# better to add them here than to try to handle them during the data cleaning.
#

NA_STRINGS <- c(
    "NA",
    "NaN",
    "",
    " ",
    "#DIV/0!",
    "#VALUE!",
    "-Inf",
    "Inf"
)

#
# These are variables that do not need a prefix according to the file
#

NON_PREFIX_VARS <- c(
    ID_VARS,
    "Salary"
)

#
# These are the variables that will not be removed through the data cleaning
# process even if they have a proportion of NAs larger than the treshold defined
# in the the `NA_VAR_PROPORTION_THRESHOLD` parameter in this file.
#

NA_VAR_WHITELIST <- c(
    ID_VARS,
    "Salary"
)
