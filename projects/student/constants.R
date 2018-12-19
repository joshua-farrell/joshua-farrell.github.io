# contant.R contains constancts related to variable labels and response values, 
# tuning parameters and graph parameters etc.


ACCURACY_METRIC <- "Kappa"  # Other option: "Accuracy"
TRAINING_PROPORTION <- 0.7
TOP_N_VAR_IMPORTANCE <- 50
CROSS_VALIDATION_FOLDS <- 3

GRAPH_WIDTH <- 1000
GRAPH_HEIGHT <- 1000
GRAPH_POINTSIZE <- 12

religions__ <- c(
    "Baptist",
    "Buddhist",
    "Church of Christ",
    "Eastern Orthodox",
    "Episcopalian",
    "Hindu",
    "Islamic",
    "Jewish",
    "LDS (Mormon)",
    "Lutheran",
    "Methodist",
    "Presbyterian",
    "Quaker",
    "Roman Catholic",
    "Seventh Day Adventist",
    "United Church of Christ/Congregational",
    "Other Christian",
    "Other Religion",
    "None"
)

careers__ <- c(
    "Accountant or actuary",
    "Actor or entertainer",
    "Architect or urban planner",
    "Artist",
    "Business (clerical)",
    "Business executive (management, administrator)",
    "Business owner or proprietor",
    "Business salesperson or buyer",
    "Clergy (minister, priest)",
    "Clergy (other religious)",
    "Clinical psychologist",
    "College administrator/staff",
    "College teacher",
    "Computer programmer or analyst",
    "Conservationist or forester",
    "Dentist (including orthodontist)",
    "Dietitian or nutritionist",
    "Engineer",
    "Farmer or rancher",
    "Foreign service worker (including diplomat)",
    "Homemaker (full-time)",
    "Interior decorator (including designer)",
    "Lab technician or hygienist",
    "Law enforcement officer",
    "Lawyer (attorney) or judge",
    "Military service (career)",
    "Musician (performer, composer)",
    "Nurse",
    "Optometrist",
    "Pharmacist",
    "Physician",
    "Policymaker/Government",
    "School counselor",
    "School principal or superintendent",
    "Scientific researcher",
    "Social, welfare, or recreation worker",
    "Therapist (physical, occupational, speech)",
    "Teacher or administrator (elementary)",
    "Teacher or administrator (secondary)",
    "Veterinarian",
    "Writer or journalist",
    "Skilled trades",
    "Laborer (unskilled)",
    "Semi-skilled worker",
    "Unemployed",
    "Other",
    "Undecided [student only]"
)

mark__ <- c("Not marked", "Marked")

CATEGORICAL_VARS <- list(
    "SEX" = list(
        "levels" = 1:2,
        "labels" = c("Male", "Female")
    ),
    "ENG_FIRST" = list(
        "levels" = 1:2,
        "labels" = c("No", "Yes")
    ),
    "CITIZEN" = list(
        "levels" = 1:3,
        "labels" = c(
            "Neither",
            "Permanent resident (green card)",
            "U.S. citizen"
        )
    ),
    "PARSTAT" = list(
        "levels" = 1:3,
        "labels" = c(
            "One or both deceased",
            "Both alive, divorced or living apart",
            "Both alive and living with each other"
        )
    ),
    "FINCON" = list(
        "levels" = 1:3,
        "labels" = c(
            "None (I am confident that I will have sufficient funds)",
            "Some (but I probably will have enough funds)",
            "Major (not sure I will have enough funds to complete college"
        )
    ),
    "SRELIGION" = list(
        "levels" = 1:19,
        "labels" = religions__
    ),
    "FRELIGION" = list(
        "levels" = 1:19,
        "labels" = religions__
    ),
    "MRELIGION" = list(
        "levels" = 1:19,
        "labels" = religions__
    ),
    "SCAREER" = list(
        "levels" = 1:47,
        "labels" = careers__
    ),
    "FCAREER" = list(
        "levels" = 1:47,
        "labels" = careers__
    ),
    "MCAREER" = list(
        "levels" = 1:47,
        "labels" = careers__
    ),
    "W_C" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "AA_B" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "AI_AN" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "A" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "NH_PI" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "H" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "PR" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "OH" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "O" = list(
        "levels" = 1:2,
        "labels" = mark__
    ),
    "VIEW_POLITICAL" = list(
        "levels" = 1:5,
        "labels" = c(
            "Far right",
            "Conservative",
            "Middle-of-the-road",
            "Liberal",
            "Far left"
        )
    )
)

self_rating__ <- c(
    "Lowest 10%",
    "Below average",
    "Average",
    "Above average",
    "Highest 10%"
)

education__ <- c(
    "Grammar school or less",
    "Some high school",
    "High school graduate",
    "Postsecondary school other than college",
    "Some college",
    "College degree",
    "Some graduate school",
    "Graduate degree"
)

view__ <- c(
    "Disagree strongly",
    "Disagree somewhat",
    "Agree somewhat",
    "Agree strongly"
)

hours__ <- c(
    "None",
    "Less than one hour",
    "1 to 2 hours",
    "3 to 5 hours",
    "6 to 10 hours",
    "11 to 15 hours",
    "16 to 20 hours",
    "Over 20 hours"
)

reason__ <- c(
    "Not important",
    "Somewhat important",
    "Very important"
)

goal__ <- c(
    "Not important",
    "Somewhat important",
    "Very important",
    "Essential"
)

satisfaction__ <- c(
    "Very Dissatisfied",
    "Dissatisfied",
    "Neutral",
    "Satisfied",
    "Very Satisfied"
)

act__ <- c("Not at all", "Occasionally", "Frequently")

ORDINAL_VARS <- list(
    "AGE" = list(
        "levels" = 1:10,
        "labels" = c(
            "16 or younger",
            "17",
            "18",
            "19",
            "20",
            "21 to 24",
            "25 to 29",
            "30 to 39",
            "40 to 54",
            "55 or older"
        )
    ),
    "DISTANCE" = list(
        "levels" = 1:6,
        "labels" = c(
            "5 or less",
            "6 to 10",
            "11 to 50",
            "51 to 100",
            "101 to 500",
            "Over 500"
        )
    ),
    "HS_GPA" = list(
        "levels" = 1:8,
        "labels" = c("D", "C", "C+", "B-", "B", "B+", "A-", "A or A+")
    ),
    "INCOME" = list(
        "levels" = 1:14,
        "labels" = c(
            "Less than $10,000",
            "$10,000 to $14,999",
            "$15,000 to $19,999",
            "$20,000 to $24,999",
            "$25,000 to $29,999",
            "$30,000 to $39,999",
            "$40,000 to $49,999",
            "$50,000 to $59,999",
            "$60,000 to $74,999",
            "$75,000 to $99,999",
            "$100,000 to $149,999",
            "$150,000 to $199,999",
            "$200,000 to $249,999",
            "$250,000 or more"
        )
    ),
    "SELF_RATING_ACADEMIC_ABILITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_ARTISTIC_ABILITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_COMPUTER_SKILLS" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_COOPERATIVENESS" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_CREATIVITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_DRIVE_TO_ACHIEVE" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_EMOTIONAL_HEALTH" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_LEADERSHIP_ABILITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_MATHEMATICAL_ABILITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_PHYSICAL_HEALTH" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_PUBLIC_SPEAKING_ABILITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_SELFCONFIDENCE_INTELLECTUAL" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_SELFCONFIDENCE_SOCIAL" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_SELFUNDERSTANDING" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_SPIRITUALITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_UNDERSTANDING_OF_OTHERS" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "SELF_RATING_WRITING_ABILITY" = list(
        "levels" = 1:5,
        "labels" = self_rating__
    ),
    "FATHEDUC" = list(
        "levels" = 1:8,
        "labels" = education__
    ),
    "MOTHEDUC" = list(
        "levels" = 1:8,
        "labels" = education__
    ),
    "VIEW_LEGAL_ABORTION" = list(
        "levels" = 1:4,
        "labels" = view__
    ),
    "VIEW_INDIVID_CHANGE" = list(
        "levels" = 1:4,
        "labels" = view__
    ),
    "VIEW_LEGAL_SAME_SEX_MARITAL" = list(
        "levels" = 1:4,
        "labels" = view__
    ),
    "VIEW_RACIAL_DISCRIM_NO_PROBLEM" = list(
        "levels" = 1:4,
        "labels" = view__
    ),
    "HOURS_STUDYING_HOMEWORK" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_SOCIALIZING_WITH_FRIENDS" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_TALKING_WITH_TEACHERS" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_EXERCISE_SPORTS" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_PARTYING" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_WORKING_PAY" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_VOLUNTEER" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_CLUBS_GROUPS" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_TV" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_HOUSE_CHILD_DUTIES" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_READING" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "HOURS_VIDEOGAMES" = list(
        "levels" = 1:8,
        "labels" = hours__
    ),
    "ACT_REL" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_BORED_CLASS" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_DEMONSTATED" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_TUTOR_STUD" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_STUDIED_STUD" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_TEACHERS_HOME" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_SMOKE" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_BEER" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_WINE_LIQUOR" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_OVERWHELMED" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_DEPRESSED" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_VOLUNTEER" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_ASK_TEACHER" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_VOTED_STUD_ELECT" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_SOC_DIVERSE" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_LATE_TO_CLASS" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_INTERNET_RESEARCH" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_COMM_SERVICE" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_DISCUSS_RELIG" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "ACT_DISCUSS_POL" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "REASON_BETTER_JOB" = list(
        "levels" = 1:3,
        "labels" = act__
    ),
    "REASON_GENERAL_ED" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "REASON_CULTURED_PERSON" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "REASON_MONEY" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "REASON_INTEREST" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_RELATIVES_WANTED" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_TEACHER_ADVISED_ME" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_ACADEMIC_REPUTATION" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_SOCIAL_REPUTATION" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_OFFERED_FINANCIAL_ASSISTANCE" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_COST" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_HS_COUNSELOR" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_COLLEGE_COUNSELOR" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_NEAR_HOME" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_NO_AID_FIRST_CHOICE" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_GOOD_JOBS" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_RELIGIOUS" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_SIZE" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_RANK_MAGAZINES" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_WEBSITE" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "CHOOSE_EARLY_ACTION_DECISION" = list(
        "levels" = 1:3,
        "labels" = reason__
    ),
    "GOAL_PERFORMING_ARTS" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_BECOMING_AN_AUTHORITY_IN_MY_FIELD" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_RECOGNITION" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_INFLUENCING_POLITICAL" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_INFLUENCING_SOCIAL" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_FAMILY" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_FINANCIAL" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_HELPING" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_CONTRIBUTION_SCIENCE" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_WRITING" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_ARTISTIC_WORKS" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_OWN_BUSINESS" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_ENVIRONMENT" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_PHILOSOPHY" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_COMMUNITY_ACTION" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_RACIAL_UNDERSTANDING" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_POLITICAL_AFFAIRS" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "GOAL_COMMUNITY_LEADER" = list(
        "levels" = 1:4,
        "labels" = goal__
    ),
    "SAT_INSTRUCTION" = list(
        "levels" = 1:5,
        "labels" = satisfaction__
    ),
    "SAT_COMMUNITY" = list(
        "levels" = 1:5,
        "labels" = satisfaction__
    ),
    "SAT_OVERALL" = list(
        "levels" = 1:5,
        "labels" = satisfaction__
    )
)

label_1__ <- "Dissatisfied/Neutral"
label_2__ <- "Satisfied"

collapse__ <- list(
    "Very Dissatisfied" = 1,
    "Dissatisfied" = 1,
    "Neutral" = 1,
    "Satisfied" = 2,
    "Very Satisfied" = 2
)

COLLAPSED_VALUES <- list(
    "levels" = 1:2,
    "labels" = c(label_1__, label_2__)
)

DEPENDENT_VARS <- list(
    "SAT_INSTRUCTION" = collapse__,
    "SAT_COMMUNITY" = collapse__,
    "SAT_OVERALL" = collapse__
)
