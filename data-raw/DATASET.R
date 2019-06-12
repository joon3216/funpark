## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")
library(data.table)


# rich
set.seed(1024)
is_rich <- rbinom(10, size = 1, prob = .5)
gender <- rbinom(10, size = 1, prob = .5)
criminal_record <- rbinom(10, size = 1, prob = .5)
rich <- data.table(
    gender = ifelse(gender == 1, 'M', 'F'),
    criminal_record = ifelse(criminal_record == 1, 'yes', 'no'),
    is_rich = is_rich
)

# rich_binom
rich_binom <- rich[
    order(gender, criminal_record),
    .(rich = sum(is_rich), not_rich = sum(!is_rich)),
    by = "gender,criminal_record"
]

# rich_pois
rich_pois <- melt(
    rich_binom,
    id.vars = c('gender', 'criminal_record'),
    variable.name = 'is_rich',
    value.name = 'count'
)




