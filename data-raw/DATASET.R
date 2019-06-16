## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")
library(data.table)

# alphabets
all_alphabets <- c(
    'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f',
    'G', 'g', 'H', 'h', 'I', 'i', 'J', 'j', 'K', 'k', 'L', 'l',
    'M', 'm', 'N', 'n', 'O', 'o', 'P', 'p', 'Q', 'q', 'R', 'r',
    'S', 's', 'T', 't', 'U', 'u', 'V', 'v', 'W', 'w', 'X', 'x',
    'Y', 'y', 'Z', 'z'
)
alphabets <- as.data.frame(matrix(all_alphabets, ncol = 2, byrow = T))
colnames(alphabets) <- c('upper', 'lower')
alphabets <- transform(
    alphabets,
    upper = as.character(upper),
    lower = as.character(lower)
)

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




