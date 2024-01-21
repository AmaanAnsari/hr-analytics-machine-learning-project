args_default_list <- list('args_clean_workspace' = T, 'args_do_downsampling' = F,
                           'args_seed' = 123)
source("src/utils/enable_arguments.R")

library(dplyr)
library(caret)

set.seed(args_seed)
data <- read.csv(paste0(getwd(), "/data/raw/aug_train.csv"))
data[data == ''] <- NA

data$city <- as.factor(data$city)
data$relevent_experience <- as.factor(data$relevent_experience)
data$enrolled_university <- as.factor(data$enrolled_university)
data$education_level <- as.factor(data$education_level)
data$major_discipline <- as.factor(data$major_discipline)
data$gender <- as.factor(data$gender)
# data$experience <- as.factor(data$experience) # in numerisch umwandeln
# data$company_size <- as.factor(data$company_size) # in numerisch umwandeln
data$last_new_job <- as.factor(data$last_new_job)
data$company_type <- as.factor(data$company_type)
data$target <- as.factor(data$target)


data$company_size <- as.character(data$company_size)
data$company_size[data$company_size == "<10"] <- "small"
data$company_size[data$company_size == "10/49"] <- "small"
data$company_size[data$company_size == "50-99"] <- "small"
data$company_size[data$company_size == "100-500"] <- "medium"
data$company_size[data$company_size == "500-999"] <- "medium"
data$company_size[data$company_size == "1000-4999"] <- "large"
data$company_size[data$company_size == "5000-9999"] <- "large"
data$company_size[data$company_size == "10000+"] <- "large"
data$company_size[is.na(data$company_size)] <- "none"
data$company_size <- as.factor(data$company_size)
#data$company_size <- as.numeric(data$company_size)

data$experience <- as.character(data$experience)
data$experience[data$experience == "<1"] <- "0"
data$experience[data$experience == ">20"] <- "21"
data$experience <- as.numeric(data$experience)

data$gender <- as.character(data$gender)
data$gender[is.na(data$gender)] <- "Other"
data$gender <- as.factor(data$gender)

data$major_discipline <- as.character(data$major_discipline)
data$major_discipline[data$education_level == "High School" & is.na(data$major_discipline)] <- "No Degree"
data$major_discipline[data$education_level == "Primary School" & is.na(data$major_discipline)] <- "No Degree"
data$major_discipline <- as.factor(data$major_discipline)

data$education_level <- as.character(data$education_level)
data$major_discipline <- as.character(data$major_discipline)
major_discipline_name <- "No Degree"
data$education_level[data$enrolled_university == "no_enrollment" & is.na(data$education_level) & is.na(data$major_discipline)] <- major_discipline_name
data$major_discipline[data$enrolled_university == "no_enrollment" & data$education_level == major_discipline_name & is.na(data$major_discipline)] <- major_discipline_name
data$education_level <- as.factor(data$education_level)
data$major_discipline <- as.factor(data$major_discipline)

# data <- data %>%
#   filter(enrolled_university == "no_enrollment" & is.na(education_level) & is.na(major_discipline)) %>%
#   mutate(education_level = "No Degree", major_discipline = "No Degree")
# test <- data$enrolled_university[data$enrolled_university == "no_enrollment" & is.na(data$education_level) & is.na(data$major_discipline)]

data$last_new_job <- as.character(data$last_new_job)
data$last_new_job[data$last_new_job == ">4"] <- "5"
data$last_new_job[data$last_new_job == "never"] <- "0"
data$last_new_job <- as.numeric(data$last_new_job)

data$company_type <- as.character(data$company_type)
data$company_type[is.na(data$company_type)] <- "Other"
data$company_type <- as.factor(data$company_type)

level_counts <- count(data, data$city)
sorted_levels <- level_counts[order(-level_counts$n), ]
topX <- 10

data$city <- as.character(data$city)
data$city[!data$city %in% sorted_levels$"data$city"[1:topX]] <- "Other"
data$city <- as.factor(data$city)


data <- data %>%
  select(-enrollee_id) %>%
  na.omit()



na_rows <- data[!complete.cases(data), ]

#baum <-sample(c("1","0"), size = 100, replace = FALSE, prob = c(0.5,0.5))

one_releativ_ratio <- as.numeric(table(data$target)["1"] /  (table(data$target)["0"] + table(data$target)["1"]))
split <- sample(nrow(data), nrow(data) * 0.8)

train <- data[split,]
test <- data[-split,]
if (args_do_downsampling) {
  train <- downSample(train, train$target) %>%
    select(-Class)
}
one_releativ_ratio <- as.numeric(table(train$target)["1"] /  (table(train$target)["0"] + table(train$target)["1"]))

rm(split)
rm(level_counts)
rm(sorted_levels)
rm(major_discipline_name)

## NAs
# library(naniar)
# vis_miss(data)

