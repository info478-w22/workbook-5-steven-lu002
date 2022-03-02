# Workbook 6: analyze NHANES data

# Set up
library(survey)
library(Hmisc)

demo <- sasxport.get("DEMO_I.xpt")
alco <- sasxport.get("ALQ_I.xpt")

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = TRUE)

# represent us population (sample weight is a proportion and we are adding all of the proportions at the time)
wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)

## Analysis
## In ALQ151, we want No to be 0 and we want to ignore/disregard 7 and 9
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

# Create a survey design
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

# 17% of people have more than 4/5 drinks per day
nhanes_mean <- svymean(
  ~alq151, nhanes_survey, na.rm = TRUE
)

# 23% are male, 10% are female.
mean_by_gender <- svyby(
  ~alq151,
  ~riagendr, 
  nhanes_survey, 
  svymean,
  na.rm = TRUE
)