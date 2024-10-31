# load libraries
library(tidyverse)  # data wrangling
library(modelr)  # add_predictions()

### STEP 1 - DATA WRANGLING

# 1a. Read data on COVID-19 from Our World in Data
Covid_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

# Download the Population CSV file and rename it to population_data.csv, then read it
Population_data <- read_csv("population_data.csv")


# 1b. Drop any data that is not country-level
Covid_data <- Covid_data %>% filter(nchar(iso_code) == 3)


# 1c. Drop any countries with populations smaller than 1,000,000
Covid_data <- Covid_data %>% filter(population >= 1000000)


# 1d. Create a new column new_deaths_smoothed_2wk
# First, create copy of Covid_data with only iso_code + date (primary key) AND new_deaths_smoothed;
# this copy will be a reference table for the values of new_deaths_soothed_2wk
Covid2 <- Covid_data %>% select(iso2 = iso_code, date2 = date, new_deaths_smoothed_2wk = new_deaths_smoothed)

# Then, in original Covid_data, create a new column called date_2wk, which is = date + 2 weeks
Covid_data <- Covid_data %>% mutate(date_2wk = date + 14)

# Now that each row has a date_2wk referencing the date that comes 2 weeks later,
# we can use an inner join between Covid_data and Covid2, where date_2wk will be matched with date2.
Covid_data <- Covid_data %>% inner_join(Covid2, by=c(iso_code="iso2", date_2wk="date2"))
# drop the date_2wk variable, as it is not needed anymore
Covid_data <- Covid_data %>% select(-"date_2wk")


# 1e. Tidy population data
# First, remove series description
Population_data <- Population_data %>% select(-"Series Name")
# Then, use pivot wider: each series variable should have its own column
Population_data <- Population_data %>% pivot_wider(names_from = "Series Code", values_from="2023 [YR2023]")


# 1f. Merge the tables and drop Country Name, since it is not needed anymore
Project_data <- Covid_data %>% inner_join(Population_data, by=c(iso_code="Country Code"))
Project_data <- Project_data %>% select(-"Country Name")


# 1g. Create scatterplots
# Isolate “most recent” data for the scatterplots (one day per Country)
Recent_data <- Project_data %>% filter(date == "2023-06-30")

# Create scatterplot 1: recent new deaths two weeks ahead vs. new cases per day
ggplot(data=Recent_data) +
geom_point(mapping=aes(x=new_cases_smoothed, y=new_deaths_smoothed_2wk)) +
labs(x="new_cases_smoothed", y="new_deaths_smoothed, 2 weeks away")

# Create scatterplot 2: recent new deaths vs. total population over 80
# First, calculate the total population of people over 80 in every country
Recent_data <- Recent_data %>% mutate(SP.POP.80UP.TOTL = as.numeric(SP.POP.80UP.FE) + as.numeric(SP.POP.80UP.MA))

# Then, create the scatterplot
ggplot(data=Recent_data) +
geom_point(mapping=aes(x=SP.POP.80UP.TOTL, y=new_deaths_smoothed)) +
labs(x="Population over 80 years (SP.POP.80UP.FE + SP.POP.80UP.MA)", y="new_deaths_smoothed")



### STEP 2 - LINEAR MODELING

# 2a. List all predictor variables
# Remove any dependent (non-predictor) variables or identity (e.g., country name, date) variables.
# Then, list the remaining variables (column names) to get all predictor variables.
Project_data %>% select(-iso_code,-continent, -location,-date,-contains("deaths")) %>% colnames()


# 2b. Generate transformed variables
# T variable 1: total_cardiovasc_deaths
Project_data <- Project_data %>% mutate(total_cardiovasc_deaths = cardiovasc_death_rate/100000*population)

# T variable 2: total_reproduction
Project_data <- Project_data %>% mutate(total_reproduction = reproduction_rate/100000*population)

# T variable 3: new_positive_tests_smoothed
Project_data <- Project_data %>% mutate(new_positive_tests_smoothed = new_tests_smoothed*positive_rate)

# T variable 4: total_smokers
Project_data <- Project_data %>% mutate(total_smokers = (female_smokers + male_smokers)/2/100*population)

# T variable 5: total_urban_population
Project_data <- Project_data %>% mutate(total_urban_population = as.numeric(SP.URB.TOTL.IN.ZS)/100*population)


# 2c. Split the dataset into train and test subsets
# Create the Training data set (only from 2022)
Train_data <- Project_data %>% filter(year(date) == 2022)

# Create the Testing data set (Jan - Jun 2023)
Test_data <- Project_data %>% filter(year(date) == 2023, month(date) >= 1, month(date) <= 6)


# 2d. Create linear models
# Create model 1: based on cases
model_cases <- Train_data %>%
# First, filter out data with missing predictor variables or new_deaths_smoothed
filter(!is.na(total_cases), !is.na(new_cases), !is.na(new_cases_smoothed), !is.na(new_deaths_smoothed_2wk)) %>%
# Then, create the linear model
lm(formula = new_deaths_smoothed_2wk ~ total_cases + new_cases + new_cases_smoothed)


# Create model 2: based on hospitals
model_hospitals <- Train_data %>%
# Filter out missing data
filter(!is.na(icu_patients), !is.na(hosp_patients), !is.na(weekly_hosp_admissions), !is.na(new_deaths_smoothed_2wk)) %>%
# Then, create the linear model
lm(formula = new_deaths_smoothed_2wk ~ icu_patients + hosp_patients + weekly_hosp_admissions)


# Create model 3: based on old age demographics and population fluctuations
model_age_demographics <- Train_data %>%
# Filter out missing data
filter(!is.na(reproduction_rate), !is.na(median_age), !is.na(aged_65_older), !is.na(aged_70_older), !is.na(excess_mortality_cumulative_absolute ), !is.na(new_deaths_smoothed_2wk)) %>%
# Then, create the linear model
lm(formula = new_deaths_smoothed_2wk ~ reproduction_rate + median_age + aged_65_older + aged_70_older + excess_mortality_cumulative_absolute + as.numeric(SP.POP.80UP.FE) + as.numeric(SP.POP.80UP.MA))


# Create model 4: based on vaccinations/boosters
model_vaccines <- Train_data %>%
# Filter out missing data
filter(!is.na(total_vaccinations), !is.na(people_vaccinated), !is.na(people_fully_vaccinated), !is.na(total_boosters), !is.na(new_vaccinations), !is.na(new_vaccinations_smoothed), !is.na(new_people_vaccinated_smoothed), !is.na(new_deaths_smoothed_2wk)) %>%
# Then, create the linear model
lm(formula = new_deaths_smoothed_2wk ~ total_vaccinations + people_vaccinated + people_fully_vaccinated + total_boosters + new_vaccinations + new_vaccinations_smoothed + new_people_vaccinated_smoothed)


# Create model 5: based on living conditions
model_living <- Train_data %>%
# Filter out missing data
filter(!is.na(population_density), !is.na(gdp_per_capita), !is.na(extreme_poverty), !is.na(life_expectancy), !is.na(human_development_index), !is.na(new_deaths_smoothed_2wk)) %>%
# Then, create the linear model
lm(formula = new_deaths_smoothed_2wk ~ population_density + gdp_per_capita + extreme_poverty + life_expectancy + human_development_index + as.numeric(SP.URB.TOTL.IN.ZS) + as.numeric(SP.POP.DPND))



### STEP 3 - EVALUATIONS

# 3a. calculate the R2 and RMSE of all models using Test data

# First, read the R2 values of all models
summary(model_cases)
summary(model_hospitals)
summary(model_age_demographics)
summary(model_vaccines)
summary(model_living)

# Next, read the RMSE values of all models
rmse(model_cases, Test_data)
rmse(model_hospitals, Test_data)
rmse(model_age_demographics, Test_data)
rmse(model_vaccines, Test_data)
rmse(model_living, Test_data)

# These results show that model_hospitals is the best model


# 3b. calculate the RMSE of the best model for all countries
Test_data %>% group_by(location) %>% summarise(population = max(population), rmse = rmse(model=model_hospitals, data=cur_data())) %>% arrange(-population)

# This table shows that model_hospitals is only useful for the United States!

# calculate the RMSE of the next best model for all countries
Test_data %>% group_by(location) %>% summarise(population = max(population), rmse = rmse(model=model_cases, data=cur_data())) %>% arrange(-population)

# This table shows that model_cases is useful for all countries!