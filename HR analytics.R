#### HR analytics/people analytics ###

#three steps: 
#1) comparing groups: high vs low performer; high vs low retention rate
#2) calculate summary stats
#3) how big the differences between two groups, visualization


### Case: which source of new candidates ultimately produces the best new hires
#1.by looking at the sales quota percentage
library(dplyr)
avg_sale <- recruitment %>%
  group_by(recruiting_source) %>%
  summarize(avg_sale_quota_pct = mean(sale_quota_pct))
  
#2. by looking at the attrition rate
# Find the average attrition for the sales team, by recruiting source, sorted from lowest attrition rate to highest
avg_attrition <- recruitment %>%
  group_by(recruiting_source) %>% 
  summarize(attrition_rate = mean(attrition)) %>% 
  arrange(attrition_rate)

#visualizating
#bar chart
library(ggplot2)
ggplot(avg_sales, aes(x=recruiting_source, y = avg_sales_quota_pct)) +
  geom_col()
 

### case: employee engagement
#mutate(),ifelse(),summarize()
survey %>%
  mutate(vacations = ifelse(vacation_days >10, "Yes", "No"))
# Load the packages
library(readr)
library(dplyr)

# Import the data
survey <- read_csv("survey_data.csv")

# Get an overview of the data
summary(survey) #not providing info about character variable

# Examine the counts of the department variable
survey %>%
  count(department)

# Output the average engagement score for each department, sorted
survey %>%
  group_by(department) %>%
  summarize(avg_engagement = mean(engagement)) %>%
  arrange(avg_engagement)

# Create the disengaged variable and assign the result to survey
survey_disengaged <- survey %>% 
  mutate(disengaged = ifelse(engagement <= 2, 1, 0)) 

# Summarize the three variables by department
survey_summary <- survey_disengaged %>%
  group_by(department) %>%
  summarize(pct_disengaged = mean(disengaged),avg_salary = mean(salary),
            avg_vacation_days = mean(vacation_days_taken))

#visualization 
library(tidyr)
library(ggplot2)

# Gather data for plotting
survey_gathered <- survey_summary %>% 
  gather(key = "measure", value = "value",
         pct_disengaged, avg_salary, avg_vacation_taken)

# Create three facets bar charts side by side
ggplot(survey_gathered, aes(x= measure, y= value, fill=department)) +
  geom_col(position = "dodge") + 
  facet_wrap(~ measure,scales ="free")


#### t-test, chi-square
# test whether sales department vs others has significantly high disengagement rate.
survey_sales <- survey %>%
  mutate(in_sales = ifelse(department == "Sales", "Sales", "Other"))

# Test the hypothesis using survey_sales (disengaged is dichotomous variable)
library(broom)
chisq.test(survey_sales$in_sales, survey_sales$disengaged)  %>%
  tidy()  %>% #return the stats in a dataframe with col names
  pull("p values")
  

# Test whether employees in the sales department take fewer vacation days on average than the rest of the company. 
t.test(vacation_days_taken ~ in_sales, data = survey_sales)



### missing variable bias that correlate with both dependent and independent variable. that can explain why these two connected
#check group compositions
# Create a stacked bar chart
ggplot(pay, aes(x= new_hire,fill = job_level)) +
  geom_bar() #just count of new_hire in different job levels

ggplot(pay, aes(x = new_hire, fill = job_level)) +
  geom_bar(position = "fill") #fill with 100%

#Do new hires have a higher average salary than current employees when job level is taken into account?
# Calculate the average salary for each group of interest
pay_grouped <- pay %>% 
  group_by(new_hire, job_level) %>% 
  summarize(avg_salary = mean(salary))

# Graph the results using facet_wrap()  
pay_grouped %>% 
  ggplot(aes(x = new_hire, y = avg_salary)) +
  geom_col()+
  facet_wrap(~job_level)


### Run the simple regression
model_simple <- lm(salary ~ new_hire, data = pay) %>% 
  summary()

### Run logistic regression
glm(high_performer ~ gender + job_level, family = "binomial", data = performance) %>%
  summary()





###### Joining HR datasets

#performance rating
# Visualize the distribution of all ratings by gender
ggplot(performance,aes(x=gender, fill = factor(rating))) +
  geom_bar(position = "fill")


### Case study: why accident rate is increasing?

# Load the packages
library(readr)
library(dplyr)
hr_data <- read_csv("hr_data_2.csv")
accident_data <- read_csv("accident_data.csv")

# Create hr_joined with left_join() and create new variable "had_accident"with NA as 0 and others as 1
hr_joined <- hr_data %>%
  left_join(accident_data, by = c("year","employee_id")) %>% 
  mutate(had_accident = ifelse(is.na(accident_type), 0,1))

#did the accident rate increase from 2016 to 2017?
# Find accident rate for each year
hr_joined %>% 
  group_by(year) %>% 
  summarize(accident_rate = mean(had_accident))

# Test difference in accident rate between years
chisq.test(hr_joined$year, hr_joined$had_accident)

# Which location had the highest acccident rate?
hr_joined %>%
  group_by(location) %>%
  summarize(accident_rate = mean(had_accident)) %>%
  arrange(desc(accident_rate))

#Where did the accident rate increase most?
# Compare annual accident rates by location
accident_rates <- hr_joined %>% 
  group_by(location, year) %>% 
  summarize(accident_rate = mean(had_accident))

# Graph it
accident_rates %>% 
  ggplot(aes(factor(year), accident_rate)) +
  geom_col() + #remember to use geom_col, not geom_bar()
  facet_wrap(~location)

#find factors relating to the increasing accident rate
# Import the survey data
survey_data <- read_csv("survey_data_2.csv")
# Create the safety dataset
safety <- hr_joined %>% 
  left_join(survey_data, by = c("year", "employee_id")) %>%
  mutate(disengaged = ifelse(engagement < 3, 1, 0), year = factor(year))  #use mutate()to update existing variable


# Visualize the difference in % disengaged by year in Southfield
southfield %>% 
  ggplot(aes(x = year, fill = factor(disengaged))) +
  geom_bar(position = "fill") #geom_bar goes with fill

# Test whether one year had significantly more disengaged employees
chisq.test(southfield$year, southfield$disengaged)

