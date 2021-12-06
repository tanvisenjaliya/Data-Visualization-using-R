## Analysis of the H1B visa petitions made.

## PART 1: Basic Analysis.

### We'll start by loading the necessary libraries.

library(readr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(dplyr)
library(VIM)
library(tmap)  
library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(leaflet)

### Get input.

visa <- read_csv("C:/Users/TANVI/AI_ML/DSDA_Sem1/PDS/h1b_kaggle.csv")
visa <- data.frame(visa)
head(visa)

### Changing some data types and removing a few columns

visa$...1 <- NULL
#visa$lon <- NULL
#visa$lat <- NULL
visa$CASE_STATUS[visa$CASE_STATUS == 
                   "PENDING QUALITY AND COMPLIANCE REVIEW - UNASSIGNED"] <- "PENDING"
visa$CASE_STATUS <- factor(visa$CASE_STATUS)
visa$YEAR <- factor(visa$YEAR)
visa$FULL_TIME_POSITION <- factor(visa$FULL_TIME_POSITION)

summary(visa)

#######################################################################
### Dealing with missing values

colSums(is.na(visa))

visa = visa[is.na(visa$YEAR) == F,]
#visa = visa[is.na(visa$PREVAILING_WAGE) == F,]
colSums(is.na(visa))


getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fill_na_position = getmode(visa$FULL_TIME_POSITION)
fill_na_position
visa$FULL_TIME_POSITION[is.na(visa$FULL_TIME_POSITION)] = fill_na_position
colSums(is.na(visa))


# use mean for replacing missing value in PREVAILING_WAGE
fill_na_wage = mean(visa$PREVAILING_WAGE,na.rm = T)
fill_na_wage
visa$PREVAILING_WAGE[is.na(visa$PREVAILING_WAGE)] = fill_na_wage
colSums(is.na(visa))


### Again looking at the summary

summary(visa)
head(visa)

#################################################################

# Outliers

str(visa)
# There is only one numeric column - Prevailing wage

boxplot(visa$PREVAILING_WAGE)
# We observe that the max value is way higher than other values, hence we shall remove it first
visa = visa[!visa$PREVAILING_WAGE == max(visa$PREVAILING_WAGE),]
dim(visa) # max value removed

# Checking boxplot again
boxplot(visa$PREVAILING_WAGE)
count(visa[visa$PREVAILING_WAGE > 3*10^8,]) #26 values
visa = visa[!(visa$PREVAILING_WAGE > 3*10^8),]

boxplot(visa$PREVAILING_WAGE)

summary(visa)

#################################################################
### Looks better ! Now the analysis part. We'll go feature by feature.

### The next features is EMPLOYER_NAME. It is the company which submits the application for its employee. Let us look at the top 15 recruiters.

# set colors
mycolors <- c("#FF7F11","#058C42","#FF3F00","#5D2E8C","#590925","#581908","#B80C09",
              "#276FBF","#337357","#B6D7B9","#8338EC","#0F4C5C","#FB8B24","#E16036",
              "#420039","#7A8B99","#8DB580","#00B295","#502419","#BB7E5D")


#visa$EMPLOYER_NAME <- factor(visa$EMPLOYER_NAME)
top_employer <- as.data.frame(visa %>% group_by(EMPLOYER_NAME) %>%
                                summarise(count = n(), percent = round(count*100/nrow(visa),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(15, wt = count))

ggplot(data = top_employer, aes(x = reorder(EMPLOYER_NAME, percent),
                                y = percent, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
  labs(x = "EMPLOYER_NAME", y = "Petitions Made(in percentage)") + 
  scale_fill_manual(values = mycolors) + 
  theme(legend.position = "none") +
  coord_flip()
#visa$EMPLOYER_NAME <- as.character(visa$EMPLOYER_NAME)


### The top three companies are Indian.

### Note: In the above graph the cumulative percentage is not 100 because there are a lot of other companies which have not been shown. The graph shows only the first 15 companies.

### The next feature is JOB_TITLE. Let us look at top 15 Job positions for which petitions are made.


#visa$JOB_TITLE <- factor(visa$JOB_TITLE)
top_employer <- as.data.frame(visa %>% group_by(JOB_TITLE) %>%
                                summarise(count = n(), percent = round(count*100/nrow(visa),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(15, wt = count))

ggplot(data = top_employer, aes(x = reorder(JOB_TITLE, percent),
                                y = percent, fill = JOB_TITLE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + 
  labs(x = "JOB TITLE", y = "Petitions Made(in percentage)") + 
  scale_fill_manual(values = mycolors) +
  theme(legend.position = "none") +
  coord_flip()

#visa$JOB_TITLE <- as.character(visa$JOB_TITLE)


### "Analyst Programmer" is the most famous job. Moreover, the top 10 jobs as we can see in the graph are purely technical jobs.


## PART 2: DATA SCIENCE JOBS

### Filter data science jobs


data_job_pattern <- "^DATA SCIENTIST*"
data_jobs <- subset(visa, grepl(data_job_pattern, toupper(visa$JOB_TITLE)) == T)
str(data_jobs)
#data_jobs$WORKSITE <- factor(data_jobs$WORKSITE)


### Now, the salary distribution of data Science jobs.


ggplot(data = subset(data_jobs, data_jobs$PREVAILING_WAGE < 
                       quantile(data_jobs$PREVAILING_WAGE,0.999)),
       aes(PREVAILING_WAGE/1000)) + 
  geom_histogram(color = "black", fill = mycolors[11], binwidth = 2.5) + 
  scale_x_continuous(breaks = seq(0,150,10)) +
  labs(x = "Salary (in thousand USD)", y = "Number of Data Science jobs",
       title = "Data Scientists' Salary Distribution")

summary(data_jobs$PREVAILING_WAGE)

### Normally distributed. Median salary is around USD 90k.

### Salary of data science jobs and number of petitions made with time.


ds_wage <- data_jobs %>% group_by(YEAR) %>% 
  summarise(median_salary = median(PREVAILING_WAGE), count = n())

ggplot(data = ds_wage, aes(x = as.numeric(as.character(YEAR)), y = median_salary)) +
  geom_line() +
  geom_point() +
  labs(x = "YEAR", y = "Median Salary(in USD)", title = "Data Scientists' salary trend")

ggplot(data = ds_wage, aes(x = as.numeric(as.character(YEAR)), y = count)) +
  geom_line() +
  geom_point() +
  labs(x = "YEAR", y = "Petitions made", title = "Data Scientists' job petitions")


### The median salary stays around USD 90k with a little decrease over the years. But it is still around USD 90k. However, a clear upward trend can be seen in number of petitions made each year.


### EMPLOYERS: let's see who provides more salary and more jobs in data science field.


#data_jobs$EMPLOYER_NAME <- factor(data_jobs$EMPLOYER_NAME)

top_employer_count <- data_jobs %>% group_by(EMPLOYER_NAME) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15, wt = count)

ggplot(data = top_employer_count, aes(x = reorder(EMPLOYER_NAME, count),
                                      y = count, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity") +
  labs(x = "EMPLOYER", y = "Number of Data Scientist",
       title = "Top Data Science Employers (in terms of petitions made)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,150,15)) +
  coord_flip()

top_employer_salary <- data_jobs %>% group_by(EMPLOYER_NAME) %>% 
  summarise(median_wage = median(PREVAILING_WAGE)) %>%
  arrange(desc(median_wage)) %>%
  top_n(15, wt = median_wage)

ggplot(data = top_employer_salary, aes(x = reorder(EMPLOYER_NAME, median_wage),
                                       y = median_wage/1000, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity") +
  labs(x = "EMPLOYER", y = "Median Wage (in USD)",
       title = "Top Data Science Employers (in terms of salary offered)") +
  geom_text(aes(label = paste0("$",median_wage)), hjust = 1.2) +
  theme(legend.position = "none", axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  coord_flip()


pie(x = top_employer_count$count, 
    labels = top_employer_count$EMPLOYER_NAME,
    radius = 1.5,
    main = "Top Data Science Employers (in terms of petitions made)", 
    col = mycolors)

### Microsoft and Facebook are way ahead in filing petitions for Data Scienctist jobs. On the other hand some not so familiar companies are paying the highest salaries in the field.

### Workplace: What are the most popular work places for data scientists ?


#data_jobs$WORKSITE <- factor(data_jobs$WORKSITE)

top_worksite_count <- data_jobs %>% group_by(WORKSITE,lat,lon) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15, wt = count)

ggplot(data = top_worksite_count, aes(x = reorder(WORKSITE, count),
                                      y = count, fill = WORKSITE)) +
  geom_bar(stat = "identity") + 
  labs(x = "CITY", y = "Number of Data Scientists",
       title = "TOP Work Locations (in terms of petitions made)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,120000,15000)) +
  coord_flip()

top_worksite_salary <- data_jobs %>% group_by(WORKSITE) %>%
  summarise(median_wage = median(PREVAILING_WAGE)) %>%
  arrange(desc(median_wage)) %>%
  top_n(15, wt = median_wage)

ggplot(data = top_worksite_salary, aes(x = reorder(WORKSITE, median_wage),
                                       y = median_wage, fill = WORKSITE)) +
  geom_bar(stat = "identity") + 
  labs(x = "CITY", y = "MEDIAN SALARY",
       title = "TOP Work Locations (in terms of salary offered)") +
  geom_text(aes(label = paste0("$",median_wage)), hjust = 1.2) +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_flip()

top_worksite_count

top_worksite_count %>% leaflet() %>% addProviderTiles('CartoDB') %>%
  addCircleMarkers(radius = 2) 

#tm_shape(us_states) + tm_polygons(col = top_worksite_count$WORKSITE, pallette = "BuGn")

### The most popular and highest paid location is undoubtedly San Francisco, California.
