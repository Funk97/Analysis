library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(forcats)


drinksDS <- read_csv("Stats survey.csv")

drinksDS %>% 
  variable.names()


drinksDS_clean <- drinksDS %>% 
  rename(gender = "Your Sex?", 
         collegeYear2023 = "What year were you in last year (2023) ?", 
         grade12GPA = "Your Matric (grade 12) Average/ GPA (in %)", 
         major = "What faculty does your degree fall under?", 
         "2023GPA" = "Your 2023 academic year average/GPA in % (Ignore if you are 2024 1st year student)", 
         accomodationStatus2023 = "Your Accommodation Status Last Year (2023)", 
         monthlyAllowance2023 = "Monthly Allowance in 2023", 
         scholarship2023 = "Were you on scholarship/bursary in 2023?", 
         studyTime = "Additional amount of studying (in hrs) per week", 
         partyTime = "How often do you go out partying/socialising during the week?", 
         drinks = "On a night out, how many alcoholic drinks do you consume?", 
         missedClasses = "How many classes do you miss per week due to alcohol reasons, (i.e: being hungover or too tired?)", 
         failedModules = "How many modules have you failed thus far into your studies?", 
         relationshipStatus = "Are you currently in a romantic relationship?", 
         parentOppions = "Do your parents approve alcohol consumption?", 
         parentRelationship = "How strong is your relationship with your parent/s?") %>% 
  separate(drinks, c("minDrinks", "maxDrinks"), sep = "-", remove = FALSE, convert = TRUE) %>% 
  mutate(minDrinks = as.numeric(str_remove(minDrinks, "\\+"))) %>% 
  select(-Timestamp) %>% 
  as_tibble() 

#counts of Major, College year, 

drinksDS_clean %>% 
  group_by(major) %>% 
  summarise(count = n())

drinksDS_clean %>% 
  group_by(grade12GPA) %>% 
  summarise(count = n()) %>% 
  ggplot() + 
  geom_point(aes(grade12GPA, count)) + 
  labs(x = "Senior Year GPA in %") 

drinksDS_clean %>% 
  count(`2023GPA`) %>% 
  ggplot() + 
  geom_point(aes(`2023GPA`, n)) + 
  labs(x = "Senior Year GPA in %")


drinksDS_clean %>% 
  summary()

#major is heavily skewed to Economic & Management Sciences


drinksDS_clean %>% 
  group_by(collegeYear2023) %>% 
  summarise(count = n())

#College year is skewed to first and second year students

#How many drinks do different college year students drink

drinksDS_clean %>% 
  select(collegeYear2023, `2023GPA`, major, grade12GPA, studyTime, partyTime, minDrinks) %>% 
  group_by(collegeYear2023, minDrinks) %>% 
  filter(collegeYear2023 != is.na(collegeYear2023)) %>% 
  summarise(count = n()) %>% 
  ggplot() + 
  geom_area(aes(minDrinks, count, fill = collegeYear2023)) + 
  theme_classic() +
  labs(x = "Minimum Amount of Drinks When Going Out", fill = "2023 College Year", title = "How many alcoholic drinks do college students have") 
  
#count of how many times college students go out
drinksDS_clean %>% 
  count(partyTime, sort = TRUE)

#col chart of different majors and how many times they go out during the week 

drinksDS_clean %>%
  group_by(partyTime, major) %>%
  filter(major != is.na(major)) %>% 
  count(partyTime, major, sort = TRUE) %>% 
  ggplot(aes(fct_reorder(partyTime, n, .fun = sum, .desc = TRUE), n, fill = major)) + 
  geom_col() + 
  labs(x = "# of time student go out", 
       y = "Amount of students that go out", 
       fill = "Major/Focus")

#How many drinks do different major have on the weekends
drinksDS_clean %>% 
  filter(partyTime == "Only weekends", 
         major != is.na(major)) %>%  
  count(major, minDrinks) %>% 
  mutate(minDrinks = reorder(minDrinks, n, FUN = sum)) %>% 
  ggplot(aes(minDrinks, n, fill = major)) + 
  geom_col(aes()) + 
  labs(title = "How many drinks do different major have on the weekends", 
       x = "Minimum amount of drinks", 
       y = "count", 
       caption = "Source: ")+
  theme(axis.text.x = element_text(angle = 45, size = 5, vjust = .5))

drinksDS_clean %>% 
  separate(studyTime, c("minStudyTime", "maxStudyTime"), sep = "-", fill = "warn", remove = FALSE, convert = TRUE) %>% 
  mutate(minStudyTime = as.numeric(str_remove(minStudyTime, "\\+")), 
         maxStudyTime = replace_na(maxStudyTime, 8)) %>% 
  filter(partyTime != "Only weekends") %>% 
  count(partyTime, minStudyTime, sort = TRUE) %>% 
  ggplot(aes(partyTime, n))+ 
  geom_freqpoly()

         