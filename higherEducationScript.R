### DDD Final Project Divya Nair
#Data taken from IPUMS 
#SESTAT samples include data from three surveys: the National Survey of College Graduates (NSCG), Survey of Doctorate Recipients (SDR), and the National Survey of Recent College Graduates (NSRCG).

#loading necessary libraries
library (readr)
library(tidyverse)
library (magrittr)
library(gt)
library(tibble)
library (ggplot2)

#loading data
originalData <- read_csv("/Users/divyanair/Desktop/DDD-I21/highered_00001.csv.gz")

#gives number of rows by number of columns

print (dim(originalData)) 
#assigns original data to new dataframe to work with
highEdData <- originalData

#reassigns confusing colnames to more understandable ones
colnames(highEdData) <- c("personID","year","studyWeight","sampleID","surveyID","age","gender","race","citizenStatus","numberOfChildren","highestDegree","jobSatisfaction","salarySatisfaction","socialSatisfaction")

#gets column/variable names
highEdDataCols <- colnames(highEdData) 
print (highEdDataCols) 

#gets row names/ observations
highEdDataRows <- rownames(highEdData) 

#gets class for all columns
colClass <- lapply (highEdData, class)  
print (colClass)

#gets type for all columns
colType <- lapply (highEdData,typeof) 
print (colType)

#gets missing values for all columns. In this data, all skips are logical and are denoted by a 98
missingVals <- lapply (highEdData,is.na) 
print (missingVals) #none should appear, as missing values have 98 in them

### Data Key (taken from IPUMS survey key)
#Gender: 1 = Female, 2 = Male
#Race/Ethnicity: 1 = Asian, 2 = White, 3 = Under-represented minority, 4 = other
#Citizenship: 0 = yes, 1 = no
#Degree: 1 = BA, 2 = MA, 3 = Doctorate, 4 = Professional
#All Satisfactions: 1 = very satisfied, 2 = satisfied, 3 = dissatisfied, 4 = very dissatisfied

### Data Cleaning
#Creating Binary Flags to clean data

#Assigning all 98 data to NA
highEdData[highEdData == 98] <- NA
highEdData$naChildren <- ifelse(is.na(highEdData$numberOfChildren) == TRUE,1,0) #gives a 1 for people who didn't respond/have no children
highEdData$naJobSat <- ifelse(is.na(highEdData$jobSatisfaction) == TRUE,1,0) #gives a 1 for people who didn't respond
highEdData$naCitizenship <- ifelse(is.na(highEdData$citizenStatus) == TRUE,1,0) #gives a 1 for people who didn't respond/ may be undocumented
highEdData$naSalarySat <- ifelse(is.na(highEdData$salarySatisfaction) == TRUE,1,0) #gives a 1 for people who didn't respond
highEdData$naSocialSat <- ifelse(is.na(highEdData$socialSatisfaction) == TRUE,1,0) #gives a 1 for people who didn't respond

#Cleaning data by removing all observations with no response for one or more questions
cleanedData <- highEdData[!(highEdData$naChildren == 1 | 
                              highEdData$naJobSat == 1 | 
                              highEdData$naCitizenship == 1 | 
                              highEdData$naSalarySat == 1 | 
                              highEdData$naSocialSat == 1),]

#### Examining relationships between variables

### Gender and Highest Degree 

#Subsetting Data to create a dataframe with only gender and highest degree obtained data
genderAndDegree <- highEdData %>% 
  dplyr::select(gender , highestDegree) %>% 
  dplyr::arrange(gender)

#Creating a new dataframe with only female data
femaleDegree <- genderAndDegree[genderAndDegree$gender == 1, ]
unique (femaleDegree$gender) #should be 1, doublechecking
avgFemDegree <- mean(femaleDegree$highestDegree) #average degree is 1.942707. Rounded to 2, females have a Masters Degree as highest degree

#Creating a new dataframe with only male data
maleDegree <- genderAndDegree[genderAndDegree$gender == 2, ] 
avgMaleDegree <- mean(maleDegree$highestDegree) #average degree is 2.007282. Higher than females. Rounded to 2, males have a Masters Degree as highest degree
unique (maleDegree$gender) #should be 2, doublechecking

#creates a dataframe with numeric degree breakdown for females
femDegreeBreakdown <- femaleDegree %>% 
  dplyr::group_by(gender,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = (breakdown / sum(breakdown)*100))

#creates a dataframe with numeric degree breakdown for males
maleDegreeBreakdown <- maleDegree %>% 
  dplyr::group_by(gender,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = (breakdown / sum(breakdown)*100))

#Creating table to summarize gender and highest degree results

#Preparing Data for table
totalGenderDeg <- cbind (femDegreeBreakdown, maleDegreeBreakdown)
totalGenderDeg <- totalGenderDeg[,c(2:4,7,8)]
totalGenderDeg[,1] <- c("Bachelors","Masters","Doctorate","Professional")
totalGenderDeg <- as_tibble(totalGenderDeg)
colnames(totalGenderDeg) <- c("Degree Name", "Female Number of Degree","Female % Degree","Male Number of Degree","Male % Degree")

#Creating table
genderAndDegTable <- totalGenderDeg %>% 
  gt() %>%
  tab_header(title = "Figure 1.1: Degree Breakdown by Gender",subtitle = "Source: IPUMS 2013 Higher Education Surveys") %>%
  tab_spanner(
    label = "Female",
    columns = vars("Female Number of Degree","Female % Degree")
  ) %>%
  tab_spanner(
    label = "Male",
    columns = vars("Male Number of Degree","Male % Degree")
  ) %>%
  cols_label(
    `Female Number of Degree` = "# Degree",
    `Female % Degree` = "% Degree",
    `Male Number of Degree` = "# Degree",
    `Male % Degree` = "% Degree"
  )

# Findings: Males and females have similar % of Professional degrees, 
# males have a significantly higher % of Doctorate Degrees, 
# females have significantly higher % Masters, BA % relatively similar

###Race and Highest Degree

#Subsetting Data to only include race and highest degree data and creating new dataframe with relevant variables
raceAndDegree <- highEdData %>% 
  dplyr::select(race , highestDegree) %>% 
  dplyr::arrange(race)

#Subsetting Asian respondents and calculating average highest degree obtained
asianDegree <- raceAndDegree[raceAndDegree$race == 1, ]
avgAsianDegree <- mean(asianDegree$highestDegree) #average Asian degree: 2.069207

#Subsetting White respondents and calculating average highest degree obtained
whiteDegree <- raceAndDegree[raceAndDegree$race == 2, ]
avgWhiteDegree <- mean(whiteDegree$highestDegree) #average Asian degree: 1.991291

#Subsetting respondents of an underrepresented minority and calculating average highest degree obtained
underRepMinDegree <- raceAndDegree[raceAndDegree$race == 3, ]
avgUnderRepMinDegree <- mean(underRepMinDegree$highestDegree) #average underepresented minority degree: 1.86711

#Creating numeric breakdown by race of how many individuals/ percent of individuals obtained what degree
asianBreakdown <- asianDegree %>% 
  dplyr::group_by(race,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = (breakdown / sum(breakdown)*100))

whiteBreakdown <- whiteDegree %>% 
  dplyr::group_by(race,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = (breakdown / sum(breakdown)*100))

underRepMinBreakdown <- underRepMinDegree %>% 
  dplyr::group_by(race,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = (breakdown / sum(breakdown)*100))

#Creating graph for race and degree data
#Preparing Data for graphing
asianBreakdown[,1] <- "Asian"
asianBreakdown[,2] <- c("Bachelor's", "Masters","Doctorate","Professional")
asianBreakdown <- asianBreakdown[,c(1,2,4)]

whiteBreakdown[,1] <- "White"
whiteBreakdown[,2] <- c("Bachelor's", "Masters","Doctorate","Professional")
whiteBreakdown <- whiteBreakdown[,c(1,2,4)]

underRepMinBreakdown[,1] <- "Underrepresented Minority"
underRepMinBreakdown[,2] <- c("Bachelor's", "Masters","Doctorate","Professional")
underRepMinBreakdown <- underRepMinBreakdown[,c(1,2,4)]

fullRaceDeg <- rbind(asianBreakdown, whiteBreakdown, underRepMinBreakdown)
fullRaceDegGrouped <- fullRaceDeg %>%
  group_by(highestDegree)

#Creating graph
RaceDegPlot <- ggplot(data = fullRaceDegGrouped,
                      aes(x = highestDegree, y = percentDegree, fill = race)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Figure 1.2: Breakdown of Degree by Race", subtitle = "Source: IPUMS 2013 Higher Education Surveys") +
  ylab("% of Respondents With Degree") +
  xlab("Highest Degree Obtained") +
  scale_fill_manual(values = c('thistle','lightskyblue1','darkseagreen2'))

RaceDegPlot

###Degree and Job/Salary/Social Satisfaction

#Subsetting data for only satisfaction and highest degree variables
degAndSat <- highEdData %>%
  dplyr::select(highestDegree, jobSatisfaction, salarySatisfaction, socialSatisfaction, naJobSat, naSalarySat, naSocialSat) %>%
  dplyr::arrange(highestDegree)

#Removing observations that did not respond for at least one satisfaction question
degAndSatCleaned <- degAndSat[!(degAndSat$naJobSat == 1 | 
                              degAndSat$naSalarySat == 1 | 
                              degAndSat$naSocialSat == 1),] 

#Calculating average satisfaction scores by observation
degAndSatCleaned$avgSat <- (
  (degAndSatCleaned$jobSatisfaction + 
     degAndSatCleaned$salarySatisfaction + 
     degAndSatCleaned$socialSatisfaction)/3
  )

#Creating a dataframe for BAs and calculating average satisfaction score by degree
BASat <- degAndSatCleaned[degAndSatCleaned$highestDegree == 1, ]
BAAvgSat <- mean(BASat$avgSat) #average satisfaction score is 1.84, or rounded to satisfied

#Creating a dataframe for MAs and calculating average satisfaction score by degree
MaSat <- degAndSatCleaned[degAndSatCleaned$highestDegree == 2, ]
MaAvgSat <- mean(MaSat$avgSat) #average satisfaction score is 1.77, or rounded to satisfied

#Creating a dataframe for Doctorates and calculating average satisfaction score by degree
DocSat <- degAndSatCleaned[degAndSatCleaned$highestDegree == 3, ]
DocAvgSat <- mean(DocSat$avgSat) #average satisfaction score is 1.72, or rounded to satisfied

#Creating a dataframe for those with Professional Degrees and calculating average satisfaction score by degree
ProfSat <- degAndSatCleaned[degAndSatCleaned$highestDegree == 4, ]
ProfAvgSat <- mean(ProfSat$avgSat) #average satisfaction score is 1.63, or rounded to satisfied

#Findings from the averages: people with BAs tend to be overall most satisfied

#Creating degree and satisfaction table/ breadown of each satisfaction by degree

#Creating a dataframe for bachelor's degree recipients. 
#Has breakdown of how many people responded "Very Satisfied, Satisfied, Dissatisfied, and Very Dissatisfied" for all three satisfaction questions.
BASum1 <- BASat %>%
  dplyr::group_by(jobSatisfaction) %>%
  dplyr::summarise(summJob = n()) %>%
  dplyr::mutate(jobPercentDegree = (summJob / sum(summJob)*100))
BaSum2 <- BASat %>%
  dplyr::group_by(salarySatisfaction) %>%
  dplyr::summarise(summSat = n()) %>%
  dplyr::mutate(salPercentDegree = (summSat / sum(summSat)*100))
BaSum3 <- BASat %>%
  dplyr::group_by(socialSatisfaction) %>%
  dplyr::summarise(summSoc = n()) %>%
  dplyr::mutate(socPercentDegree = (summSoc / sum(summSoc)*100))
BaSumTot <- cbind(BASum1, BaSum2, BaSum3) %>%
  dplyr::select(jobSatisfaction, summJob, jobPercentDegree, summSat, salPercentDegree, summSoc, socPercentDegree)
colnames(BaSumTot) <- c("Satisfaction", "BAjob#", "BAjob%","BAsat#","BAsat%","BAsoc#","BAsoc%")  

#Creating a dataframe for master's degree recipients. 
#Has breakdown of how many people responded "Very Satisfied, Satisfied, Dissatisfied, and Very Dissatisfied" for all three satisfaction questions.
MaSum1 <- MaSat %>%
  dplyr::group_by(jobSatisfaction) %>%
  dplyr::summarise(summJob = n()) %>%
  dplyr::mutate(jobPercentDegree = (summJob / sum(summJob)*100))
MaSum2 <- MaSat %>%
  dplyr::group_by(salarySatisfaction) %>%
  dplyr::summarise(summSat = n()) %>%
  dplyr::mutate(salPercentDegree = (summSat / sum(summSat)*100))
MaSum3 <- MaSat %>%
  dplyr::group_by(socialSatisfaction) %>%
  dplyr::summarise(summSoc = n()) %>%
  dplyr::mutate(socPercentDegree = (summSoc / sum(summSoc)*100))
MaSumTot <- cbind(MaSum1, MaSum2, MaSum3) %>%
  dplyr::select(summJob, jobPercentDegree, summSat, salPercentDegree, summSoc, socPercentDegree)
colnames(MaSumTot) <- c("MAjob#", "MAjob%","MAsat#","MAsat%","MAsoc#","MAsoc%") 

#Creating a dataframe for doctorate degree recipients. 
#Has breakdown of how many people responded "Very Satisfied, Satisfied, Dissatisfied, and Very Dissatisfied" for all three satisfaction questions.
DocSum1 <- DocSat %>%
  dplyr::group_by(jobSatisfaction) %>%
  dplyr::summarise(summJob = n()) %>%
  dplyr::mutate(jobPercentDegree = (summJob / sum(summJob)*100))
DocSum2 <- DocSat %>%
  dplyr::group_by(salarySatisfaction) %>%
  dplyr::summarise(summSat = n()) %>%
  dplyr::mutate(salPercentDegree = (summSat / sum(summSat)*100))
DocSum3 <- DocSat %>%
  dplyr::group_by(socialSatisfaction) %>%
  dplyr::summarise(summSoc = n()) %>%
  dplyr::mutate(socPercentDegree = (summSoc / sum(summSoc)*100))
DocSumTot <- cbind(DocSum1, DocSum2, DocSum3) %>%
  dplyr::select(summJob, jobPercentDegree, summSat, salPercentDegree, summSoc, socPercentDegree)
colnames(DocSumTot) <- c("Docjob#", "Docjob%","Docsat#","Docsat%","Docsoc#","Docsoc%")

#Creating a dataframe for professional degree recipients. 
#Has breakdown of how many people responded "Very Satisfied, Satisfied, Dissatisfied, and Very Dissatisfied" for all three satisfaction questions.
ProfSum1 <- ProfSat %>%
  dplyr::group_by(jobSatisfaction) %>%
  dplyr::summarise(summJob = n()) %>%
  dplyr::mutate(jobPercentDegree = (summJob / sum(summJob)*100))
ProfSum2 <- ProfSat %>%
  dplyr::group_by(salarySatisfaction) %>%
  dplyr::summarise(summSat = n()) %>%
  dplyr::mutate(salPercentDegree = (summSat / sum(summSat)*100))
ProfSum3 <- ProfSat %>%
  dplyr::group_by(socialSatisfaction) %>%
  dplyr::summarise(summSoc = n()) %>%
  dplyr::mutate(socPercentDegree = (summSoc / sum(summSoc)*100))
ProfSumTot <- cbind(ProfSum1, ProfSum2, ProfSum3) %>%
  dplyr::select(summJob, jobPercentDegree, summSat, salPercentDegree, summSoc, socPercentDegree)
colnames(ProfSumTot) <- c("Profjob#", "Profjob%","Profsat#","Profsat%","Profsoc#","Profsoc%")


#Creating table
BASatTable <- BaSumTot %>%
  gt() %>%
  tab_header(title = "Figure 1.3: Bachelor's Satisfaction Breakdown", subtitle = "Source: IPUMS 2013 Higher Education Surveys") %>%
  tab_spanner (label = "Bachelors", columns = vars("BAjob#", "BAjob%","BAsat#","BAsat%","BAsoc#","BAsoc%")) %>%
  cols_label(
    `BAjob#` = "# of Respondents Satisfied with Job",
    `BAjob%` = "% of Respondents Satisfied with Job",
    `BAsat#` = "# of Respondents Satisfied with Salary",
    `BAsat%` = "% of Respondents Satisfied with Salary",
    `BAsoc#` = "# of Respondents Satisfied with Social Contribution",
    `BAsoc%` = "% of Respondents Satisfied with Social Contribution"
    )
BASatTable

MASatTable <- MaSumTot %>%
  gt() %>%
  tab_header(title = "Figure 1.4: Master's Satisfaction Breakdown", subtitle = "Source: IPUMS 2013 Higher Education Surveys") %>%
  tab_spanner (label = "Master's", columns = vars("MAjob#", "MAjob%","MAsat#","MAsat%","MAsoc#","MAsoc%")) %>%
  cols_label(
    `MAjob#` = "# of Respondents Satisfied with Job",
    `MAjob%` = "% of Respondents Satisfied with Job",
    `MAsat#` = "# of Respondents Satisfied with Salary",
    `MAsat%` = "% of Respondents Satisfied with Salary",
    `MAsoc#` = "# of Respondents Satisfied with Social Contribution",
    `MAsoc%` = "% of Respondents Satisfied with Social Contribution"
  )
MASatTable

DocSatTable <- DocSumTot %>%
  gt() %>%
  tab_header(title = "Figure 1.5: Doctorate Satisfaction Breakdown", subtitle = "Source: IPUMS 2013 Higher Education Surveys") %>%
  tab_spanner (label = "Doctorate", columns = vars("Docjob#", "Docjob%","Docsat#","Docsat%","Docsoc#","Docsoc%")) %>%
  cols_label(
    `Docjob#` = "# of Respondents Satisfied with Job",
    `Docjob%` = "% of Respondents Satisfied with Job",
    `Docsat#` = "# of Respondents Satisfied with Salary",
    `Docsat%` = "% of Respondents Satisfied with Salary",
    `Docsoc#` = "# of Respondents Satisfied with Social Contribution",
    `Docsoc%` = "% of Respondents Satisfied with Social Contribution"
  )
DocSatTable

ProfSatTable <- ProfSumTot %>%
  gt() %>%
  tab_header(title = "Figure 1.6: Professional Degree Satisfaction Breakdown", subtitle = "Source: IPUMS 2013 Higher Education Surveys") %>%
  tab_spanner (label = "Professional Degree", columns = vars("Profjob#", "Profjob%","Profsat#","Profsat%","Profsoc#","Profsoc%")) %>%
  cols_label(
    `Profjob#` = "# of Respondents Satisfied with Job",
    `Profjob%` = "% of Respondents Satisfied with Job",
    `Profsat#` = "# of Respondents Satisfied with Salary",
    `Profsat%` = "% of Respondents Satisfied with Salary",
    `Profsoc#` = "# of Respondents Satisfied with Social Contribution",
    `Profsoc%` = "% of Respondents Satisfied with Social Contribution"
  )
ProfSatTable


