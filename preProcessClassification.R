library(tidyr)
library(stringr)
library(Hmisc)

library(caret)
library(rpart.plot)

#Put process into function so it can be called from the executable file
preProcess <- function(filename)
{
  
  #Read in the happiness file, convert to dataframe and remove the unwanted columns 
  happy <- read.csv(filename, header=TRUE)
  happy_df <- data.frame(happy)
  happy_df <- happy_df[-c(4:12)]
  
  
  #Read in the country facts file and convert to dataframe, removing every other row because they are empty
  factbook <- read.csv('factbook-country-profiles.csv', header=TRUE)
  factbook_df <- data.frame(factbook)
  factbook_df <- factbook_df[-c(seq(1, 507, by=2)),]
  
  #Rename the column appropriately
  colnames(factbook_df)[1] <- "Country"
  
  #Merge dataframes and remove the unwanted columns
  combined_df <- merge(happy_df, factbook_df, by="Country")
  combined_df <- combined_df[-c(4,6,7,8,12,14,18,19,21,22,24,26,29,20,31,32,33)]
  combined_df$Happiness.Rank <- NULL
  
  
  #Prepare data in new combined data frame
  
  
  #Split the happiness score into 3 equally distributed groups, save these groups as factors
  combined_df$Happiness.Score <-cut2(combined_df$Happiness.Score, g=3)
  combined_df$Happiness.Score <- as.character(combined_df$Happiness.Score)
  combined_df$Happiness.Score <- substr(combined_df$Happiness.Score, 2, 15)
  combined_df$Happiness.Score <- gsub(",", "-", combined_df$Happiness.Score)
  combined_df$Happiness.Score <- gsub("]", "", combined_df$Happiness.Score)
  combined_df$Happiness.Score <- gsub(")", "", combined_df$Happiness.Score)
  combined_df$Happiness.Score <- as.factor(combined_df$Happiness.Score)
  
  
  
  #1 if country has border countries, 0 if no border countries
  combined_df$border.countries <- strsplit(as.character(combined_df$border.countries), "km")
  combined_df$border.countries[combined_df$border.countries == "N/A"] <- 0
  combined_df$border.countries[combined_df$border.countries != '0'] <- 1
  combined_df$border.countries <- as.numeric(combined_df$border.countries)
  combined_df$border.countries <- as.factor(combined_df$border.countries)
  
  
  
  #Change coastline so that 1 means there is a coast and 0 means that there isn't
  combined_df <- suppressWarnings(separate(combined_df, col="coastline", into=c("coastline", "test"), sep=" "))
  combined_df$test <- NULL
  combined_df$coastline <- as.numeric(gsub(",","",combined_df$coastline))
  combined_df$coastline[combined_df$coastline != 0.0] <- 1.0
  combined_df$coastline <- as.factor(combined_df$coastline)
  
  
  
  #Search through files for keywords to generalise weather as "hot", "cold", or "varied. Allowing 3 distinct categories for classification
  combined_df$climate <- ifelse(grepl("cold", combined_df$climate) & grepl("hot", combined_df$climate), "varied", as.character(combined_df$climate))
  combined_df$climate <- ifelse(grepl("hot|tropical", combined_df$climate), "hot", as.character(combined_df$climate))
  combined_df$climate <- ifelse(grepl("cold", combined_df$climate), "cold", as.character(combined_df$climate))
  combined_df$climate <- ifelse(grepl("cold|hot", combined_df$climate), as.character(combined_df$climate), "varied")
  combined_df$climate <- as.factor(combined_df$climate)
  
  
  
  #Extract selected natural resources into new columns, 1 if natural resource present and 0 if not. Delete original column after
  combined_df$natural.gas <- combined_df$natural.resources
  combined_df$natural.gas <- ifelse(grepl("gas", combined_df$natural.gas), "1", "0")
  combined_df$natural.gas <- as.numeric(combined_df$natural.gas)
  combined_df$natural.gas <- as.factor(combined_df$natural.gas)
  
  combined_df$coal <- combined_df$natural.resources
  combined_df$coal <- ifelse(grepl("coal", combined_df$coal), "1", "0")
  combined_df$coal <- as.numeric(combined_df$coal)
  combined_df$coal <- as.factor(combined_df$coal)
  
  combined_df$petrol <- combined_df$natural.resources
  combined_df$petrol <- ifelse(grepl("petrol", combined_df$petrol), "1", "0")
  combined_df$petrol <- as.numeric(combined_df$petrol)
  combined_df$petrol <- as.factor(combined_df$petrol)
  
  combined_df$oil <- combined_df$natural.resources
  combined_df$oil <- ifelse(grepl("oil", combined_df$oil), "1", "0")
  combined_df$oil <- as.numeric(combined_df$oil)
  combined_df$oil <- as.factor(combined_df$oil)
  
  combined_df$gold <- combined_df$natural.resources
  combined_df$gold <- ifelse(grepl("gold", combined_df$gold), "1", "0")
  combined_df$gold <- as.numeric(combined_df$gold)
  combined_df$gold <- as.factor(combined_df$gold)
  
  combined_df$fish <- combined_df$natural.resources
  combined_df$fish <- ifelse(grepl("fish", combined_df$fish), "1", "0")
  combined_df$fish <- as.numeric(combined_df$fish)
  combined_df$fish <- as.factor(combined_df$fish)
  
  combined_df$iron <- combined_df$natural.resources
  combined_df$iron <- ifelse(grepl("iron", combined_df$iron), "1", "0")
  combined_df$iron <- as.numeric(combined_df$iron)
  combined_df$iron <- as.factor(combined_df$iron)
  
  combined_df$hydropower <- combined_df$natural.resources
  combined_df$hydropower <- ifelse(grepl("hydropower", combined_df$hydropower), "1", "0")
  combined_df$hydropower <- as.numeric(combined_df$hydropower)
  combined_df$hydropower <- as.factor(combined_df$hydropower)
  
  combined_df$diamond <- combined_df$natural.resources
  combined_df$diamond <- ifelse(grepl("diamond", combined_df$diamond), "1", "0")
  combined_df$diamond <- as.numeric(combined_df$diamond)
  combined_df$diamond <- as.factor(combined_df$diamond)
  
  combined_df$natural.resources <- NULL
  
  
  
  #Get the percentage value of the largest religion, use the popularity of the majority religion as a general indicator for how religous the country is. Split into ranges of 25.
  combined_df <- suppressWarnings(separate(combined_df, col="religions", into=c("religions", "test"), sep="%"))
  combined_df$test <- NULL
  combined_df$religions <- substr(combined_df$religions, nchar(combined_df$religions)-5, nchar(combined_df$religions))
  combined_df <- separate(combined_df, col="religions", into=c("test", "religions"), sep=" ")
  combined_df$test <- NULL
  combined_df$religions <- as.numeric(combined_df$religions)
  combined_df$religions <-cut(combined_df$religions, seq(0,100,10))
  combined_df$religions <- as.character(combined_df$religions)
  combined_df$religions <- substr(combined_df$religions, 2, 7)
  combined_df$religions <- gsub(",", "-", combined_df$religions)
  combined_df$religions <- gsub("]", "", combined_df$religions)
  combined_df$religions <- as.factor(combined_df$religions)
  
  
  #Cut median age into ranges of 25 to determine if population is generally older or younger
  combined_df <- separate(combined_df, col="median.age", into=c("median.age", "test"), sep=" ")
  combined_df$test <- NULL
  combined_df$median.age <- as.numeric(combined_df$median.age)
  combined_df$median.age <-cut(combined_df$median.age, seq(10,50,20))
  combined_df$median.age <- as.character(combined_df$median.age)
  combined_df$median.age <- substr(combined_df$median.age, 2, 6)
  combined_df$median.age <- gsub(",", "-", combined_df$median.age)
  combined_df$median.age <- as.factor(combined_df$median.age)
  
  
  
  #Make population growth rate numeric then cut into ranges of 1 to give general growth rate
  combined_df <- separate(combined_df, col="population.growth.rate", into=c("population.growth.rate", "test"), sep="%")
  combined_df$test <- NULL
  combined_df$population.growth.rate <- as.numeric(combined_df$population.growth.rate)
  combined_df$population.growth.rate <- ifelse(combined_df$population.growth.rate>0, 1, 0)
  combined_df$population.growth.rate <- as.factor(combined_df$population.growth.rate)
  
  
  
  #Cut life expectancy into ranges of 20 to determine a generally shorter or generally longer lifespan
  combined_df <- separate(combined_df, col="life.expectancy", into=c("life.expectancy", "test"), sep=" ")
  combined_df$test <- NULL
  combined_df$life.expectancy <- as.numeric(combined_df$life.expectancy)
  combined_df$life.expectancy <-cut(combined_df$life.expectancy, seq(50,90,10))
  combined_df$life.expectancy <- as.character(combined_df$life.expectancy)
  combined_df$life.expectancy <- substr(combined_df$life.expectancy, 2, 8)
  combined_df$life.expectancy <- gsub(",", "-", combined_df$life.expectancy)
  combined_df$life.expectancy <- gsub("]", "", combined_df$life.expectancy)
  combined_df$life.expectancy <- as.factor(combined_df$life.expectancy)
  
  
  
  #Health expenditure in GDP %, cut into ranges of 6 to give general idea of spending on health
  combined_df <- separate(combined_df, col="health.expenditure", into=c("health.expenditure", "test"), sep="%")
  combined_df$test <- NULL
  combined_df$health.expenditure <- as.numeric(combined_df$health.expenditure)
  combined_df$health.expenditure <-cut(combined_df$health.expenditure, seq(0,18,3))
  combined_df$health.expenditure <- as.character(combined_df$health.expenditure)
  combined_df$health.expenditure <- substr(combined_df$health.expenditure, 2, 8)
  combined_df$health.expenditure <- gsub(",", "-", combined_df$health.expenditure)
  combined_df$health.expenditure <- gsub("]", "", combined_df$health.expenditure)
  combined_df$health.expenditure <- as.factor(combined_df$health.expenditure)
  
  
  
  #Obesity rate cut into ranges of 25 to determine if obesity rat is generally high or low
  combined_df <- separate(combined_df, col="obesity.rate", into=c("obesity.rate", "test"), sep="%")
  combined_df$test <- NULL
  combined_df$obesity.rate <- as.numeric(combined_df$obesity.rate)
  combined_df$obesity.rate <-cut(combined_df$obesity.rate, seq(0,60,15))
  combined_df$obesity.rate <- as.character(combined_df$obesity.rate)
  combined_df$obesity.rate <- substr(combined_df$obesity.rate, 2, 8)
  combined_df$obesity.rate <- gsub(",", "-", combined_df$obesity.rate)
  combined_df$obesity.rate <- gsub("]", "", combined_df$obesity.rate)
  combined_df$obesity.rate <- as.factor(combined_df$obesity.rate)
  
  
  #Trim government type comments
  combined_df$government.type <- ifelse(grepl("dictatorship", combined_df$government.type), "dictatorship", as.character(combined_df$government.type))
  combined_df$government.type <- ifelse(grepl("presidential republic", combined_df$government.type), "presidential republic", as.character(combined_df$government.type))
  combined_df$government.type <- ifelse(grepl("parliamentary republic", combined_df$government.type), "parliamentary republic", as.character(combined_df$government.type))
  combined_df$government.type <- ifelse(grepl("federal republic", combined_df$government.type), "federal republic", as.character(combined_df$government.type))
  combined_df$government.type <- ifelse(grepl("democracy", combined_df$government.type), "democracy", as.character(combined_df$government.type))
  combined_df$government.type <- ifelse(grepl("monarchy", combined_df$government.type), "monarchy", as.character(combined_df$government.type))
  combined_df$government.type <- as.factor(combined_df$government.type)
  
  
  #Determine if GDP growth rate is positive or negative and then insert 1 for positive and 0 for negative
  combined_df <- suppressWarnings(separate(combined_df, col="GDP.growth.rate", into=c("GDP.growth.rate", "test"), sep="%"))
  combined_df$test <- NULL
  combined_df$GDP.growth.rate <- as.numeric(combined_df$GDP.growth.rate)
  combined_df$GDP.growth.rate <- ifelse(combined_df$GDP.growth.rate>0, 1, 0)
  combined_df$GDP.growth.rate <- as.factor(combined_df$GDP.growth.rate)
  
  combined_df$Country <- NULL
  
  return(combined_df)
}
#combined_df$Country <- NULL


