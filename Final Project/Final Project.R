library(tidyverse)
library(psych)
library(knitr)
library(kableExtra)
library(DT)
library(stats)
library(fmsb)
library(nlme)
library(VIF)
library(plotly)
library(base)

#Import DataSet
IMDBFilmRaw <- read_csv("C:/Alice Wei/WFU/MSBA/R/Projects/RProjects/Final Project/movie_metadata.csv")

#Retrieve Data Variables
Variables <- names(IMDBFilmRaw)
#View(tibble(x =1:28,y = Variables))
VariableList <- tibble(No=1:28,Var = sort(Variables))


#Data Cleaning

#Select and reorder columns
CleanFilmData <- IMDBFilmRaw[,c(12,26,4,1,24,10,20,21,9,23,5,6,8,25,28)]

#Remove NA, Sepreate Column and get first element, add new variable)
FinalFilmData <- CleanFilmData %>% 
  na.omit() %>% 
  separate(genres, into = c("Genre_1")) %>%
  mutate(ProfitTurnover = (gross - budget) / budget, 
         AdjustedGross = gross / 1000000,
         AdjustedBudget = budget / 1000000,
         Language = ifelse(language == "English","English","Non-English"), 
         Country = ifelse(country == "USA", "USA", "Non-US"),
         TotalFacebooklikes = actor_3_facebook_likes + actor_2_facebook_likes +
           actor_1_facebook_likes + director_facebook_likes + 
           movie_facebook_likes,
         TitleLength = nchar(movie_title),
         Generation = ifelse(title_year <= 1926, "GI Generation", 
                             ifelse(title_year <= 1945, 'Mature/ilents',
                                    ifelse(title_year <= 1964, "Baby Boomers",
                                           ifelse(title_year <= 1980, "Generation X",
                                                  ifelse(title_year <= 2000, "Millenium","Boomletes"))))))


write_rds(FinalFilmData, "C:/Alice Wei/WFU/MSBA/R/Projects/RProjects/Final Project/FinalFilmData.rds")
write_csv(FinalFilmData, "C:/Alice Wei/WFU/MSBA/R/Projects/RProjects/Final Project/FinalFilmData.csv")
# remove summed columns
FinalFilmData <- FinalFilmData[,-c(7,8,11:15)]

RegressionData <- FinalFilmData %>% filter(Country == "USA") 

RegressionData <- RegressionData[,-c(4:8,12,13,16)]

#Retrieve New Var Names
FinalVar <- names(FinalFilmData)
FinalVarList <- tibble(VarNum = 1:15, VarName = FinalVar)
VarType <- lapply(FinalFilmData, typeof)
FinalVarList <- tibble(Num = 1:15, VarName = FinalVar, Type = VarType)
# Data Analysis

# MultipleRegression
#MultipleRegression <- lm(log(imdb_score) ~ duration + title_year + gross + 
                           budget + TotalFacebooklikes,
                         data = FinalFilmData)

#Nonlinear <- nls(imdb_score ~ duration,
                 data = FinalFilmData)
#gls <- gls(log(imdb_score) ~ duration + title_year + gross + 
             budget + TotalFacebooklikes + ProfitTurnover,
           data = FinalFilmData)

FullRegressionglm <- glm(imdb_score  ~ duration + AdjustedGross + 
             AdjustedBudget + ProfitTurnover + TotalFacebooklikes + TitleLength,
           data = RegressionData, family = gaussian)

#glmsqr <- glm(imdb_score  ~ duration + sqrt(gross) + 
             sqrt(budget) + TotalFacebooklikes,
           data = FinalFilmData)

summary(glm)
summary(glmsqr)
RegressionSummary <- summary(MultipleRegression)
RegressionSummary



#Final Regression Model

FullRegression <- glm(imdb_score*imdb_score  ~ duration + AdjustedGross + 
                           AdjustedBudget + ProfitTurnover + TotalFacebooklikes + TitleLength,
                         data = RegressionData, family = gaussian)

FullRegressionlm <- lm(imdb_score*imdb_score  ~ duration + AdjustedGross + 
                        AdjustedBudget + ProfitTurnover + TitleLength,
                      data = RegressionData)
summary(FullRegression)

plot(FullRegression)

# Correlation 
cor(RegressionData[,unlist(lapply(RegressionData, is.numeric))])

 #residual
residual <- residuals(MultipleRegression)
influence(MultipleRegression)
plot(FinalFilmData$gross, residual)
residualgls <- residuals(gls)
residualgls
residualglm <- residuals(glm)
residualglmsqr <-residuals(glmsqr)
plot(FinalFilmData$gross, residualglm)

qqnorm(residualglm) + qqline(residualglm)
+qqnorm(residualglmsqr) + qqline(residualglmsqr)
ggplot(FinalFilmData, aes(gross, residualglm)) + geom_point() + scale_x_contin
uous(trans = "log1p")
ggplot(FinalFilmData, aes(title_year, residualglm)) + geom_point() #+ scale_x_continuous(trans = "log1p")
#vif

VIF(glm)

# test regression between gross and budget

summary(lm(gross ~ budget, data = FinalFilmData))



# TurnOver Regression
summary(lm(ProfitTurnover ~ budget, data = FinalFilmData))


# Data Summary

summary(FinalFilmData)



# Plots
sqrplot <- function(x){sqrt(x)}
plot(sqrplot(1:1000000), type = "l")





# Distribution

FinalFilmData %>% ggplot(aes(gross)) +geom_histogram(binwidth = 100000)
FinalFilmData %>% ggplot(aes(gross)) +geom_histogram(binwidth = 100000)
FinalFilmData %>% ggplot(aes(imdb_score))+geom_bar(fill = '#FFFF90')   



FinalFilmData %>% ggplot(aes(budget))+geom_histogram()+xlim()

FinalFilmData %>% ggplot(aes(ProfitTurnover), color = title_year) + geom_histogram()+xlim(0,5)

ScoreSummary <- FinalFilmData %>% summarise(Mean=mean(imdb_score), Median=median(imdb_score), Sd = sd(imdb_score))
FinalFilmData %>% ggplot(aes(y = imdb_score, x = duration, color = title_year) + geom_point()
FinalFilmData %>% ggplot(aes(y = imdb_score, x = gross), fill = title_year) + geom_point()+geom_smooth() + xlim(100000,1000000000)
FinalFilmData %>% ggplot(aes(y = imdb_score, x = ProfitTurnover, color = Generation)) + geom_point()+geom_smooth(method = lm)+xlim(0,10)
FinalFilmData %>% ggplot(aes(y = gross, x = budget)) + geom_point()+geom_smooth()+xlim(100000,400000000)
FinalFilmData %>% ggplot(aes(y = imdb_score, x = title_year)) + geom_point()+geom_smooth(method = lm) 
FinalFilmData %>% ggplot(aes(x=budget,y=gross))+geom_point()+xlim(100000,100000000)
FinalFilmData %>% ggplot(aes(Genre_1,imdb_score), color = Genre_1) +geom_boxplot() 

FinalFilmData %>% ggplot(aes(y=imdb_score, x = Genre_1, categoty = color, fill = color))+geom_boxplot()

cor(FinalFilmData[,unlist(lapply(FinalFilmData, is.numeric))])
x <- lm(imdb_score ~ movie_facebook_likes + gross + budget + duration + title_year + movie_facebook_likes, data=FinalFilmData)
y <- lm(imdb_score ~ gross, data = FinalFilmData, na.action=na.exclude)
skew(FinalFilmData$gross,FALSE)
summary(x)
y

summary(y)
summary(FinalFilmData$gross)
?summary







z <- lm(FinalFilmData$gross ~ FinalFilmData$budget)
z
summary(z)

i <- lm(FinalFilmData$imdb_score ~ FinalFilmData$ProfitTurnover)
i
summary(i)


