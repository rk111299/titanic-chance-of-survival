# Importing the data into R
titan <- read.csv("titanic.csv", header = TRUE)
attach(titan)

# Check for missing values
any(is.na(titan))

# Predicting survival based on sex of passenger
# Fitting our logistic regression model
model1 <- glm(Survived ~ Sex, family = binomial)

# Retrieving probability of surviving as a male
print(log_odds_male <- predict(model1, data.frame(Sex='male')))
print(odds_male <- exp(log_odds_male))
print(probability_male <- odds_male/(1+odds_male))

# 19% chance of survival as a male

# Retrieving probability of surviving as a female
print(log_odds_female <- predict(model1, data.frame(Sex='female')))
print(odds_female <- exp(log_odds_female))
print(probability_female <- odds_female/(1+odds_female))

# 74% chance of survival as a female

# Predicting probability of Jack's survival

# Model 2: All variables in the dataset
model_all <- glm(Survived ~ Sex + factor(Pclass) + Age + Siblings.Spouses.Aboard +
                   Parents.Children.Aboard + Fare + Embarked, family=binomial)

# Model 3: Final model with best predictors
final_model <- glm(Survived ~ Sex + factor(Pclass) + Age + 
                     Siblings.Spouses.Aboard, family=binomial)

# Finding the probabilities of Jack's characteristics
print(log_odds_jack <- predict(final_model, 
                         data.frame(Sex='male', Pclass=3, Age=20, 
                                    Siblings.Spouses.Aboard=0)))
print(odds_jack <- exp(log_odds_jack))
print(probability_jack <- odds_jack/(1+odds_jack))

# Jack has a 13% chance of survival

# Creating a regression table 
library(stargazer)
stargazer(model1, model_all, final_model, type="text")

