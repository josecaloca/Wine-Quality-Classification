#Title of Database: Wine recognition data
#website for the data "https://archive.ics.uci.edu/ml/datasets/wine"

#These data are the results of a chemical analysis of
#wines grown in the same region in Italy but derived from three
#different cultivars.
#The analysis determined the quantities of 13 constituents
#found in each of the three types of wines. 


#reading in the data. clean the data set to make it ready for analysis

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

if(!require(ggstatsplot)) remotes::install_github(
  repo = "IndrajeetPatil/ggstatsplot", # package path on GitHub
  dependencies = TRUE, # installs packages which ggstatsplot depends on
  upgrade_dependencies = TRUE # updates any out of date dependencies
)


dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",dl)
wine_data <- fread(dl)

#adding column names. These were gathered from the wine.names file at 
#"https://archive.ics.uci.edu/ml/machine-learning-databases/wine/"
column_names <- c('Class',
                  'Alcohol',
                  'Malic_Acid',
                  'Ash',
                  'Alcalinity_of_Ash',
                  'Magnesium',
                  'Total_Phenols',
                  'Flavanoids',
                  'Nonflavanoids_Phenols',
                  'Proanthocyanins',
                  'Color_Intensity',
                  'Hue',
                  'Proteine_Concentration',
                  'Proline')


colnames(wine_data) <- column_names
wine_data$Class <- as.factor(wine_data$Class)
head(wine_data)

#the goal of this project will be to predict the Class(1,2, or 3) of the wine from some or all of the 13 attributes


# we'll make density plots and overlap them by class.
#couldn't figure out how to to it at once, so I'll do it by hand

#alcohol and Malic_acid

data_long <- gather(wine_data, condition, measurement, 2:3, factor_key=TRUE)

ggstatsplot::grouped_ggbetweenstats(
  data = data_long,
  x = Class,
  y = measurement,
  grouping.var = condition, # grouping variable
  outlier.tagging = TRUE, # whether outliers need to be tagged
  outlier.coef = 2,
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni",
  title.prefix = "Wine attribute",
  palette = "default_jama",
  package = "ggsci",
)

#ash and #alcalinity of ash

data_long <- gather(wine_data, condition, measurement, 4:5, factor_key=TRUE)

ggstatsplot::grouped_ggbetweenstats(
  data = data_long,
  x = Class,
  y = measurement,
  grouping.var = condition, # grouping variable
  outlier.tagging = TRUE, # whether outliers need to be tagged
  outlier.coef = 2,
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni",
  title.prefix = "Wine attribute",
  palette = "default_jama",
  package = "ggsci",
)

#magnesium and Total Phenols

data_long <- gather(wine_data, condition, measurement, 6:7, factor_key=TRUE)

ggstatsplot::grouped_ggbetweenstats(
  data = data_long,
  x = Class,
  y = measurement,
  grouping.var = condition, # grouping variable
  outlier.tagging = TRUE, # whether outliers need to be tagged
  outlier.coef = 2,
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni",
  title.prefix = "Wine attribute",
  palette = "default_jama",
  package = "ggsci",
)

#Flavanoids and non-flavanoids phenols

data_long <- gather(wine_data, condition, measurement, 8:9, factor_key=TRUE)

ggstatsplot::grouped_ggbetweenstats(
  data = data_long,
  x = Class,
  y = measurement,
  grouping.var = condition, # grouping variable
  outlier.tagging = TRUE, # whether outliers need to be tagged
  outlier.coef = 2,
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni",
  title.prefix = "Wine attribute",
  palette = "default_jama",
  package = "ggsci",
)

 #Proanthocyanins and Color Intensity

data_long <- gather(wine_data, condition, measurement, 10:11, factor_key=TRUE)

ggstatsplot::grouped_ggbetweenstats(
  data = data_long,
  x = Class,
  y = measurement,
  grouping.var = condition, # grouping variable
  outlier.tagging = TRUE, # whether outliers need to be tagged
  outlier.coef = 2,
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni",
  title.prefix = "Wine attribute",
  palette = "default_jama",
  package = "ggsci",
)

#Hue and OD280/OD315_of_diluted wines

data_long <- gather(wine_data, condition, measurement, 12:13, factor_key=TRUE)

ggstatsplot::grouped_ggbetweenstats(
  data = data_long,
  x = Class,
  y = measurement,
  grouping.var = condition, # grouping variable
  outlier.tagging = TRUE, # whether outliers need to be tagged
  outlier.coef = 2,
  ggsignif.args = list(textsize = 4, tip_length = 0.01),
  p.adjust.method = "bonferroni",
  title.prefix = "Wine attribute",
  palette = "default_jama",
  package = "ggsci",
)

#proline

ggstatsplot::ggbetweenstats(
  data = wine_data,
  x = Class,
  y = Proline,
  title = "Distribution of sepal length across Iris species"
)


#
#
#splitting data into train and test sets.
#training will be 75% of data
colnames(wine_data) <- make.names(colnames(wine_data)) #the names for the columns I specified were bad
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(wine_data$Class, times = 1, p = 0.25, list = FALSE)

train_set <- wine_data[-test_index]
test_set <- wine_data[test_index,]

#i'll make a graph of alcohol vs ash and see what I get. Just for fun

train_set %>%
  ggplot(aes(x = Alcohol, y = Ash, col = Class)) + 
  geom_point(size = 4)

#I will start by making a naive model using ifelse and the level of alcohol
alcohol_model <- function(val){
  if(val <= 12.75){
    2
  } else if(val <= 13.75){
    3
  } else 1
}

y_hat_alc <- sapply(test_set$Alcohol, alcohol_model)
y_hat_alc <- as.factor(y_hat_alc)
mean(y_hat_alc == test_set$Class) #this gives an accuracy of 77%

#I want to make a model that accounts for more than just alcohol
#The Total_Phenols column shows similar distributions like alcohol. So I will plot them together

train_set %>%
  ggplot(aes(x = Alcohol, y = Total_Phenols, col = Class)) + 
  geom_point(size = 4)
#Now I'll make another naive model using alcohol and total_phenols

alc_phen <- function(alc, phen){
  if (alc <= 12.75){
    2
  } else if(phen >= 2.5){
    1
  } else 3
}

y_hat_alc_phen <- mapply(alc_phen, test_set$Alcohol, test_set$Total_Phenols)
y_hat_alc_phen <- as.factor(y_hat_alc_phen)
mean(y_hat_alc_phen == test_set$Class) #this gave an accuracy of 80%

#I want to continue in this fashion and make one more naive model.
#Flavanoids also has similar distributions like alcohol. So I will plot flavanoids and alc

train_set %>%
  ggplot(aes(x = Alcohol, y = Flavanoids, col = Class)) + 
  geom_point(size = 4)


alc_flav <- function(alc, flav){
  if(flav > 2.5){
    1
  } else if(alc <= 12.5){
    2
  } else
    3
}

y_hat_alc_flav <- mapply(alc_flav, test_set$Alcohol, test_set$Flavanoids)
y_hat_alc_flav <- as.factor(y_hat_alc_flav)
mean(y_hat_alc_flav == test_set$Class) #this also gave an accuracy of 77%

#to allow more flexibility in the rules, I need to use machine learning.

#I will start with a decision tree model. We only use CV for the complexity parameter.
set.seed(3, sample.kind = "Rounding")
train_rpart <- train(Class ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0,0.1,0.01)),
                     data = train_set)

ggplot(train_rpart, highlight = TRUE) #this shows the optimal cp

y_hat_rpart <- predict(train_rpart, test_set) #we make a prediction

mean(y_hat_rpart == test_set$Class)

#we get an accuracy of 93% !
#lets look at sensitivity and specificity through the confusion matrix

confusionMatrix(y_hat_rpart, test_set$Class)
#
#
#now I will use a random forest model
#first I use modelLookup to see what parameter is needed
modelLookup("rf")  #looks like I need mtry

set.seed(6, sample.kind = "Rounding")
train_rf <- train(Class ~ .,
                  data = train_set,
                  model = "rf",
                  tuneGrid = data.frame(mtry = seq(0,10,0.5)))

ggplot(train_rf, highlight = TRUE)

y_hat_rf <- predict(train_rf, test_set) 
head(y_hat_rf)
mean(y_hat_rf == test_set$Class) #this gave an accuracy of 100%!
confusionMatrix(y_hat_rf, test_set$Class)
#
#
#I want to try knn now
#first I use modelLookup
modelLookup("knn") #the parameter is k
set.seed(7, sample.kind = "Rounding")

train_knn <- train(Class ~ ., 
                   data = train_set,
                   model = "knn",
                   tuneGrid = data.frame(mtry = seq(1,10)))

ggplot(train_knn, highlight = TRUE)
y_hat_knn <- predict(train_knn, test_set)
head(y_hat_knn)

mean(y_hat_rf == test_set$Class) #this also gave an accuracy of 100%

confusionMatrix(y_hat_rf, test_set$Class)
