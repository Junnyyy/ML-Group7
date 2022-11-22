install.packages('readr')
install.packages('tree')
install.packages('randomForest')
#install.packages('gbm')
library(readr)
library(tree)
library(randomForest)
#library(gbm)

# Load in health Data
health <- read_csv("healthcare-dataset-stroke-data.csv", col_types = cols(gender = col_factor(levels = c("Male", "Female", "Other")), hypertension = col_factor(levels = c("0", "1")), heart_disease = col_factor(levels = c("0", "1")), ever_married = col_factor(levels = c("No", "Yes")), work_type = col_factor(levels = c("children", "Govt_job", "Never_worked", "Private", "Self-employed")), Residence_type = col_factor(levels = c("Rural", "Urban")), bmi = col_number(), smoking_status = col_factor(levels = c("formerly smoked", "never smoked", "smokes")), stroke = col_factor(levels = c("0", "1"))))
# Remove ID column
health <- health[,-1]
# Omit NA values
health <- na.omit(health)
View(health)


# Create training data
set.seed(500)
train <- sample(1:nrow(health), nrow(health)/2 + 0.5)
health.train <- health[train,]

# Create testing data
health.test <- health[-train,]

# Create Decision Tree
health.tree <- tree(stroke ~ ., data = health.train)

# Plot Decision Tree
plot(health.tree)
text(health.tree, pretty = 0)
summary(health.tree)

# Predict using testing Data
health.pred <- predict(health.tree, health.test, type = "class")
table(health.pred, health.test$stroke)


# Prune Decision Tree
cv.health = cv.tree(health.tree, FUN = prune.misclass)
plot(cv.health$size, cv.health$dev, type = "b")

# Predict using Pruned Tree
health.pruned <- prune.misclass(health.tree, best = 5)
health.pred.pruned <- predict(health.pruned, health.test, type = "class")
table(health.pred.pruned, health.test$stroke)


# Bagged Model
p = ncol(health)
n = nrow(health)
B = 1000
health.bag <- randomForest(stroke ~ ., data = health.train, mtry = p, ntree = B, importance = TRUE)
health.bag

# Predict using Bagged Model
health.pred.bag <- predict(health.bag, health.test, type = "class")
table(health.pred.bag, health.test$stroke)

# Importance of Variables in Bagged Model
varImpPlot(health.bag)

# Create a new decision tree based on the bagged model
health.tree.bag <- tree(stroke ~ ., data = health.train, subset = health.bag$inbag)
plot(health.tree.bag)
text(health.tree.bag, pretty = 0)


# Random Forest Variables
STROKE.test <- as.factor(health.train$stroke)
X.test <- health[-train, -11]
p = ncol(health)
n = nrow(health)
B = 1000

# Random Forest Model
health.rf <- randomForest(stroke ~ ., data = health, subset = train, xtest = X.test, ytest = STROKE.test, mtry = sqrt(p), ntree = B, importance = TRUE, keep.forest = TRUE)
health.rf

# Predict using Random Forest Model
health.pred.rf <- predict(health.rf, health.test, type = "class")
table(health.pred.rf, health.test$stroke)

# Importance of Variables in Random Forest Model
varImpPlot(health.rf)

# Create new decision tree based on random forest model
health.tree.rf <- tree(stroke ~ ., data = health.train, subset = health.rf$inbag)plot(health.tree.rf)
text(health.tree.rf, pretty = 0)

# Boosted Model
# health.boost <- gbm(stroke ~ ., data = health.train, distribution = "bernoulli", n.trees = B, interaction.depth = 4, shrinkage = 0.01)
# health.boost
# summary(health.boost)