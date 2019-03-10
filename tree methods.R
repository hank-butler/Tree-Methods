library(ISLR)

head(College)

df <- College

library(ggplot2)

ggplot(df, aes(x = Room.Board, y = Grad.Rate))+
  geom_point(aes(color = Private))

ggplot(df, aes(x = F.Undergrad))+
  geom_histogram(aes(fill = Private), color = "black", bins = 50)

ggplot(df, aes(x = Grad.Rate))+
  geom_histogram(aes(fill = Private), color = "black", bins = 50)

subset(df, Grad.Rate > 100)

df['Cazenovia College', "Grad.Rate"] <- 100

# Train Test Split

library(caTools)

set.seed(101)

sample = sample.split(df$Private, SplitRatio = 0.70)
train = subset(df, sample == T)
test = subset(df, sample == F)

# Decision Tree

library(rpart)

tree <- rpart(Private ~ . , method = "class", data = train)

tree.preds <- predict(tree, test)

head(tree.preds)

# Turn two columns into one to match original Y/N label for Private column

tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if (x >= 0.5){
    return('Yes')
  }else{
    return('No')
  }
}

tree.preds$Private <- sapply(tree.preds$Yes, joiner)

head(tree.preds)

# table() to create a confusion matrix

table(tree.preds$Private, test$Private)

#       No Yes
# No   57   9
# Yes   7 160

library(rpart.plot)

prp(tree)

# Build Random Forest

library(randomForest)

rf.model <- randomForest(Private ~ . , data = train, importance = T)

rf.model$confusion

#      No Yes class.error
# No  128  20  0.13513514
# Yes  11 385  0.02777778

# predictions

p <- predict(rf.model, test)

table(p, test$Private)

# p      No Yes
# No   56   5
# Yes   8 164