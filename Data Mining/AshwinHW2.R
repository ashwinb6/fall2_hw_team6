library(haven)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

#read in data
data = read_sas("C:\\Users\\thebi\\OneDrive\\Documents\\Data Mining\\fftsales.sas7bdat")
options(digits=2)
set.seed(7515)

# Splitting data
split = createDataPartition(data$y, p=.70, list=FALSE)
train = data[split,]
val = data[-split,]
dim(train)
dim(val)


# Making Decision Tree
tree = rpart(y ~ . - y, data=train, method='class', parms = list(split='entropy'))

.pardefault = par()
par(mai=c(.2,.2,.2,.2))
plot(tree,uniform=T)
text=tree
text(tree, use.n=T)
par(.pardefault)
prp(tree, type=0, extra=8, leaf.round=1, border.col=1, box.col=brewer.pal(10,"Set3")[tree$frame$yval])

#Which variables are most important? ConsumerGroup, balance, last_campaign_outcome
barchart(tree$variable.importance[order(tree$variable.importance)], xlab='Importance',
         horiz=T, ylab='Variable', main='Variable Importance', col = 'sky blue')

#Training Misclassification Rate = 0.17
tscores = predict(tree, type='class')
scores= predict(tree, val, type='class')
cat('Training Misclassification Rate:', sum(tscores!=train$y)/nrow(train))

#Validation Misclassification Rate = 
cat('Validation Misclassification Rate:', sum(scores!=val$y)/nrow(val))
