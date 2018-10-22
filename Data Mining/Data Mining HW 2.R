#################
#  DATA MINING  #
#      HW2      #
# ORANGE TEAM 6 #
#################

# Load necessary packages
library(haven)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

# Read in data from SAS database
df_fft <- read_sas('C:/Users/escha/OneDrive/Documents/MSA/Fall Classes/Data Mining/Data/fftsales.sas7bdat')

# Generate list of variable names
#variables <- paste(colnames(df_fft[-9]),collapse = " + ")

# Split into training and validation sets
set.seed(2018)

train_split <- createDataPartition(df_fft$y, p=0.70, list=FALSE)

fft_train <- df_fft[train_split,]
fft_val <- df_fft[-train_split,]

# Default decision tree
tree <- rpart(y ~ . -y,data = fft_train,method = 'class',parms = list(split='entropy'))

# Analyzing variable importance
tree$variable.importance

barchart(tree$variable.importance[order(tree$variable.importance)], xlab = 'Importance', horiz=T, xlim=c(0,2000),ylab='Variable', main = 'Variable Importance',cex.names=0.8, las=2, col = 'orange')

# Misclassification Scores
tscores = predict(tree,type='class')
scores = predict(tree, fft_val, type='class')
cat('Training Misclassification Rate:', sum(tscores!=fft_train$y)/nrow(fft_train))
cat('Validation Misclassification Rate:', sum(scores!=fft_val$y)/nrow(fft_val))

# Visualizing our decision tree
prp(tree, type = 0, extra = 8, leaf.round = 1, border.col = 1, box.col = brewer.pal(10,"Set3")[tree$frame$yval])