#https://www.kaggle.com/borismarjanovic/price-volume-data-for-all-us-stocks-etfs

getwd()
setwd("Desktop")
orig_data = read.csv("R Studio/Stock Market Project/Sample Stocks/a.us.txt")

# variables
#x1 = Open Price of Today
#x2 = Close Price of Yesterday
#x3 = High Price of Yesterday
#x4 = Low Price of Yesterday
#x5 = Volume of Yesterday
#Y = Change_in Stock Price of Today

# Alter/Create new dataframe to get variables we want.
row_number = seq_len(dim(orig_data)[1])
orig_data = cbind(row_number, orig_data)

create_df = function(row, full_df) {
row_num = as.numeric(row["row_number"])
  #prevent first row from being null, couldn't be a dataframe
  if (row_num == 1){
    price_change_today = 0
    open_today = 0
    close_yest = 0
    high_yest = 0
    low_yest = 0
    volume_yest = 0
  }
  
  if (row_num >=2){
    open_today = as.numeric(row["Open"])
    close_today = as.numeric(row["Close"])
    price_change_today = open_today - close_today
   
    close_yest = as.numeric(full_df[(row_num-1),]["Close"])
    high_yest = as.numeric(full_df[(row_num-1),]["High"])
    low_yest = as.numeric(full_df[(row_num-1),]["Low"])
    volume_yest = as.numeric(full_df[(row_num-1),]["Volume"])
  }
    
  vec = c(price_change_today, open_today, close_yest, high_yest, low_yest, volume_yest)
  return(vec)
    
}

variable_df = apply(X = orig_data, MARGIN = 1, FUN = create_df, full_df = orig_data)

variable_df = t(variable_df)

colnames(variable_df) = c("Price_Change_Today", "Open_Today", 
                          "Close_Yest", "High_Yest", "Low_Yest", 
                          "Volume_Yest")

# this line turns the vectors into a list type,allowing for usage of $
# (atomic = atom, one, vector type.  when need recursive type like a list)
variable_df = as.data.frame(variable_df)

#___________
# Check for relationship btw. Dependent variable and each Independent Variable.  
library(ggplot2) # needed for xlim
plot(x = variable_df$Price_Change_Today, y= variable_df$Open_Today, type = "l", xlim=c(-2,2)) 
plot(x = variable_df$Price_Change_Today, y= variable_df$Close_Yest, type = "l", xlim=c(-2,2))
plot(x = variable_df$Price_Change_Today, y= variable_df$High_Yest, type = "l", xlim=c(-2,2))
plot(x = variable_df$Price_Change_Today, y= variable_df$Low_Yest, type = "l", xlim=c(-2,2))
plot(x = variable_df$Price_Change_Today, y= variable_df$Volume_Yest, type = "l", xlim=c(-7,7))
# Residuals, the 4,519 errors (actual-predicted) are large. 
# The R-Squared (variance in y / large residual error) = small number, bad model.
simple.fit = lm(Price_Change_Today~Open_Today, data=variable_df)
summary(simple.fit)
# Our plots above show complete randomness
#____________

# Lets try Classification into Postive or Negative Price Class
  # 2 Classes and Supervised Learning, so lets do Logistic Regression or SVM

# Set Y Value, Pos Change = 1, Neg Change = 0 
variable_df = transform(variable_df, Price_Change_Today = ifelse(Price_Change_Today >= 0,1,0))

# Standardize the Data 
scale_f = function(col){
  return(scale(col))
}
scaled = lapply(variable_df[,(2:6)], scale_f)
scaled = as.data.frame(scaled)
variable_df[,(2:6)] = scaled

#split data
train_n = 4521 *.80
train_data = variable_df[1:train_n,]
test_data = variable_df[(train_n + 1):4521,]

library(stats)
# fit the model
correlations = glm(Price_Change_Today ~ Open_Today+Close_Yest+High_Yest+Low_Yest+Volume_Yest, 
    data=train_data, family=binomial)

# predict
predict_class_prob = predict(correlations, test_data, type = "response")
class = ifelse(predict_class_prob >=.5, 1, 0)

wrong = as.numeric(table(test_data$Price_Change_Today == class)[1])
right = as.numeric(table(test_data$Price_Change_Today == class)[2])
print("Model's Accuracy:")
print(right / (wrong + right))

