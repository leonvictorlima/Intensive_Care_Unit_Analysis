# Loading file from dataset

load("dataset.RData")

# Loading the package

source('tools_package.R')

# Assign new dataframe

df <- dt

# Set seed
set.seed(125)

# Variable list with names which will be converted to factor type;

var_factor_list <- list('UnitDischargeCode', 'Gender', 'IsArterialHypertension',
                        'IsDiabetesComplicated')

# Variable list with names which will be converted to numeric type;

var_num_list <- list("Age","UnitLengthStay")

# Order of new data frame: 
# obs: The new order will be: Age, Gender, UnitLengthStay,
# IsArterialHypertension, IsDiabetesComplicated ,and UnitDischargeCode.

order_array <- c(2,3,5,6,7,4)

# Calling function to make treatment to dataframe

library(dplyr)

df3 <- format_dataframe_Reorder(df, var_factor_list,
                                var_num_list,order_array)

View(df3)
# In this case, we will split the values to be predicted (x) and 
# the real values (y).

X <- df3[, 1:5]
y <- df3$UnitDischargeCode

# Function to predict data using Machine Learning

prediction<-machine_learning_prediction(df3)

# OBS: To check results:

caret::confusionMatrix(prediction,y)

# Report

create_report("C:/FCD/projeto_Epimedsolution/enviar/Report_ICU.Rmd", "word_document")
