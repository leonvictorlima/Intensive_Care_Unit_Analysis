################ ##################

# This project is part of EpimedSolutions challenge. Its descriptions is showed
# below:

# You have also received the file "dataset.RData" together with this document.
# The .RData file contains data on 10.000 patients that has been admitted to 
# a certain Intensive Care Unit (ICU). It is your responsibility 
# to create an R package to help to analyze this data.

# Steps it must have:

# The package should contain functions to:

# 1) make the appropriate treatment to the dataset.

# 2) produce some plots to understand the relationship between the variables
# and get some insights about what increases the chances of 
# a patient to die in this ICU.

# 3) make predictions about the probability of a patient to die in this ICU.

# 4) produce a final report – in English! – using the other 
# functions created to describe the results you got while analyzing data.


# Variables descriptions:

# UnitAdmissionId: unique identifier for each patient.
# Age: age of patient (in years).
# Gender: gender of patient (“F” for female and “M” for male).
# UnitDischargeCode: "A" (alive) or "D" (dead) at the end of admission.
# UnitLengthStay: number of days each patient has been in the ICU.
# IsArterialHypertension: indicates if the patient has arterial hypertension.
# IsDiabetesComplicated: indicates if the patient has diabetes.

### #####################################

# Initial steps:

# get and Set working directory

getwd()


# Loading file from dataset
load("dataset.RData")

df <- dt

# check the data

class(df)
View(df)
dim(df)

# Libraries

library(dplyr)
library(ggplot2)
#install.packages("plotly")
library(plotly)


### Step (2) - Exploratory Analysis

# Variables type:

str(df)

# All variables were loaded as characteres. we have to change it. Previous
# adapt its values, we have to analyse whether NA values and the values 
# variability.

# Show data frame detailed:
summary(df)

# NA values?
sum(is.na(df))

# Where are them?
which(is.na(df))

# Look at one example:
View(df[30284,])

# NULL values?
sum(is.null(df))

# As the NA values are not contunded for analysis, we will drop them;

df <- na.omit(df)


# NA values?
sum(is.na(df))

# In description, UnitDischargeCode has 02 (two) values: A (alive) or 
# D(dead), and Gender: F (female) or M (male), and IsArterialHypertension:
# 0 (no) or 1 (yes), and IsDiabetesComplicated: 0 (no) or 1 (yes).
# Based on that, we must convert it to factor.

# Fuction to Re-type variables to factor 

tofactor <- function(dataframe, list_variable){
  for (items in list_variable){

    var_labels = (length(levels(factor(dataframe[[items]])))-1)
    var_levels = levels(factor(dataframe[[items]]))
  
    dataframe[[items]] <- factor(dataframe[[items]], levels = var_levels, labels = c(0:var_labels))
  }
  return(dataframe)
}

#tofactor <- function(dataset, variable){
#  for (items in variable){
#    dataset[[items]] <- as.factor(dataset[[items]])
#  }
#  return(dataset)
#}

# Fuction to Re-type variables to numeric 

tonumeric <- function(dataframe, list_variabel){
  for (items in list_variabel){
    dataframe[[items]] <- as.numeric(dataframe[[items]])
  }
  return(dataframe)
}

# Creating list of variables to be changed to Factor;

var_factor_list <- list('UnitDischargeCode', 'Gender', 'IsArterialHypertension',
                        'IsDiabetesComplicated')


# List for variables to numeric;

var_num_list <- list("Age","UnitLengthStay")

# Set new DataFrame with variables to Factor


df2 <- df %>% tofactor(var_factor_list) %>% tonumeric(var_num_list)


# Reorder column by position and drop out ID

#colnames(df)
df3 <- df2[, c(2,3,5,6,7,4)]

View(df3)

summary(df3)

# Plot 01

# The first step in this analyses is understand the distribution of age and what is the room
# which hosted more patients in UCI. 

# Making use of simple elements, it is possible have in mind the type
# of the data and circunstances, in order to identify any desequilibrium in the dataset. 

# In Distribution of Age's plot is seen the major age is between 60 and 80 years.

?ggplot


# Distribuition of Age

par(mfrow=c(1,1))

hist(df3$Age, breaks=10, col=rgb(1,0,0,0.5), xlab="Age", 
     ylab="Quantity", main="Distribution of Age" )

# Plot 02 - UCI Gender Catalog

# Here is showed how is distributed the gender in the UCI; The allocation of
# both gender is basically the same.


pie_plot <- plot_ly(type='pie',labels = c("Female", "Male"),
            values=table(df3$Gender),
            insidetextfont = list(color = '#FFFFFF'),
            textinfo='label+percent',
            insidetextorientation='radial',
            marker = list(colors = c('rgb(211,94,96)', 'rgb(114,147,203)'),
                          line = list(color = '#FFFFFF', width = 1))) 
        
pie_plot <- pie_plot %>% layout(title = 'UCI Gender Catalog')
pie_plot

# Plot 03 - Boxplot of Gender by Age

# Using boxplot we can see some important aspects:

# 1) Women age is more than the men age;
# 2) The concentration of 25 % of ages are around: 55 years old for both;
# 3) Median: Men is less than Women;
# 4) 75 % is around 80 years old for Women, while 75 years old for men.
# 5) Both have some youngs in UCI, the men years range is more than women though.


ggplot( df3,aes(y=Age, x=Gender, fill=Gender,outlier.colour = "red")) + 
  geom_boxplot(alpha=0.3) +
  scale_y_continuous(name = "Age",
                     breaks = seq(0, 125, 25),
                     limits=c(0, 125)) +
  scale_x_discrete(labels = c('Female','Male'))+
  scale_fill_discrete(labels = c('MARTE','Male')) +
  ggtitle("Boxplot of Gender by Age") + labs(fill = "Dose (mg)") +
  theme_bw()



# Plot 04 - Total of Unit Discharge Code

# The total of dead and alive is a precious measurement wich contributes
# to create an overview about how many people did not survive.

# The number of patients survived is pretty superior for not survived. 

  plot_ly(x = c("Alive","Died"), y = table(df3$UnitDischargeCode), type = 'bar',
          marker = list(color = c('rgb(60,179,113)','rgb(0,0,0)'),
                        textinfo='percent',
                        line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Total Died and ALive (Unit Discharge Code)')


# Plot 05 - Count of Died and Alive by Gender

# In addition with the last plot, to have it more precisely and segmented by gender
# brings  the concept how it is distributed.
  
# And again, there is not difference between women or men died and survived, their total
# are very similar.


# Filtering data

male <- df3 %>% filter(df3$Gender == 1)
female <- df3 %>% filter(df3$Gender == 0)


# plotting

plot_ly(x = c("Alive", "Died"), y = table(male$UnitDischargeCode), type = 'bar', name = 'Male',
        marker = list(color = 'rgb(114,147,203)', 
                      line = list(color = '#FFFFFF', width = 1))) %>%
add_trace(y = table(female$UnitDischargeCode), name = 'Female',
          marker = list(color = 'rgb(211,94,96)',
                        line = list(color = '#FFFFFF', width = 1))) %>% 
layout(title = 'Total Died and Alive by Gender (Unit Discharge Code)', barmode = 'stack')

# Plot 06

# As previously exhibited, there is not a massive difference for the number of lost lives
# for gender. In this subject yet, another observation is showed by Total Died by Age.

# It is observed how many lost their lives segregated by age. People with more than 60 years
# are the substiantial mass. By the way, 04 (four) babies, 0 (zero) age, lost their lives
# as well.


df3_age_dead <- df3 %>% subset(select = c(1,6))
df3_age_dead_table <- data.frame(table(df3_age_dead))
df3_age_dead_table <- df3_age_dead_table %>% filter(UnitDischargeCode == 1)
View(df3_age_dead_table)

plot_ly(x = df3_age_dead_table$Age, y = df3_age_dead_table$Freq, type = 'bar',
        marker = list(color = 'rgb(0,0,0)',
                      line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Total Died by Age',
         barmode = 'group',
         xaxis = list(title = "Age"),
         yaxis = list(title = "Died"))

# Plot 07

# Futhermore, How is the behavior of people in the rooms?

# 1) Most part of the time, the rooms are occupied for 01 (one) day.
# 2) People who alive concentrate more attend for rooms.
# 3) As long as the number of days decrease, the number of people making use of
# rooms is less.

# Selecting variables 
df3_unit <- df3 %>% subset(select = c(3,6))

# Count and transform in dataframe 
df3_unit <- data.frame(table(df3_unit))

# Spliting values for Alive and Died
df3_unit_died <- filter(df3_unit, UnitDischargeCode == 1)
df3_unit_alive <- filter(df3_unit, UnitDischargeCode == 0)

# Mean

mean_days_unit_died <- mean(df3_unit_died$Freq)
mean_days_unit_alive <- mean(df3_unit_alive$Freq)
  
plot_ly(x = df3_unit_alive$UnitLengthStay, y = df3_unit_alive$Freq,
        name = 'Alive',
        type = 'scatter',
        mode = 'lines',
        color = I('Green'),
        line = list(width = 1))%>%
  add_trace(y = df3_unit_died$Freq,
            name = 'Died',
            color = I('Black'),
            line = list(width = 1)) %>%
  layout(title = 'Quantity of the Days Hosted by Room',
         barmode = 'group',
         xaxis = list(title = "Room",tickangle = -45),
         yaxis = list(title = "Number of the days hosted")) 

# Plot 08

# The last analyse step is the relation between Arterial Hypertension
# and Diabetes.

# The result shows most part of patients have Hypertension and
# no Diabetes, meanwhile, the second big part of patients do not have none.

# This insight is also related for people who have died, in this case: more dead 
# for Hypertension and none disease.



# Set and Filtering the data

# Dead

df3_hyper_diab_dead <- df3 %>% subset(select = c(4:6))
df3_hyper_diab_dead <- data.frame(table(df3_hyper_diab_dead))
df3_hyper_diab_dead <- df3_hyper_diab_dead %>% filter(UnitDischargeCode == 1)

# Alive

df3_hyper_diab_alive <- df3 %>% subset(select = c(4:6))
df3_hyper_diab_alive <- data.frame(table(df3_hyper_diab_alive))
df3_hyper_diab_alive <- df3_hyper_diab_alive %>% filter(UnitDischargeCode == 0)


# plotting

status <- c('None',
            'Just Hypertension',
            'Just Diabetes ',
            'Both')


plot_ly(df3_hyper_diab_dead, y = status , x = ~Freq , type = 'bar', name = 'Died',
        marker = list(color = 'rgb(0,0,0)', 
                      line = list(color = '#FFFFFF', width = 1))) %>%
  add_trace(x = df3_hyper_diab_alive$Freq, name = 'Alive',
            marker = list(color = 'rgb(60,179,113)',
                          line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Total Died and Alive by Hypertension and Diabetes',
         xaxis = list(title = "Quantity"),
         yaxis = list(title = "Condition"),
         barmode = 'stack')

# Conclusions

# 1) The most part of people who use the UCI are between 60 to 80 years old;
# 2) Basically, there is no concentration of gender in UCI. Both have almost the same 
# population;
# 3) Women age is more than the men age;
# 4) The concentration of 25 % of ages are around: 55 years old for both;
# 5) Median: Men is less than Women;
# 6) 75 % is around 80 years old for Women, while 75 years old for men;
# 7) Both have some youngs in UCI, the men years range is more than women though;
# 8) The number of patients who survived is pretty superior for not survived;
# 9) There is not difference for number of deaths by gender. They are vey similiar;
# 10) In relation deaths: people with more than 60 years are the majority;
# 11) 04 (four) babies, 0 (zero) age, lost their lives;
# 12) Most part of the time, the rooms are occupied for 01 (one) day;
# 13) People who alive concentrate more attend for rooms;
# 14) As long as the number of days decrease, the number of people making use of
# decreses;
# 15) Most part of patients have Hypertension and no Diabetes;
# 16) the second big part of patients do not have any disease.
# 17) Majority of deaths are for Hypertension and none disease; 


########### Machine Learning ###########

# libraries

library(caret)
library(lattice)
library(grid)

View(df3)

# Target variable

# Count
table(df3$UnitDischargeCode)

# Proportion
target <- table(df3$UnitDischargeCode)
round(prop.table(target),digits=2)

# Variables are unbalaced. We have to balance them.
# We will make use of SMOTE and ROSE techniques for that.


# Seed
set.seed(125)

# Split data

index <- createDataPartition(df3$UnitDischargeCode, p = 0.8, list = FALSE)

train_data <- df3[index, ]
test_data  <- df3[-index, ]

# SMOTE

control_smote <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              verboseIter = FALSE,
                              sampling = "smote")
# ROSE

control_smote_rose <- trainControl(method = "repeatedcv",
                                   number = 10,
                                   repeats = 10,
                                   verboseIter = FALSE,
                                   sampling = "rose")

# Model 01 - LDA

# SMOTE

fit.lda<- caret::train(UnitDischargeCode ~ .,
                       data = train_data,
                       method = "lda",
                       preProcess = c("scale", "center"),
                       trControl = control_smote)


# FOLD
fit.lda_rose <- caret::train(UnitDischargeCode ~ .,
                             data = train_data,
                             method = "lda",
                             preProcess = c("scale", "center"),
                             trControl = control_smote_rose)

# Model 02 - LDA2

# SMOTE

fit.lda2<- caret::train(UnitDischargeCode ~ .,
                        data = train_data,
                        method = "lda2",
                        preProcess = c("scale", "center"),
                        trControl = control_smote)


# ROSE
fit.lda2_rose <- caret::train(UnitDischargeCode ~ .,
                              data = train_data,
                              method = "lda2",
                              preProcess = c("scale", "center"),
                              trControl = control_smote_rose)


# Model 03 - Random Forest 

# SMOTE
fit.rf<- train(UnitDischargeCode ~ .,
               data = train_data,
               method = "rf",
               preProcess = c("scale", "center"),
               trControl = control_smote)

# ROSE
fit.rf_rose <- train(UnitDischargeCode ~ .,
                     data = train_data,
                     method = "rf",
                     preProcess = c("scale", "center"),
                     trControl = control_smote_rose)


# Model 04 - Support Vector Machines with Linear Kernel (svmLinear)

# SMOTE
fit.svmLinear<- train(UnitDischargeCode ~ .,
                      data = train_data,
                      method = "svmLinear",
                      preProcess = c("scale", "center"),
                      trControl = control_smote)

# ROSE
fit.svmLinear_rose <- train(UnitDischargeCode ~ .,
                            data = train_data,
                            method = "svmLinear",
                            preProcess = c("scale", "center"),
                            trControl = control_smote_rose)


# Model 05 - Support Vector Machines with Radial Basis Function Kernel (svmRadial)


# SMOTE
fit.svmRadial<- train(UnitDischargeCode ~ .,
                      data = train_data,
                      method = "svmRadial",
                      preProcess = c("scale", "center"),
                      trControl = control_smote)

# ROSE
fit.svmRadial_rose <- train(UnitDischargeCode ~ .,
                            data = train_data,
                            method = "svmRadial",
                            preProcess = c("scale", "center"),
                            trControl = control_smote_rose)

# Evaluatiing models
res <- resamples(list(LDA = fit.lda,
                      LDA_rose = fit.lda_rose,
                      LDA_2 = fit.lda2,
                      LDA_2_rose = fit.lda2_rose,
                      RF = fit.rf,
                      RF_rose = fit.rf_rose,
                      LinearSVM = fit.svmLinear,
                      LinearSVM_rose = fit.svmLinear_rose,
                      RadialSVM = fit.svmRadial))
# Analysis
summary(res)
bwplot(res)
dotplot(res)

# Confusion matrix

# Models:
fit.lda
fit.rf_rose #melhor = mtry = 2
fit.svmLinear #


# LDA

# confusion matrix
lda_prediction = predict(fit.lda, test_data[1:5])
cm_lda = confusionMatrix(lda_prediction,test_data$UnitDischargeCode)
cm_lda

# dataframe predicted x real
lda_dataframe <- as.data.frame(cbind(lda_prediction,test_data$UnitDischargeCode))
colnames(lda_dataframe) <- c('predicted', 'real')

# creating pred
pred_lda <- prediction(lda_dataframe$predicted, lda_dataframe$real)

# plot ROC

plot.roc.curve(pred_lda, title.text = "LDA - Curva ROC")
plot.pr.curve(pred_lda, title.text = "LDA - Curva Precision/Recall")


# Random FOrrest

rf_prediction = predict(fit.rf, test_data[,1:5])
cm_rf = confusionMatrix(rf_prediction, test_data$UnitDischargeCode)
cm_rf

# dataframe predicted x real
rf_dataframe <- as.data.frame(cbind(rf_prediction, test_data$UnitDischargeCode))
colnames(rf_dataframe) <- c('predicted', 'real')

# creating pred
pred_rf <- prediction(rf_dataframe$predicted, rf_dataframe$real)

#Plot ROC
plot.roc.curve(pred_rf, title.text = "RF - Curva ROC")
plot.pr.curve(pred_rf, title.text = "RF - Curva Precision/Recall")

# svmLinear

# Confusion Matrix

svm_prediction = predict(fit.svmLinear, test_data[,1:5])
cm_svm <- confusionMatrix(svm_prediction, test_data$UnitDischargeCode)
cm_svm

# dataframe predicted x real
svm_dataframe = as.data.frame(cbind(svm_prediction, test_data$UnitDischargeCode))
colnames(svm_dataframe) <- c('predicted', 'real')

# creating pred
pred_svm <- prediction (svm_dataframe$predicted, svm_dataframe$real)

# Plot ROC
plot.roc.curve(pred_svm, title.text = "SVM - Curva ROC")
plot.pr.curve(pred_svm, title.text = "SVM - Curva Precision/Recall")



# The choosen one is SVM;

# save the model to disk:

# LDA
saveRDS(fit.lda, './fit.lda.rds')

# sVM

saveRDS(fit.svmLinear,'./fit.svmLinear.rds')
