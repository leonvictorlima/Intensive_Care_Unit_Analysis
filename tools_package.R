
# Clean NA values
# Require: dataframe.
clean_dataframe <- function(dataframe){
  newdataframe<-dataframe #load new dataframe
  newdataframe <- na.omit(newdataframe)#Clean NA values
  return(newdataframe)
}

# Convert to factor variables.
# Require: dataframe and one list of variables name to be applied.

tofactor <- function(dataframe, list_variable){
  for (items in list_variable){
    
    var_labels = (length(levels(factor(dataframe[[items]])))-1)
    var_levels = levels(factor(dataframe[[items]]))
    
    dataframe[[items]] <- factor(dataframe[[items]], levels = var_levels, labels = c(0:var_labels))
  }
  return(dataframe)
}


# Convert to numeric variables.
# Require: dataframe and one list of variables name to be applied.
tonumeric <- function(dataframe, list_variabel){
  for (items in list_variabel){
    dataframe[[items]] <- as.numeric(dataframe[[items]])
  }
  return(dataframe)
}

# Reorder column by position and drop out ID
# Require: Dataframe to be remodeled and array with number of columns order,
# for example: dataframe with 4 columns -->new order: array_columns = c(2,4,1,3)
reorder_dataframe <- function(dataframe, array_columns){
  newdataframe <- dataframe
  newdataframe <- newdataframe[,array_columns]
  return(newdataframe)
}

# Require: library(dplyr)
# This function formats the dataframe changing its type for factor and numeric.
# For that, both lists with columns name must be passed to it.
format_dataframe <- function(dataframe, factor_list, numeric_list){
  
  require(dplyr)
  
  newdataframe <- dataframe
  newdataframe <- na.omit(newdataframe)
  
  # Functions
  
  tofactor <- function(dataframe, list_variable){
    for (items in list_variable){
      
      var_labels = (length(levels(factor(dataframe[[items]])))-1)
      var_levels = levels(factor(dataframe[[items]]))
      
      dataframe[[items]] <- factor(dataframe[[items]], levels = var_levels, labels = c(0:var_labels))
    }
    return(dataframe)
  }
  
  # Fuction to Re-type variables to numeric 
  
  tonumeric <- function(dataframe, list_variabel){
    for (items in list_variabel){
      dataframe[[items]] <- as.numeric(dataframe[[items]])
    }
    return(dataframe)
  }
  
  newdataframe2 <- newdataframe %>% tofactor(factor_list) %>% tonumeric(numeric_list)
  
  return(newdataframe2)
}


# Require: library(dplyr)
# As the same as format_dataframe but incremented with array with columns order,
# for example: array <- c(2,1,3).
format_dataframe_Reorder <- function(dataframe, factor_list, numeric_list, array_columns_order){
  
  require(dplyr)
  
  newdataframe <- dataframe
  newdataframe <- na.omit(newdataframe)
  
  # Functions
  
  tofactor <- function(dataframe, list_variable){
    for (items in list_variable){
      
      var_labels = (length(levels(factor(dataframe[[items]])))-1)
      var_levels = levels(factor(dataframe[[items]]))
      
      dataframe[[items]] <- factor(dataframe[[items]], levels = var_levels, labels = c(0:var_labels))
    }
    return(dataframe)
  }
  
  # Fuction to Re-type variables to numeric 
  
  tonumeric <- function(dataframe, list_variabel){
    for (items in list_variabel){
      dataframe[[items]] <- as.numeric(dataframe[[items]])
    }
    return(dataframe)
  }
  
  newdataframe2 <- newdataframe %>% tofactor(factor_list) %>% tonumeric(numeric_list)
  
  newdataframe3 <- newdataframe2[,array_columns_order]
  
  return(newdataframe3)
}


# Plot histogram
# Require value (as numeric), bins (as numeric) and labels
plot.hist <- function(value,bins,main_label.text, y_label.text,x_label.text){
  hist(value, breaks = bins, col = rgb(1,0,0,0.5),
       xlab = x_label.text,
       main = main_label.text,
       ylab = y_label.text)
}

#Pieplot
# Require Plotply library, values, array_columns, main text label
plot.pie <- function(value, labels, main_label.text){
  
  require(plotly)
  plot_ly(type='pie',labels = labels,
                    values=value,
                    insidetextfont = list(color = '#FFFFFF'),
                    textinfo='label+percent',
                    insidetextorientation='radial',
                    marker = list(colors = c('rgb(211,94,96)', 'rgb(114,147,203)'),
                                  line = list(color = '#FFFFFF', width = 1))) %>%
    layout(title = main_label.text)
  
}

# Box plot
# Requires: dataframe, value for y axis, value for y axis, value for
# fill (colour), text (or vector) with same size of y to y_label,
# text (or vector) same size of x to y_label, text (or vector) same size
# of x to legend_labels, main text; 
plot.boxplot<-function(dataframe,y,x,fill,y_label.text,x_label.text,
                       legend_labels.text,legend.text,title.text){
  require(ggplot2)
  
  ggplot(dataframe,aes(y=y, x=x, fill= x,outlier.colour = "red")) + 
    geom_boxplot(alpha=0.3) +
    scale_y_continuous(name = y_label.text,
                       breaks = seq(0, 125, 25),
                       limits=c(0, 125)) + 
    scale_x_discrete(name = x_label.text,
                     labels = legend_labels.text)+
    scale_fill_discrete(labels = legend_labels.text) + 
    ggtitle(title.text) + labs(fill = legend.text) +
    theme_bw()
  
}

# Barplot

# Requires: Plotly library, value for x axis, value for y axis, and main text; 
plot.bar <- function(x, y, title.text, x_label.text, y_label.text){
  
  require(plotly)
  plot_ly(x = x, y = y, type = 'bar',
          marker = list(color = c('rgb(60,179,113)','rgb(0,0,0)'),
                        textinfo='percent',
                        line = list(color = '#FFFFFF', width = 1))) %>%
    layout(title = title.text,
           xaxis = list(title = x_label.text),
           yaxis = list(title = y_label.text))
  
}

# Stack-Barplots
# Requires: Plotly library, value for x axis, value for y axis, names
# for x and y, and main text; 
plot.stackbar <- function(x, y, namestack01.text, y2, namestack02.text,
                          main_title.text){
  require(plotly)
  plot_ly(x = x, y = y, type = 'bar', name = namestack01.text,
          marker = list(color = 'rgb(114,147,203)', 
                        line = list(color = '#FFFFFF', width = 1))) %>%
    add_trace(y = y2, name = namestack02.text,
              marker = list(color = 'rgb(211,94,96)',
                            line = list(color = '#FFFFFF', width = 1))) %>% 
    layout(title = main_title.text, barmode = 'stack')
  
}

# Linear plot
# Requires: Plotly library, value for x axis, value for y axis, names
# for x and y, and main text, and x axis, and y axis; 
plot.linear_run <- function(x,y,name01,y2, name02,title.text,
                        x_label.text, y_label.text){
  require(plotly)
  plot_ly(x = x, y = y,
          name = name01,
          type = 'scatter',
          mode = 'lines',
          color = I('Green'),
          line = list(width = 1))%>%
    add_trace(y = y2,
              name = name02,
              color = I('Black'),
              line = list(width = 1)) %>%
    layout(title = title.text,
           barmode = 'group',
           xaxis = list(title = x_label.text,tickangle = -45),
           yaxis = list(title = y_label.text)) 
  
}


# Linear plot
# Requires: Plotly library, value for x axis, value for y axis,and values
# for x2, and names for x and x2, and main text, and x axis, and y axis; 
plot.newbar <- function(x, y, name01,x2, name02,title.text,x_label.text,
                        y_label.text){
  
  require(plotly)
  
  plot_ly(x = x, y=y , type = 'bar', name = name01,
          marker = list(color = 'rgb(0,0,0)', 
                        line = list(color = '#FFFFFF', width = 1))) %>%
    add_trace(x = x2, name = name02,
              marker = list(color = 'rgb(60,179,113)',
                            line = list(color = '#FFFFFF', width = 1))) %>%
    layout(title = title.text,
           xaxis = list(title = x_label.text),
           yaxis = list(title = y_label.text),
           barmode = 'stack')
  
}

# Machine learning predictions
# Important: the data must be in the same format
# which was created the model.
machine_learning_prediction <- function(dataframe){
  super_model <- readRDS("./fit.svmLinear.rds")
  # make a predictions on "new data" using the final model
  final_predictions <- predict(super_model, dataframe)
  return(final_predictions)
}

create_report<- function(directory.text, output.text){

  return(rmarkdown::render(directory.text,
                           output.text))
}

