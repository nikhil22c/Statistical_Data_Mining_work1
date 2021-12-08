#Importing dataset into R
df<- read.csv('data1.csv') #Importing the dataset into R
summary(df) #gives the metadata of the entire dataset 

view(df) #displays the dataset 


#Cleaning the dataset
df <- subset (df, select = -c(X, X.1, X.2, X.3, X.4, X.5))
#this is done to remove the NA values from the original dataset and we are storing it in the df variable


summary(df) #again, gives the summary of the the new dataset which we have cleaned. 


df <- df [-c(41),] #this is dropping the 41st row in the original dataset as it had NA values. 
summary(df)

typeof(df) #gives the data type of the dataset we are dealing with




typeof(df$Feature2)


df$Feature2 <- as.numeric(gsub(",","",df$Feature2)) #Cleaning the Feature 2 column as it had commas and the commas were replaced by "" nothing.
#this was done as commas are a nuisance when it comes to data wrangling and playing with it. 
typeof(df$Feature2)

data <- as.matrix(df)
heatmap(data)


#Calculating the Pearson Co-efficient to see how our features affect our target variable
pearson_values = c()

column_name = c()



for (i in colnames(df)){ #Initialing a for loop to iterate through every column to calculate the pearson value
  print(i)
  v <- cor(df[i], df["Property1"], method = c("pearson"))
  pearson_values <- c(pearson_values, v)
  column_name <- c(column_name, i)
}

#print(pearson_values)

pearson_values <- data.frame(values = pearson_values, column_name = column_name) #Appending the values of pearson which are newly calculated to a new dataframe.
view(pearson_values)
#print(array)


#Creating a linear regression model


linear_model = subset (df, select = -c(Feature4, Feature5, Feature6, Feature7, Feature8, Feature9, Feature10, Feature15, Feature16, Feature17, Feature18))
linm <- lm(linear_model$Property1~., data = linear_model) #applying the linear regresison model
summary(linm)



cormatrix <- round(cor(df),2)
summary(cormatrix)

install.packages("corrplot")
library(corrplot)

corrplot(cormatrix)

install.packages("reshape2")
library(reshape2)

melted_cormatrix <- melt(cormatrix) #Creates a table of variable 1 and variable 2. using this table we plot the heatmap
head(melted_cormatrix)

install.packages("ggplot2")
library(ggplot2)

#Plotting the heat map using ggplot2 package of R
ggplot(data = melted_cormatrix, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

ggplot(data = melted_cormatrix, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "light blue")

