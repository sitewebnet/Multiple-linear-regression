install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

heart.data <- read.csv("D:/R/Datasets/heart.data.csv")
View(heart.data)
summary(heart.data)

#In the data set the independent variables are(smoking and biking)
#dependent variable is heart disease


#CHECKING IF THE DATA SET MEETS THE FOUR MAIN ASSUMPTIONS OF LINEAR REGRESSION

#....1..Autocorrelation
cor(heart.data$biking, heart.data$smoking)
 
#the correlation between biking and smoking is small(0.015/15%) so I can include both parameters in the model

#....2..Normality
hist(heart.data$heart.disease)

#the distribution of observations is roughly bell shaped and so I continue

#....3..Linearity
#use 2 scatterplots one for smoking and heart disease and another for biking and heart diseases
plot(heart.disease ~ biking, data = heart.data)
plot(heart.disease ~ smoking, data = heart.data)

 #as seen the rlship between smoking and heart disease is a bit less clear,though it still appears linear.


##PERFORM THE ANALYSIS - I aim to find if there exists a linear rlship btween biking to work, smoking and heart diseases in the survey of 500towns
#The biking rates range between (1-75)%  #Smoking rates = (0.5-30)% and heart diseases rates = (0.5-20.5)%

#FITTING THE LINEAR MODEL
heart.diseases.lm <- lm(heart.disease ~ biking + smoking, data = heart.data)
summary(heart.diseases.lm)

###...EXPLANATIONS....###
#From the output; THE ESTIMATED EFFECT OF BIKING ON HEART DISEASE IS -0.2, AND THE ESTIMATED EFFECT OF SMOKING IS 0.178
#The standard errors for these regression coefficients are very small, and the t statistics are very large (-147 and 50.4, respectively).
#The p values reflect these small errors

#....4..Homoscedasticity
par(mfrow=c(2,2))
plot(heart.diseases.lm)
par(mfrow=c(1,1))
#the residuals show no bias

#VISUALIZING THE DATA
#Here I will plot the rlship between biking and heart disease at different levels of smoking. smoking will be a factor

#..creating a new dataframe with the information needed to plot the model
plotting.data <- expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out = 30),
  smoking = c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking))
)

#..predict the y values/heart diseases based on the model
plotting.data$predicted.y <- predict.lm(heart.diseases.lm, newdata = plotting.data)

#..Rounding the smoking numbers to 2d.c to enable easy reading
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)

#..Change the smoking variable into a factor - this will enable us plot the interaction between biking and heart disease at each of leve; of smoking i choose
plotting.data$smoking <- as.factor(plotting.data$smoking)

#...plot
heart.plot <- ggplot(heart.data, aes(x = biking, y = heart.disease)) +
  geom_point()
heart.plot

#..adding the regression lines
heart.plot <- heart.plot +
  geom_line(data = plotting.data, aes(x = biking, y = predicted.y, color = smoking), linewidth = 1.25)
heart.plot

#..making the graph ready for publication
heart.plot <- heart.plot + theme_bw() + 
  labs(title = "Rates of heart diseases (% of population) \n as a function(of biking to work and smoking",
      x = "Biking to work (% of population)",
      y = "Heart diseases (% of population)",
      color = "Smoking \n (% of population)")
heart.plot

#..to add our regression model to the graph
heart.plot + annotate(geom = "text", x=30, y=1.75, label=" = 15 + (-0.2*biking) + (0.178*smoking)")


#Additional conlusion
#In my survey of 500 towns, I found significant relationships between the frequency of biking to work and 
#the frequency of heart disease and the frequency of smoking and frequency of heart disease (p < 0 and p < 0.001, respectively).
#Specifically I found a 0.2% decrease (± 0.0014) in the frequency of heart disease for every 1% increase in biking, and 

#a 0.178% increase (± 0.0035) in the frequency of heart disease for every 1% increase in smoking.
