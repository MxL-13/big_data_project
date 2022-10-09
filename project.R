#Loading the data and libraries
library(aod)
library(tidyverse)
library(ggplot2)
library(ROCR)


#After downloading the folder, please update the path to your directory and download the atp.csv file from the link to the same subfolder
path <- "data/atp.csv"
df <- read.csv(path, stringsAsFactors=FALSE)

#Init counter for picture output
x = 0

#Dropping columns that are not needed/not numerical and putting the ground truth in a seperate df
#then drop every row that has NA values, as there is more then enough left
groundT <- df_nona["GRes_CUR_1"]
drop <- c("GameD","Name_1", "Name_2","Result_CUR_1","Result_CUR_2","SETS_0.2_CUR_1","SETS_1.2_CUR_1", "SETS_2.0_CUR_1", "SETS_2.1_CUR_1","TName","TourCountry")
df_num <- df[,!(names(df) %in% drop)]
df_num <- drop_na(df_num)


#Filtering for interesting columns to display/ analyze later
test <- df_num[ , grepl( "BreakPoints|DaysFromLast" , names(df_num) ) ]
test <- df_num[ , grepl( "Serve1|Ret" , names(df_num) ) ]
test1 <- test[ , grepl( "2" , names(test) ) ]
test1 <- test[ , grepl( "1" , names(test) ) ]


#Making the ground truth able to be colorcoded
cls <- character(nrow(groundT))
cls[] <- "black"
cls[groundT == 1] <- "win"
cls[groundT == 0] <- "lose"

#Print the pairplots of the filtered data
x = x+1
str <- paste("C://Users//mleut//Desktop//STU-Big Data//Fundamentals of Big Data//plots//plotssaving_plot", toString(x), ".png")
png(file=str,width=2000, height=2000)
pairs(test1, col = cls)
dev.off()

#Look for a pattern in the last_Ret 
df_num$GRes_CUR_1[df_num$GRes_CUR_1 == 1] <- "win"
df_num$GRes_CUR_1[df_num$GRes_CUR_1 == 0] <- "lose"
print(ggplot(df_num) + geom_bar(mapping = aes(x= IsLastRet_CUR_2, fill = GRes_CUR_1)))
print(ggplot(df_num) + geom_bar(mapping = aes(x= IsLastRet_CUR_1, fill = GRes_CUR_1)))
df_num$GRes_CUR_1[df_num$GRes_CUR_1 == "1"] <- 1
df_num$GRes_CUR_1[df_num$GRes_CUR_1 == "0"] <- 0

#Check for patterns in the 1st serve percentage
df_win <- df_num[df_num$GRes_CUR_1 == 1,]
df_lose <- df_num[df_num$GRes_CUR_1 == 0,]

ggplot() + geom_histogram(df_win, mapping = aes(x=Serve1stWonPCT_A_1), fill="red") +
  geom_histogram(df_lose, mapping = aes(x=Serve1stWonPCT_A_1), fill="blue", alpha=0.4)

#Check the variances of different but related variables to check for information
var(df_win$Serve1stPCT_A_1)
var(df_lose$Serve1stPCT_A_2)
var(df_win$Serve1stPCT_L5_1)
var(df_win$Serve1stPCT_1)

#Make a ttest to see if the means are the same to be able to see if there is a notable difference
t.test(df_win$Serve1stPCT_A_1, df_lose$Serve1stPCT_A_1, var.equal = TRUE)
t.test(df_num$Serve1stPCT_A_1, df_num$Serve1stPCT_L5_1)
print(ggplot(df_num, aes(x = Serve1stPCT_A_1 , fill = GRes_CUR_1)) + geom_density(alpha = 0.5))

#Check if any of those variables are equal/comparable
one.way <- aov(Serve1stPCT_L5_1 ~ Serve1stWon_L5_1 + Serve2ndWon_L5_1 + ReceivingPointsWon_L5_1, data = df_num)
summary(one.way)


#Check for interesting information in the Surface Data, depending on wins/losses
print(ggplot(df_num) + geom_bin2d(mapping = aes(y = GRes_CUR_1, x = Surface)))


#Check if the Aces parameter are unique or not
test2 <- df_num[ , grepl( "Aces" , names(df_num) ) ]
fa <- factanal(test2, factors = 3,  rotation = "varimax", lower = 0.0001)
fa

#Try to predict the winner through logistic regression with parameters that seem
#usefull as found out in the analysis
df_num$GRes_CUR_1 <- as.numeric(df_num$GRes_CUR_1)
pred <- glm(GRes_CUR_1 ~ Serve2ndWonPCT_L5_1 + DaysFromLast_CUR_2 + ReceivingPointsWonPCT_L5_2 + DaysFromLast_CUR_2 + IsLastRet_CUR_2 + IsLastRet_CUR_1 + DaysFromLast_CUR_1 + Serve1stWon_L5_1 + Serve1stPCT_L5_1 + Serve1stPCT_L5_2, data=df_num, family = "binomial")
summary(pred)
confint(pred)
exp(coef(pred))

#Try without IsLastRet1
pred_comp <- glm(GRes_CUR_1 ~ Serve2ndWonPCT_L5_1 + DaysFromLast_CUR_2 + ReceivingPointsWonPCT_L5_2 + DaysFromLast_CUR_2 + IsLastRet_CUR_2 + DaysFromLast_CUR_1 + Serve1stWon_L5_1 + Serve1stPCT_L5_1 + Serve1stPCT_L5_2, data=df_num, family = "binomial")
summary(pred_comp)
confint(pred_comp)
exp(coef(pred_comp))

#Compare both (higher = without ISLastRet1)
pchisq( 41732-41731, 30358-30357, lower=FALSE)

#Try out the model for predictions
prediction_mod <- predict(pred_comp, type="response")

predObj <- prediction(prediction_mod, df_num$GRes_CUR_1 )
rocObj <- performance(predObj, measure="tpr", x.measure="fpr")
aucObj <- performance(predObj, measure= "auc")

plot(rocObj, main= paste( "Area under the curve:",
                          round(aucObj@y.values[[1]] ,4)))
a <- rocObj@x.values
# extract the alpha(threshold), FPR, and TPR values from rocObj
alpha<- round(as.numeric(unlist(rocObj@alpha.values)),4)
fpr <- round(as.numeric(unlist(rocObj@x.values)),4)
tpr <- round(as.numeric(unlist(rocObj@y.values)),4)
#plot true positives vs false positives
par(mar = c(5,5 , 2, 5))
plot(alpha,tpr, xlab="Threshold" , xlim=c(0,1),
     ylab="True positive rate " , type= "l" )
par(new="True")
plot(alpha,fpr, xlab="" , ylab="", axes=F, xlim=c(0,1), type= "l" )
axis(side=4)
mtext(side=4, line=3, "False positive rate" )
text(0.18 , 0.18, "FPR")
text(0.58 , 0.58 , "TPR") 
