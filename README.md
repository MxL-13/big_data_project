# big_data_project


This project analyzes a data set consisting of a lot of different variables from over 200.000 recorded ATP Tennis Matches from the last 10 years. The goal is to find variables that are essential for a model to predict the outcome of tennis matches. The Dataset has a lot of variables concerning the tennis match, most of them are known to the public before the match starts, and those variables are going to be used in this analysis. It consists mostly of checking the influence and uniqueness of different data and if there are differences in the means of variables regarding if a player lost or not. Those hypotheses are then put to the test in a logistic regression that uses the least amount of variables that were deemed important. 


To recreate the project and its plots and results, just download the main branch and the data from the link in the data folder (unfortunately the data is far too big for being uploaded to Github). Make sure to download the "atp.csv" file from the link and save it into the data folder. 

Next you have to make sure that every library that is mentioned in the first few rows of the R file are installed to your RStudio, if not just run install.packages() with the name in quotationmarks for each missing one. 

Lastly you will have to add the path where you saved the project to to the "path" variable so that it looks like this: "C:/your/path/data/atp.csv".

Then you should be good to go, to explore the dataset!
