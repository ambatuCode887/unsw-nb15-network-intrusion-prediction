# unsw-nb15-network-intrusion-prediction
This project applies R programming and machine learning techniques to analyze and predict network intrusion attacks using the UNSW-NB15 dataset.


The dataset used in this project is not included in this repository because it was prepared and distributed for academic coursework by an authorized instructor. Due to unclear redistribution permissions, the dataset is not publicly shared. However, the dataset remains stored locally on the author's device for academic use.

To find the original datasets please go to : 
https://research.unsw.edu.au/projects/unsw-nb15-dataset?utm_source=chatgpt.com

#Instruction
setwd("") #depending how you wanna import the dataset into your programs, you can remove these 2 line
getwd() 

# Import in .csv file
df <- read.csv("UNSW-NB15_uncleaned.csv", header = TRUE, sep = ",") #for new student that wanna learn, there's 2 type of file import in csv, one is for tibble read_csv and one is for normal which is read.csv
df

#once you done these steps you should be able to see your dataset appear on your screen. 
