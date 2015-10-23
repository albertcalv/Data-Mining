#Principal Component Analysis
#further reading: http://gastonsanchez.com/blog/how-to/2012/06/17/PCA-in-R.html

setwd("/home/albert")
dd<-read.csv("NBA.csv",header=T, sep=",")

attach(dd)
names(dd)
summary(dd)

var_num <- data.frame(away_score, home_score, converted_x, converted_y)

#Feature Selection: ? 

#P-TEST
#Decision rule is: p_value<=0.05: Signifficant association



#GRAPHICAL INTERPRETATION