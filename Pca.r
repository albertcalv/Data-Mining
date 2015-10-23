#Principal Component Analysis
#further reading: http://gastonsanchez.com/blog/how-to/2012/06/17/PCA-in-R.html

setwd("/home/albert")
dd<-read.csv("NBA.csv",header=T, sep=",")

attach(dd)
names(dd)
summary(dd)

#qd.period        = period Period of the game.
#nd.away_score    = away score Sum of points scored by away team.
#nd.home_score    = home_score Sum of points scored by home team. 
#qd.play_id       = play_id Identifier of the play. 
#nd.num           = num of free shots a player scores. 
#nd.outof         = outof Number of free shots realized. 
#nd.points        = points Points that this player does. 
#nd.shot_distance = shot_distance The distance from where the shot is taken. 
#nd.original_x    = original_x (*) X coordinate.
#nd.original_y    = original_y  (*) Y coordinate. 

data_pca <- data.frame(num, outof, points, shot_distance, original_x, original_y)

# PRINCIPAL COMPONENT ANALYSIS OF dcon
pca1 = prcomp(data_pca, scale=T)
class(pca1)
attributes(pca1)
print(pca1)
summary(pca1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
inerProj<- pca1$sdev^2 
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner
pinerEix

#PLOT OF INDIVIDUALS
nd = 6
data_pca[1,]

#eigen values
lbd = pca1$sdev[1:nd]^2
U = pca1$rotation[,1:nd]
Psi = pca1$x[,1:nd]
Psi[1,]

iden = row.names(data_pca)
etiq = names(data_pca)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

plot(Psi[,1],Psi[,2])


#PLOT OF VARIABLES
Phi = cor(data_pca,Psi)
plot(Phi[,1],Phi[,2],type="none",xlim=c(min(Phi[,1],0),max(Phi[,1],0)))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, Phi[,1], Phi[,2], length = 0.07,col="blue")
text(Phi[,1],Phi[,2],labels=etiq,col="darkblue")


#BIPLOT 
biplot(Psi,U)


#VARIANCES - sedimentacion
screeplot(pca1, type="lines",col=3)

pca1$rotation 


#old PRINCIPAL COMPONENT ANALYSIS OF dcon
pca1 = prcomp(data_pca, scale=T)
class(pca1)
attributes(pca1)
print(pca1)
summary(pca1)

#total variance 
barplot(summary(pca1)$importance[2, ])

plot(pca1$x[, 1:2], pch = 19)
abline(h = 0, v = 0, lty = 2, col = 8)

#biplot 
biplot(pca1)
abline(h = 0, v = 0, lty = 2, col = 8)

plot(pca1)
#using pca <- offers more detailed results
# Tools -> Install Packages ... -> FactoMineR
library(FactoMineR)
pca2 = PCA(data_pca, graph = FALSE)
print(pca)

#Matrix with eigenvalues
print(pca$eig)

#Correlations between variables and PCs
print(pca$var$coord)

# PCs (aka scores)
print(pca$ind$coord)


