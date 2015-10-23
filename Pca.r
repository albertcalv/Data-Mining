#Principal Component Analysis
#further reading: http://gastonsanchez.com/blog/how-to/2012/06/17/PCA-in-R.html

setwd("/home/albert")
dd<-read.csv("NBA.csv",header=T, sep=",")

attach(dd)
names(dd)
summary(dd)

#dd.period        = period Period of the game.
#dd.away_score    = away score Sum of points scored by away team.
#dd.home_score    = home_score Sum of points scored by home team. 
#dd.play_id       = play_id Identifier of the play. 
#dd.num           = num of free shots a player scores. 
#dd.outof         = outof Number of free shots realized. 
#dd.points        = points Points that this player does. 
#dd.shot_distance = shot_distance The distance from where the shot is taken. 
#dd.converted_x   = original_x (*) X coordinate.
#dd.converted_y   = original_y  (*) Y coordinate. 

data_pca <- data.frame(away_score, home_score, converted_x, converted_y)

# PRINCPLE COMPONENT ANALYSIS OF DCON
pca = prcomp(data_pca, scale=T)
attributes(pca)
print(pca)
summary(pca)
  #The last 2 commands generate the summary of the pca : pca indicates the importance
  #of each components in the analysis
  #Explicar para cada variable la relacion positiva i negativa respecto cada variable


# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
inerProj<- pca$sdev^2 
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner
pinerEix


#VARIANCES - sedimentacion
  #This plot aims to decide whats it's the nd(significative dimension to plot 
  #individuals)
  #Cummulated Inertia in subspaces, from first principal component to the 4th dimension subspace
barplot(100*cumsum(pca$sdev[1:dim(data_pca)[2]]^2)/dim(data_pca)[2])
  #Del grafico podemos ver como las dos primeras dimensiones contienen el 80% de la informacion
  #si queremos el 50% de la info cojemos 1 dimension
  #si queremos el 80% de la info cojemos 2 dimensiones 
  #si queremos el 100% de la info cojemos 3,4,5 dimensiones
  
#PLOT OF INDIVIDUALS
nd = 5
data_pca[1,]

#eigen values
lbd = pca$sdev[1:nd]^2
U = pca$rotation[,1:nd]
Psi = pca$x[,1:nd]
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



