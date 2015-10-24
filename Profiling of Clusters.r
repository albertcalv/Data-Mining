setwd("/home/albert")
dd<-read.csv("NBA.csv",header=T, sep=",")

attach(dd)
names(dd)
summary(dd)

#Feature Selection ???
team <- as.factor(team)
levels(team) <- c(NA, "positiu","negatiu")

var_num <- data.frame(away_score, home_score, converted_x, converted_y)

oneway.test(away_score~team)
o<-oneway.test(away_score~team)
attributes(o)
o$p.value


#
# PROFILE OF [FEATURE SELECTION]
#
#P-TEST
#profiling test: Permit to understand if Positive _________ is associated with 
#with significant ____________ or significant ___________
#use test value
#decision rule: P-value >=0.975 means positive ________ associated
#with ____________ or p-value<=0.025 means __________ associated with
#____________


par(ask=TRUE)
ncon <- length(var_num)

for (i in 1:ncon) {
  barplot(tapply(var_num[[i]], team, mean),main=paste("Means of", names(var_num)[i], "by teams" ))
  abline(h=mean(var_num[[i]]))
  legend(0,mean(var_num[[i]]),"global mean",bty="n") }



# Test the influence of the variable wrt the type of Dictamen 
# PVALUE OF THE HYPOTHESIS TEST COMPARING THE MEAN OF THE GROUP WITH THE GLOBAL MEAN
# WE DETECT POSITIVE DEVIATIONS ONLY

p.xk <- function(vec,fac){
  #freq dis of fac
  nk <- as.vector(table(fac)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(vec,fac,mean);
  #valors test
  txk <- (xk-mean(vec))/(sd(vec)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F)
}
#
nresp <- length(levels(team))

pvalk.con <- matrix(NA,nresp,ncon)
rownames(pvalk.con) <- levels(team)
colnames(pvalk.con) <- names(var_num)


for (i in 1:ncon) {
  pvalk.con[,i] <- p.xk(var_num[[i]],team) }

for (k in 1:nresp) { print(paste("P.values of Team:",levels(team)[k])); print(sort(pvalk.con[k,])) }

for (k in 1:nresp) { print(paste("P.values of Team:",levels(team)[k])); print(sort(pvalk.con[k,]), digits=3) }

########################################


#
# GRAPHICAL REPRESENTATION Dictamen * CATEGORICAL VARIABLES
#period, team, event_type, assist, block, player, result

varcat <- data.frame(period, team, event_type, assist, block, player, result)


# FUNCTION 
# PVALUE OF THE HYPOTHESIS TEST COMPARING THE PROPORTION OF THE GROUP WITHIN ONE MODALITY WITH THE GLOBAL PROPORTION OF THE GROUP
#
p.zkj <- function(resp,expl){
  taula <- table(resp,expl);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=T);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  zkj <- dpf/dvt; 
  pzkj <- pnorm(zkj,lower.tail=F);
  list(rowpf=pf,vtest=zkj,pval=pzkj)
}

#par(mfrow=c(1,3))

par(ask=TRUE)

n <- nrow(dd)
ncat <- length(varcat)

for (i in 1:ncat) {
  rowprof <- p.zkj(team,varcat[[i]])$rowpf
  marg <- table(varcat[[i]])/n
  print(append("Categories=",levels(varcat[[i]])))
  plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(varcat)[i]))
  lines(rowprof[1,],col="blue")
  lines(rowprof[2,],col="red")
  legend("topright",c("pos","neg"),col=c("blue","red"),lty=1,cex=0.6) 
}


#Hipothesis Test
pvalk.cat = NULL

for (i in 1:ncat) {	
  auxpvalk <- p.zkj(team,varcat[[i]])$pval 
  pvalk.cat = cbind(pvalk.cat,auxpvalk) }

for (k in 1:nresp) { print(paste("P.values of Team:",levels(team)[k])); print(sort(pvalk.cat[k,])) }


pval<-NULL
pval<-cbind(pvalk.con, pvalk.cat)
pval

for (k in 1:nresp) { print(paste("P.values of Team:",levels(team)[k]));
  print(sort(pval[k,]))
}