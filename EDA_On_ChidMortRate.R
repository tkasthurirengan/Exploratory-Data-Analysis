setwd("F:/BGSU/Assignments/EDA")
dia <-read.csv("Child-Mort.csv",header = T)
attach(dia)
length(IncomeType)
library(aplpack)
stem.leaf(dia$Child,unit=10,m=2)

boxplot(Child~IncomeType,main = "Comparing Box Plots by Mort Rate",xlab="Income Type",ylab="Mortality Rate")
Agg <-aggregate(Child,list(group=IncomeType),fivenum)
dia_fivenum <- Agg$x
dia_fivenum
STEP<- 1.5*(dia_fivenum[,4]-dia_fivenum[,2])
Lower_inner_fence<-dia_fivenum[,2]-STEP 
Upper_inner_fence<-dia_fivenum[,4]+STEP 
Lower_outer_fence<-dia_fivenum[,2]-2*STEP 
Upper_outer_fence<-dia_fivenum[,4]+2*STEP 
Lower_inner_fence
Upper_inner_fence


median_median_line <- function(x, y, data)
{
  if(!missing(data))
  {
    x <- eval(substitute(x), data) 
    y <- eval(substitute(y), data) 
  }
  
  stopifnot(length(x) == length(y))
  
  #Step 1
  one_third_length <- floor(length(x) / 3)
  groups <- rep(1:3, times = switch((length(x) %% 3) + 1, one_third_length,
                                    c(one_third_length, one_third_length + 1, one_third_length),
                                    c(one_third_length + 1, one_third_length, one_third_length + 1)
  ))
  
  #Step 2
  
  y <- y[order(x)]
  
  x <- sort(x)
  
  groups<-sort(groups)
  
  #Step 3
  median_x <- tapply(x, groups, median)                                 
  median_y <- tapply(y, groups, median)
  
  #Step 4
  slope <- (median_y[3] - median_y[1]) / (median_x[3] - median_x[1])
  intercept <- median_y[1] - slope * median_x[1]
  
  #Step 5
  middle_prediction <- intercept + slope * median_x[2]
  intercept <- intercept + (median_y[2] - middle_prediction) / 3
  c(intercept = unname(intercept), slope = unname(slope))
  
}

logmed<-log10(dia_fivenum[,3])
logdf<-log10(dia_fivenum[,4]-dia_fivenum[,2]) 
par(mfrow=c(1,1))
plot(logmed, logdf, main="Spread and Level Plot for Raw Data")
rlin<-median_median_line(logmed,logdf)
rlin


# After finding intercept and slope, we need to fit the line in data

Isl_linefit <- rlin[2]*logmed+rlin[1]
plot(logmed, logdf, main="Spread and Level Plot Before Transformation")
lines(logmed, Isl_linefit)

boxplot(Child^0.5~IncomeType , main = "Comparing Box PLots By Mortaliy Rate After Transformation")

# Now, we can see the relationship between the spread and the level data using the Power tranformation
# We should not see any relationship between logmed and logdf

tri<-aggregate(Child^(0.5), list(group=IncomeType), fivenum)
tris<-tri$x

#calculating five number summary after the Transformation.

tris
trislogmed<-log10(tris[,3])
trislogdf<-log10(tris[,4]-tris[,2])
plot(trislogmed,trislogdf, main="Spread and Level Plot After Transformation - Mort Rate") 

# Calculating Outlier for the transformed data

STEP<- 1.5*(tris[,4]-tris[,2])
Lower_inner_fence_AT<-tris[,2]-STEP 
Upper_inner_fence_AT<-tris[,4]+STEP 
Lower_outer_fence_AT<-tris[,2]-2*STEP 
Upper_outer_fence_AT<-tris[,4]+2*STEP 
Lower_inner_fence_AT
Upper_inner_fence_AT

# we can compare the spreds now for the raw and tranformed data

rawdf_IS <- dia_fivenum[,4]-dia_fivenum[,2]
rawdf_IS

transdf_IS<- tris[,4]-tris[,2]
transdf_IS

max(rawdf_IS)/min(rawdf_IS)
max(transdf_IS)/min(transdf_IS)

hist(Child,freq = FALSE)
lines(density(Child,bw=1.5),lwd=1.5)
plot(density(Child, bw=1.5), lwd=2, axes=FALSE, xlab="", ylab="", main="Distribution of Child Mortality Rates")  


summary(Child)
# As we can see from the summary that, the median is smaller
#than mean, indicates right- skewness. 

n1 <- length(Child)
n1
u1 <- seq((n1+1-(n1+1)/2),(n1+1-1),by=1)
u1
#Sorting the values of Child in a reverse order. 
u1i <- sort(Child)[rev(u1)]
u1i <- sort(Child)[rev(u1)]-median(Child)
u1i
v1 <- seq(1,(n1+1)/2,by=1)
v1
v1i <- sort(Child)[(v1)]
v1i <- median(Child)-sort(Child)[(v1)]
v1i
plot(v1i,u1i,main = "Symmetry plot for Child")

#Adding the line u=v in graph. 

su<-seq(0,90,1)
lines(su,su,type = "l")

# We can see from the graph that the points lie above the lines and confirms the right skewness. we need to transform this data now. 

roots <-sqrt(Child)
stem.leaf(roots)

u1i<-sort(roots)[rev(u1)] 
u1 <- seq((n1+1-(n1+1)/2),(n1+1-1),by=1)
u1i<-sort(roots)[rev(u1)]-median(roots)
v1 <- seq(1,(n1+1)/2,by=1)
v1i<-median(roots)-sort(roots)[v1]
u1i
v1i
plot(v1i,u1i, main="Symmetry Plot for Root - Child")
su<-seq(0,8,0.1)
lines(su,su)
summary(roots)

# PLOTTING

library(car)
scatterplot(Child~Year, xlab="Year", ylab="Child",smoother=FALSE, boxplots=FALSE) 
lm(Child~Year)
Fit <- -2.567*(Year)+5201.646
Residual <- Child-Fit
plot(Year, Residual, xlab="Year", ylab="Child")
abline(h=0, lwd=2) 
scatterplot(Child~Year, xlab="Year", ylab="Child",boxplots=FALSE)  
plot(Year,Child, xlab = "Year",ylab = "Child")
lines(lowess(Year,Child, f=0.5)) 
# This means there is a decrease. 
data <- data.frame(Year,Child) 
sorted.data <- data[order(Year), ] 
sorted.data
plot(Year,Child, xlab = "Year",ylab = "Child")
abline(v=1979)
abline(v=1996)
s.points <- data.frame(x=c(1967,1987,2005), y=c(168.7,94.5,62.8)) 
points(s.points, cex=2, pch=19, col="red")
bo<- (62.8-94.5)/(2005-1967)
bo
ao<-1/3*((168.7-bo*(1967-1987))+94.5+(62.8-bo*(2005-1987))) 
ao
#So the three group line is Y=-0.83(x-1987)+108.1105
plot(Year,Child, main="Resistant Line")
curve(-0.834 * (x - 1987) + 108.1105, add=TRUE, col="blue") 

plot(Year,Child)
su.points <- data.frame(x=c(1967,1987,2005), y=c(168.7,94.5,62.8)) 
points(su.points, cex=2, pch=19, col="red")
abline(lm(y~x, data=su.points[1:2,]), col="blue")
abline(lm(y~x, data=su.points[2:3,]), col="blue") 

# Straightening Work Function 
straightening.work<-function(sp, px, py) 
{ 
  sp$tx<-(sp$x^px-1)/px 
  sp$ty<-(sp$y^py-1)/py 
  sp$slope[1]<-diff(sp$ty[1:2])/diff(sp$tx[1:2]) 
  5  
  sp$slope[2]<-diff(sp$ty[2:3])/diff(sp$tx[2:3]) 
  sp$half.slope.ratio<-sp$slope[2]/sp$slope[1] 
  sp$slope[3]<-NA 
  sp$half.slope.ratio[2:3]<-NA 
  row.names(sp)<-c("Left", "Center", "Right") 
  sp 
} 
straightening.work(su.points, 1, 1) 
straightening.work(su.points, 0.5, 1)
straightening.work(su.points, 0.001, 0.001)
straightening.work(su.points, -0.33, -0.5) 
new.x<-Year^(-0.33)
new.y<--0.5*Child
plot(new.x,new.y)

mmline <- function(x, y, data)
{
  if(!missing(data)) 
  {
    x <- eval(substitute(x), data)           
    y <- eval(substitute(y), data)
  }
  stopifnot(length(x) == length(y))
  #Step 1
  one_third_length <- floor(length(x) / 3)
  groups <- rep(1:3, times = switch((length(x) %% 3) + 1, one_third_length,
                                    c(one_third_length, one_third_length + 1, one_third_length),
                                    c(one_third_length + 1, one_third_length, one_third_length + 1)
  ))
  #Step 2            
  data<-data.frame(x,y)      
  data<-data[order(data$x),]      
  x<-data$x      
  y<-data$y
  
  #Step 3      
  groups<-sort(groups)  
  median_x <- tapply(x, groups, median)
  median_y <- tapply(y, groups, median) 
  #Step 4      
  slope <- (median_y[3] - median_y[1]) / (median_x[3] - median_x[1]) 
  intercept <- 1/3*(median_y[1] - slope *(median_x[1]-median_x[2])+median_y[2]+median_y[3] - slope *(median_x[3]-median_x[2]))
  d<-data.frame(median_x, median_y, slope, intercept)      
  d$slope[2-3]<-NA      
  d$intercept[2-3]<-NA     
  return(d) 
}

mfit<-mmline(new.x,new.y)
mfit
FIT1<-mfit$intercept+mfit$slope*(new.x-mfit$median_x[2]) 
RESIDUAL<-new.y-FIT1 
plot(new.x, RESIDUAL)
abline(h=0)
 
