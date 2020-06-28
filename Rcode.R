# Import libraries
library(readr)

# Read data from data file (geese.txt) and store in R in a data frame (geese)
geese <- read_tsv("geese.txt")
# View dataframe
View(geese)

# Stepwise regression

# Minimal model
formL <- formula(~1)
# Maximum model
formU <- formula(~ALTITUDE*LATERAL, family = binomial, data=geese)
start.model <- glm(RESPONSE~ALTITUDE*LATERAL, family = binomial, data=geese)
step.model <- step(start.model, direction = "backward", scope=list(lower=formL, upper=formU))

# Output ANOVA of final stepwise model
anova(step.model, test="Chisq")

# Calculate p-value of residual deviance 
pchisq(df=460, q=342, lower.tail=F)

# Output summary of final stepwise model
summary(step.model)

# Constructing a probability matrix

# create altitude: an object which stores a list of five of the following integers consecutively: 3, 6, 9, 12
ALTITUDE<-c(rep(3,5), rep(6,5), rep(9,5), rep(12,5))
# create lateral: an object which stores a list of five lots of integers 0, 10, 20 , 30, 40  
LATERAL<-rep(c(0,10,20,30,40),4)
# pair lateral and altitude values in dataframe newx
newx<-data.frame(LATERAL, ALTITUDE)
# predict the probability of geese flying high for each pair of lateral and altitude values in newx
preds<-predict(step.model,new =newx,type="response")
# construct a probability matrix and label the columns and rows
prob<-matrix(preds, ncol=4, byrow=F)
colnames(prob)<-c("A3", "A6","A9", "A12")
rownames(prob)<-c("L0","L10","L20","L30","L40")
prob

# Create a scatter plot of the observed helicopter altitudes and lateral distances
plot(geese$LATERAL, geese$ALTITUDE, main = "Scatter Plot of Altitude and Lateral Distance Values", xlab = "Lateral distance (100m)", ylab = "Altitude (100m)", 
     ylim=c(0,12), xlim=c(0,80), col = "blue", pch=8)
par(new=TRUE)
# Overlay the altitude and lateral distance pairs of values used in the probability matrix
plot(newx$LATERAL, newx$ALTITUDE, xlab = "Lateral distance (100m)", ylab = "Altitude (100m)", ylim=c(0,12), xlim=c(0,80), col = "red", pch = 16)
# Produce a legend in the bottom right corner
legend(x=50, y=3, legend=c('Observed', 'Prediction Values'),
       col=c('blue', 'red'), pch=c(8,16))