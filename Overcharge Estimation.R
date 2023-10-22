library(MASS)  #mvnorm
library(stats) #summarise regressions with asymptotic sd estimators (non heteroscedastic robust, use sandwich library for that)
library(matrixcalc)  # Used for matrix inverse
library (matlib)





#Generating some random variable that will help building variables
sigma = matrix(c(1,0.8,0,0.8,1,0.4,0,0.4,1)/4,nrow = 3)

sigma # A Variance matrix such X_3 is not correlated to X1
      # but both X1 and X_3 are correlated to X_2
X=mvrnorm(10000, mu = rep(0,3), Sigma = sigma)

eta = rnorm(10000, 0, 0.01) #some noise, orthogonal to X1,X2,X3.

end_date = 7000 # Date of end of cartel 
                # (to see if results are robust to chages in E(Cartel))

# Coefficient of interest
OC = 0.1 # Overcharge
#The whole internship consists in retrieveing the value of OC
cartel = c(rep(1,end_date), rep(0, 10000-end_date)) # Infringement dummy

# Features  of controls
OC_cost = 0.05 # E(cost|Cartel==1) - E(cost|Cartel==0)
Pass_on = 0.8 # Share of cost that is passed to price
cost = X[,2]+ OC_cost * cartel # cost is not orthogonal to cartel

Base_price = 1.5 # log_price of the reference unit sold (all other controls at 0)
# Will be cleared by regressing on the constant. Is just there for realism.
# Can be retrieved by constant coef if all other controls are centered.

#First regression
price = Base_price + OC*cartel  + Pass_on*cost + eta

reg1.1 = lm(price~cartel)
reg1.1

reg1.2 = lm(price~cartel + cost)
reg1.2

#Regression using categorical variables only
OC = 0.3 #Overcharge
OC_cost = 0.4 #Cost unusual delta on a small subset
Pass_on = 0.2 #cost pass on rate
cost_ = rep(1, 10000)
cost_[7500: 8500] = 1 + OC_cost
#price generation
price = Base_price + OC * cartel + Pass_on * cost_+eta
reg2.1 = lm(price~cartel)
reg2.1

reg2.2 = lm(price~cartel + cost_)
reg2.2
plot(price, type = "l", lwd = 3, col = "brown1", ylim = c(1,2.5), xlab = "Time",
     ylab ="log(value)")
lines(cost_, lwd = 3, col  = "cornflowerblue")
abline(v = 7000, lwd = 2, lty = 2, col = "lightgrey")
legend("topleft", legend=c("Price", "Cost"),
       col=c("brown1", "cornflowerblue"), cex=1.2,lty = c(1,1),lwd = c(2,2),
       box.lty=1, box.lwd=2)
textcartel = rbind(c(5500, 2.2), c(8500, 2.2))
rownames(textcartel) = c("Cartel","No cartel")
text(textcartel, rownames(textcartel), cex = 1.3)


# Graph of the above regression
# Base of the set of controls
e1 = rep(0, 10000)
e1[c(-1, -10000)]=1
e2 = rep(0, 10000)
e2[7500: 8500] = 1
e3 = cartel
e3[1] = 0 


plot(price, type = "l", lwd = 3, col = "brown1", ylim = c(0,2.5), xlab = "Time", ylab ="log(value)")
lines(e1, lwd = 3, col = "aquamarine3")
lines(e2, lwd = 3, col  = "cornflowerblue")
lines(e3, lwd = 3, col  = "black", lty = 2262)
abline(v = 7000, lwd = 2, lty = 3, col  = "lightgrey")
legend("topleft", legend=c("Price","e1","e2","e3"),
       col=c("brown1", "aquamarine3","cornflowerblue","black"), cex=1.2,lty = c(1,1,1,2262),lwd = c(2,2,2,2),
       box.lty=1, box.lwd=2)

dev.off()


