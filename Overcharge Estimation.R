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
OC_cost = 0.4 # E(cost|Cartel==1) - E(cost|Cartel==0)
Pass_on = 0.8 # Share of cost that is passed to price
cost = X[,2]+ OC_cost # cost is not orthogonal to cartel

Base_price = 2 # log_price of the reference unit sold (all other controls at 0)
# Will be cleared by regressing on the constant. Is just there for realism.
# Can be retrieved by constant coef if all other controls are centered.

price = Base_price + OC*cartel  + Pass_on*cost + eta

reg1.1 = lm(price~cartel)
reg1.1

reg1.2 = lm(price~cartel + cost)
reg1.2

plot(price, ylim = c(0,5), pch = 3, col = "red",cex = 0.6 )
lines(cost)
plot.new()

