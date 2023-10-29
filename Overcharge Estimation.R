library(MASS)  #mvnorm
library(stats) #summarise regressions with asymptotic sd estimators (non heteroscedastic robust, use sandwich library for that)
library(matrixcalc)  # Used for matrix inverse
library (matlib)
library(rgl)#usedfor 3d representation
source("C:/Users/tayoy/Documents/GitHub/OLS-Illustrations/Projections.R")


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
cartel = rep(0, 10000)
cartel[1:5000] = 1
OC = 0.3 #Overcharge
OC_cost = 0.4 #Cost unusual delta on a small subset
Pass_on = 0.2 #cost pass on rate
cost_ = rep(1, 10000)
cost_[6000: 8500] = 1 + OC_cost
#price generation
price = Base_price + OC * cartel + Pass_on * cost_+eta
reg2.1 = lm(price~cartel)
reg2.1

reg2.2 = lm(price~cartel + cost_)
reg2.2

plot(price, type = "l", lwd = 3, col = "brown1", ylim = c(0,2.5), xlab = "Time",
     ylab ="log(value)")
lines(cost_, lwd = 3, col  = "cornflowerblue")
abline(v = 5000, lwd = 2, lty = 3, col = "lightgrey")
legend("topleft", legend=c("Price", "Cost"),
       col=c("brown1", "cornflowerblue"), cex=1.2,lty = c(1,1),lwd = c(2,2),
       box.lty=1, box.lwd=2)
textcartel = rbind(c(3500, 0.5), c(7000, 0.5))
rownames(textcartel) = c("Cartel","No cartel")
text(textcartel, rownames(textcartel), cex = 1.3)



# Graph of the above regression
# Base of the set of controls
e1 = rep(0, 10000)
e1[c(-1, -10000)]=1
e2 = rep(0, 10000)
e2[6000: 8500] = 1
e3 = cartel
e3[1] = 0 


plot(price, type = "l", lwd = 2, col = "brown1", ylim = c(0,2.5), xlab = "Time", ylab = "")
lines(e1, lwd = 3, col = "aquamarine3")
lines(e2, lwd = 3, col  = "cornflowerblue")
lines(e3, lwd = 2, col  = "black", lty = 2262)
abline(v = 5000, lwd = 2, lty = 3, col  = "lightgrey")
legend("topright", legend=c("Price","e1","e2","e3"),
       col=c("brown1", "aquamarine3","cornflowerblue","black"), cex=1.2,lty = c(1,1,1,2262),lwd = c(2,2,2,2),
       box.lty=1, box.lwd=2, ncol = 2)

dev.off()

# 3D representation of controlling for a cost.
angle = acos(cor(cost_,cartel))
basis = rbind(c(-1,0,0), c(0,0,1))
cart = c(0,1,0)

coor2 = cos(angle)
coor1 = - sin(angle)
cst = c(coor1, coor2, 0)
prc = c(0.2*cst + 0.3*cart + 0.3* basis[2,])
prc = prc/norm(prc, type = "2")
prc = matrix(prc, nrow = 1)
rownames(prc)= "Price"


vec = rbind(cart, cst)
rownames(vec) = c("Cartel", "Cost")
close3d()
open3d()
planes3d(0, 0, 1, 0, col="cornflowerblue", alpha=0.4)
vectors3d(basis, col = "black")
vectors3d(vec, col = "blue4", lwd = 2)
vectors3d(prc, col = "red", lwd = 2)
pointfall = cbind(c(1,0,0),c(0,1,0)) %*% coefs(t(prc), cbind(c(1,0,0),c(0,1,0)))
first_proj =  matrix(c(0,1,0), nrow = 3) %*%coefs(t(prc), matrix(c(0,1,0), nrow = 3))

real_cart_coef = coefs(matrix(prc),cbind(cart, cst))[1]

segments3d(t(cbind(pointfall,first_proj)), lwd = 2, color = "gold")
segments3d(t(cbind(pointfall,t(prc))), lwd = 2, col = "red")
segments3d(t(cbind(pointfall,c(0,0,0))), lwd = 2, col = "red")
segments3d(t(cbind(pointfall,real_cart_coef*c(0,1,0))), lwd = 2, col = "springgreen2")
corner(c(0,0,0),as.vector(pointfall),as.vector(prc), col = "red", lwd = 2)
corner(c(0,0,0),as.vector(first_proj),as.vector(pointfall), col = "gold", lwd = 2)
points3d(real_cart_coef*c(0,1,0), size = 20, color= "green3", pch = 2)
points3d(as.vector(first_proj), size = 20, color= "gold2", pch = 5)
text3d(real_cart_coef*c(0,1,0)+c(0.6,0,0.05) , texts="Unbiased coefficient", color= "green4", )
text3d(as.vector(first_proj)+c(0.6,0,0.05) , texts="Biased coefficient", color= "yellow4" )
light3d(0,0)
close3d()

# Included variable bias with a dummy control
# 1) Economist's model

OC_uniform = 0.36 # uniform overcharge of 36% in the economist's model
OC_specificG = 0.6 # overcharge when G=1 in reality
OC_specfifcNoG= 0.3# overcharge when G=0 in reality
P_increase = 0.1 # price increase that G=1 justifies
OC_cost = 0.1 # E(cost|T=1) - E(cost|T=0)
Pass_on = 0.8 #cost pass on rate
Cost_inflationG = 0.1
cartel = rep(0, 10000)
cartel[1:5000] = 1
G = rep(0,10000)
G[3500:4500] =1
cost_ = rep(0, 10000)
cost_[1:5000] =  OC_cost 
cost_[3500:4500] = OC_cost+Cost_inflationG
cost_=cost_+eta #eta is directly attributed to cost for collinearity reasons
Base_price = 1 # average price when T=0

#price generation with uniform overcharge 
price1 = Base_price + OC_uniform * cartel + Pass_on * cost_ + (P_increase)*G 

#why controlling for G is justified
lm(price1~cartel +cost_)
lm(price1~cartel +cost_+G)

plot(price1, type = "l", lwd = 3, col = "brown1", ylim = c(0,2.5), xlab = "Time",
     ylab ="log(value)")
lines(cost_, lwd = 3, col  = "cornflowerblue")
lines(G, lwd = 2, col  = "black", lty = 3)


abline(v = 5000, lwd = 2, lty = 3, col = "lightgrey")
legend("topleft", legend=c("Price", "Cost","Dummy G"),
       col=c("brown1", "cornflowerblue", "black"), cex=1.2,lty = c(1,1,3),lwd = c(2,2,2),
       box.lty=1, box.lwd=2)
textcartel = rbind(c(2000, 0.5), c(8000, 0.5))
rownames(textcartel) = c("Cartel","No cartel")
text(textcartel, rownames(textcartel), cex = 1.3)
dev.off()

# Economist's model fails when overcharge is not uniform (can be explained by G)
price2 = Base_price + OC_specfifcNoG * cartel * (1-G) +OC_specificG * cartel *G + Pass_on * cost_ + P_increase *G
lm(price2~cartel + cost_+G)

plot(price2, type = "l", lwd = 3, col = "brown1", ylim = c(0,2.5), xlab = "Time",
     ylab ="log(value)")
lines(price1, lwd = 0.5, lty = 1, col = "grey70")
lines(cost_, lwd = 3, col  = "cornflowerblue")
lines(G, lwd = 2, col  = "black", lty = 3)
abline(v = 5000, lwd = 2, lty = 3, col = "lightgrey")
legend("topleft", legend=c("Price with specific overcharge when G=1", "Cost","Dummy G","Price with uniform overcharge 0.36"),
       col=c("brown1", "cornflowerblue", "black", "grey70"), cex=0.9,lty = c(1,1,3,1),
       lwd = c(2,2,2), box.lty=1, box.lwd=2)
textcartel = rbind(c(2000, 0.5), c(8000, 0.5))
rownames(textcartel) = c("Cartel","No cartel")
text(textcartel, rownames(textcartel), cex = 1.3)
dev.off()

#Decomposition of the coefficient allocation determinants (see Annex, sect.4.3)
#Different correlations when there is a special overcharge on product with G=1
PxtG = lm(G~price+cost)$residuals #projection of G on Vect(1,X,T) orthogonal
eta0 = lm(price2~cartel+cost)$residuals #residuals in reality
etaEconomist = lm(price1~cartel+cost)$residuals #residuals if uniform overcharge
cor(eta0, PxtG) #G is more efficient in reality
cor(etaEconomist,PxtG) # Than in the expected case of uniform overcharge

# What if G=1 is not included in T=1
G[6000:10000] = 1
cost_ = rep(0, 10000)
cost_[cartel==1] =  OC_cost 
cost_[G==1&cartel==1] = Cost_inflationG + OC_cost
cost_[G==1&cartel==0] = Cost_inflationG
cost_=cost_ #eta is directly attributed to cost for collinearity reasons 
price3 = Base_price + OC_specfifcNoG * cartel * (1-G) +OC_specificG * cartel *G + Pass_on * cost_ + P_increase *G
lm(price3~cartel + cost_+G)


plot(price3, type = "l", lwd = 3, col = "brown1", ylim = c(0,2.5), xlab = "Time",
     ylab ="log(value)")
lines(cost_, lwd = 3, col  = "cornflowerblue")
lines(G, lwd = 2, col  = "black", lty = 3)


abline(v = 5000, lwd = 2, lty = 3, col = "lightgrey")
legend("topleft", legend=c("Price", "Cost","Dummy G"),
       col=c("brown1", "cornflowerblue", "black"), cex=1.2,lty = c(1,1,3),lwd = c(2,2,2),
       box.lty=1, box.lwd=2)
textcartel = rbind(c(2000, 0.5), c(8000, 0.5))
rownames(textcartel) = c("Cartel","No cartel")
text(textcartel, rownames(textcartel), cex = 1.3)
