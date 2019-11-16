# Let x4 and x8 be the only two important features; x1, x5 and x14 have multi-collearity with x8; 
# all other features are just noise.
# all 15 features are standard normal distribution

x2 = rnorm(2000)
x3 = rnorm(2000)
x4 = rnorm(2000)
x6 = rnorm(2000)
x7 = rnorm(2000)
x8 = rnorm(2000)
x9 = rnorm(2000)
x10 = rnorm(2000)
x11 = rnorm(2000)
x12 = rnorm(2000)
x13 = rnorm(2000)
x15 = rnorm(2000)
x1=0.95*x8+0.05*rnorm(2000) # this is a noisier version of x8
x5=0.9*x8+0.1*rnorm(2000) # this is a noisier version of x8
x14=0.85*x8+0.15*rnorm(2000) # this is a noisier version of x8

##########
# Generating training data: true pattern has 2 components, a cos(x8) and a quadratic funciton with interaction term of only two variables
color = ifelse((cos(5*x8)+(x4-0.5)^2+(x8+0.5)^2-2*x4*x8  <1) , 2, 3) 
y =ifelse( (cos(5*x8)+(x4-0.5)^2+(x8+0.5)^2-2*x4*x8  < 1) , 0, 1)
sim = data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,y)
plot(x4,x8,col = color) # this is like the answer of the underlying pattern

# add noise so the theoretical best performance is 85% 
noise_ID <- sample(2000, 300) # 15% noise
sim[noise_ID, ]$y <- ifelse(sim[noise_ID, ]$y == 1, 0,1)
plot(sim$x4,sim$x8,col = sim$y +2) # this is the pattern you see in your training dataset on those two features
# write.csv(sim, file="A3_train.csv",row.names=FALSE)

###########
# More visualization of your data
###########

# Correlation plot
install.packages("corrplot")
library(corrplot)
# First calculate corrleation coefficients to be visualised
cor_matrix = cor(sim[ ,c(1:16)], method='pearson',use='complete.obs')
# Now produce the plot
corrplot(cor_matrix, method='circle', type='upper', addCoef.col = "black")

# Mass scatter plots
pairs(sim[,c(1,4,8,5,14)]) # typically, this one is a handy tool for visualization
library("psych")
pairs.panels(sim[,c(1,4,8,5,14)]) # typically, this is a modified version of the pairs function
# for more details, please refer to http://www.ucd.ie/ecomodel/Resources/Sheet3_visualisation_WebVersion.html 

# another good package
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(sim[,c(4,8)],col=factor(sim$y)) # this supposedly should be able to show color on the scatter plots when y is a factor



