
# The data is 70.19999695	71.09999847
#71.69999695	72.30000305	73.09999847	
#72.90000153	74.40000153	75.40000153	
#76	76.90000153	77.40000153	78.19999695	
#78.90000153	78.69999695	79	78	80	
#79.80000305	80.30000305	80.5	80.69999695	
#81.09999847	81.5	81.90000153	82.30000305	
#82.69999695	83.19999695	83.5

# Getting the data
singapore = scan()

singapore = ts(singapore,start=1980)

plot(singapore,ylab="Labour Force Paricipation Rate 25-54")

# Holt linear trend model
library(forecast)

holttrend = holt(singapore,h = 5)
summary(holttrend)
plot(holttrend)

# With a damping parameter

holtdamp = holt(singapore,h=15,damped=T)
plot(holtdamp)
summary(holtdamp)

holtdamp2 = holt(singapore,h=15,damped=T,phi = 0.8)
plot(holtdamp2)
