
# The data is 70.19999695	71.09999847
# 71.69999695	72.30000305	73.09999847	
# 72.90000153	74.40000153	75.40000153	
# 76	76.90000153	77.40000153	78.19999695	
# 78.90000153	78.69999695	79	78	80	
# 79.80000305	80.30000305	80.5	80.69999695	
# 81.09999847	81.5	81.90000153	82.30000305	
# 82.69999695	83.19999695	83.5

# Getting the data
singapore = scan()

singapore = ts(singapore,start=1980)

plot(singapore,ylab="Labour Force Paricipation Rate 25-54")

# Holt linear trend model
library(forecast)

holttrend = holt(singapore,h = 10)
summary(holttrend)
plot(holttrend)

# With a damping parameter

holtdamp = holt(singapore,h=10,damped=T)
plot(holtdamp)
summary(holtdamp)

holtdamp2 = holt(singapore,h=10,damped=T,phi = 0.8)
plot(holtdamp2)

singarima = auto.arima(singapore)
summary(singarima)

plot(forecast(singarima,h=5))

# Exact calculation of Arima parameters
auto.arima(singapore,approximation = F, stepwise = F)

arimaforecast = forecast(singarima,h=10)

# Visualization

autoplot(singapore) +
  forecast::autolayer(holttrend$mean,
                      series='Holt Linear Trend') +
  forecast::autolayer(holtdamp$mean,
                      series='Holt Damped Trend') +
  forecast::autolayer(arimaforecast$mean,
                      series='ARIMA') +
  xlab('year') + ylab('Labour Force 
                      Participation Rate Age 25-54')+
  guides(colour=guide_legend(title='Forecast Method')) +
  theme(legend.position = c(0.8,0.2)) +
  
  ggtitle('Singapore') + 
  theme(plot.title = element_text(family='Times',
                                  hjust = 0.5,color='blue',
                                  face='bold',size=15))

# In sample plot

autoplot(singapore,size=2) +
  forecast::autolayer(holttrend$fitted,
                      series='Holt Linear Trend',size=1) +
  forecast::autolayer(holtdamp$fitted,
                      series='Holt Damped Trend',size=1) +
  forecast::autolayer(arimaforecast$fitted,
                      series='ARIMA',size=1) +
  xlab('year') + ylab('Labour Force 
                      Participation Rate Age 25-54')+
  guides(colour=guide_legend(title='Forecast Method')) +
  theme(legend.position = c(0.8,0.2)) +
  
  ggtitle('Singapore') + 
  theme(plot.title = element_text(family='Times',
                                  hjust = 0.5,color='blue',
                                  face='bold',size=15))
