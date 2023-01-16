#importing required libraries
library(readxl)
library(forecast)
library(tseries)
library(backports)
library(urca)
require(tseries)

#ignoring warning
options(warn=1)

#Reading Excel
x<-read_xlsx(file.choose(),col_names = TRUE,sheet = 2)
df=data.frame(x)
attach(df)

#Stationarity Test using ADF,KPSS
multi_stat_tests<- function(df){
  for(i in 1:ncol(df)){
    df_multi=data.frame(var=names(df)[i],box.pvalue=Box.test(ts(df[,i]),lag=20,type="Ljung-Box")$p.value,adf.pvalue=adf.test(ts(df[,i]),alternative = "stationary")$p.value,kpss.pvalue=kpss.test(ts(df[,i]))$p.value,
                        box=Box.test(ts(df[,i]),lag=20,type="Ljung-Box")$p.value<0.05,
                        adf=adf.test(ts(df[,i]),alternative = "stationary")$p.value<0.05,
                        kpss=kpss.test(ts(df[,i]))$p.value<0.05
    )
    return(df_multi)
  }
}

#Calling function to print the results of the test
suppressWarnings(for (i in 1:ncol(df)){
  t[i,]=multi_stat_tests(df[i])
  t
}) 

colnames(df)
forecast.period=5
forecast=function(df){
  for(i in 1:ncol(df)){
    df_forecast=data.frame(var=names(df)[i],model=auto.airma(ts(df[,i])))
  }
  return(df_forecast)
}
suppressWarnings(for (i in 1:ncol(df)){
  q[i,]=forecast(df[,i])
  q
})

#Finding ARIMA parameters for each country 
for (i in 1:ncol(df)){
  m=auto.arima(df[,i])
  m
}

#Fitting ARIMA for each country and Recording each parameter in a dataframe 
l=matrix()
pred=matrix()
train=df[1:24,]
test=df[25:29,]
z=matrix()
y=data.frame()
for (i in 2:ncol(df)){
  model.fit=auto.arima(df[,i])
  l=model.fit$arma
  print(colnames(df[i]))
  print(c(l[1],l[6],l[2]))
  z=data.frame(model.fit$fitted)
  y[,i]=as.data.frame(z)
  print(y[i])
  #print(model.fit)
  #pred=predict(as.vector(model.fit),n.ahead = 5)
  #print(pred$pred)
  #pred
  
}

#predicting for required country
model.fit=auto.arima(df$India,ic = "AICc")
model.fit
pred=forecast(model.fit,h = 5,level = 95)
pred

#plotting the fitted model
plot(df$Peru,type='l')
plot(model.fit)
plot(pred)


#ARIMAX model fitting
install.packages("TSA")
library(TSA)
y<-read_xlsx(file.choose(),col_names = TRUE,sheet = 2)
t=y["ln(M1)"][3:31]
u=y["ln(GDP)"][3:31]
v=y["ln(Consumption)"][3:31]
z=y["ln(CPI)"][3:31]
x=as.matrix(cbind(t,u,v))
A=arima(z,xreg = x,order = c(1,1,0))
A
tail(t)
arima()
x=as.matrix(cbind(t,u,v))
m=auto.arima(z,xreg = x)
m
auto.arima(z,xreg = x,ic = "aicc")
auto.arima(z,ic = "aicc")

#Forecasting for each factor
m1=auto.arima(t)
m1
forecast_m1=forecast(m1,level=95)
forecast_m1
gdp=auto.arima(u)
gdp
forecast_gdp=forecast(gdp,level = 95)
forecast_gdp
consumption=auto.arima(v)
consumption
forecast_consumption=forecast(gdp,level = 95)
forecast_consumption

#Forecasting using ARIMAX model
new=read_xlsx(file.choose(),col_names = TRUE,sheet = 2)
new[31:39,]
t1=new["ln(M1)"][31:39,]
u1=new["ln(GDP)"][31:39,]
v1=new["ln(Consumption)"][31:39,]

new_reg=as.matrix(cbind(t1,u1,v1))
new_reg
forecast=predict(A,newxreg=new_reg)
forecast
arimax=as.matrix(forecast)
write.csv(arimax,"Forecast_Arimax.csv")
summary(A)


india=read_xlsx(file.choose(),col_names = TRUE,sheet = 2)
india
india1=log(india$India,base = exp(1))
india1
fit=auto.arima(india1)
fit
fit$fitted
forec=forecast(fit,h=10)
forec
as.matrix(fit$fitted)
A$fitted
?auto.arima

#Analysis of residuals
checkresiduals(fit)
checkresiduals(A)
tsdiag(A)

#Plotting forecasts
autoplot(forecast(fit))
autoplot(forec)
autoplot(forecast(A,xreg=x))

#ACF of residuals
plot(acf(A$residuals))

#Ljung Box test for residuals
Box.test(A$residuals,fitdf =1 ,lag = 9)

#Correlation plots
Ccf(A$residuals,gdp$residuals)
Ccf(A$residuals,consumption$residuals)
Ccf(A$residuals,m1$residuals)
ccf_value=Ccf(z,v)
ccf_value
install.packages("GGally")
library(GGally)
plot.g=new[1:29,6:9] %>% as.data.frame() %>% ggpairs(xlab="",title = "Correlation Plot") 
summary(A)

#Checking Significance of Parameters
(1-pnorm(abs(m$coef)/sqrt(diag(m$var.coef))))^2
install.packages("lmtest")
library(lmtest)
coeftest(A,df=17)


CPI=read_xlsx(file.choose(),col_names = TRUE)
GDP=read_xlsx(file.choose(),col_names = TRUE)
Population=read_xlsx(file.choose(),col_names = TRUE)
CPI=as.data.frame(CPI)
GDP=as.data.frame(GDP)
Population=as.data.frame(Population)
install.packages("devtools")
install.packages("RCurl")
install.packages("httr")
library(devtools)
library(RCurl)
library(httr)
set_config(config(ssl_verifypeer = 0L))
devtools::install_github("RcppCore/Rcpp")
devtools::install_github("thomasp85/gganimate", force = TRUE)
library(reshape)
install.packages("gapminder")
library(gapminder)
install.packages("dplyr")
library(dplyr)
library(ggplot2)

# Rename the first column as "Country"
colnames(Population)[1] <- "Country"
colnames(GDP)[1] <- "Country"
colnames(CPI)[1] <- "Country"

# Use reshape library to move the year dimension as a column
GDP_m = melt(GDP, id.vars = c("Country")) 
CPI_m = melt(CPI, id.vars = c("Country"))
Population_m = melt(Population, id.vars = c("Country"))

# Give a different name to each KPI (e.g. pop, life, fert)
colnames(Population_m)[3] <- "pop"
colnames(GDP_m)[3] <- "GDP"
colnames(CPI_m)[3] <- "CPI"

# Merge the 3 data frames into one
mydf=merge(GDP_m,CPI_m,by = c("Country","variable"),header = T)
mydf=merge(mydf,Population_m, by=c("Country","variable"),header=T)

# The only piece of the puzzle missing is the continent name for each country for the color - use gapminder library to bring it
continent = gapminder %>% group_by(continent, country) %>% distinct(country, continent)
continent = data.frame(lapply(continent, as.character), stringsAsFactors=FALSE)
colnames(continent)[1] = "Country"

# Filter out all countries that do not exist in the continent table
mydf_filter <- mydf %>% filter(Country %in% unique(continent$Country))

#Add the continent column to finalize the data set
mydf_filter <- merge(mydf_filter, continent, by=c("Country"), header =T)

# Do some extra cleaning (e.g. remove N/A lines, remove factors, and convert KPIs into numerical values)
mydf_filter[is.na(mydf_filter)] <- 0
mydf_filter <- data.frame(lapply(mydf_filter, as.character), stringsAsFactors=FALSE)
mydf_filter$variable <- as.integer(as.character(gsub("X","",mydf_filter$variable)))
colnames(mydf_filter)[colnames(mydf_filter)=="variable"] <- "year"
mydf_filter$pop <- as.numeric(as.character(mydf_filter$pop))
mydf_filter$GDP <- as.numeric(as.character(mydf_filter$GDP))
mydf_filter$CPI <- as.numeric(as.character(mydf_filter$CPI))

# Load libraries
library(ggplot2)
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)
install.packages("viridis")
library(viridis)
install.packages("hrbrthemes")
library(hrbrthemes)

# Add a global theme
theme_set(theme_grey()+ theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6),panel.background = element_rect(fill = NA),panel.grid.major = element_line(colour = "grey50")))

# Create the plot with years as frame, limiting y axis from 30 years to 100
p = ggplot(mydf_filter, aes(CPI, GDP, size = pop, color = continent, frame = year)) +
  labs(x="CPI", y = "GDP", caption = "(Based on data from World Bank)", color = 'Continent',size = "Population") + 
  geom_point(alpha=0.7,shape = 19,show.legend = TRUE,) +
  scale_size(range = c(1.4,19),name = "Population")+
  xlim(0,500)+
  theme_ipsum() +
  scale_color_brewer(type = 'div', palette = 'Spectral') + 
  # gganimate code
  ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()
pp=ggplotly(p,tooltip = "text")
# animate
animate(p, width = 950, height = 450)
# save as a GIF
anim_save("output.gif")

# Load libraries
install.packages("plotly")
library(plotly)
library(ggplot2)
# Create the plot
p <- ggplot(mydf_filter, aes(fert, life, size = pop, color = continent, frame = year)) +
  geom_point()+ ylim(30,100)  + labs(x="Fertility Rate", y = "Life expectancy at birth (years)", color = 'Continent',size = "Population (millions)") + 
  scale_color_brewer(type = 'div', palette = 'Spectral')
# Generate the Visual and a HTML output
ggp <- ggplotly(p, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
ggp
htmlwidgets::saveWidget(ggp, "index.html")

write.csv(GDP_m,"Selected_GDP.csv")
write.csv(CPI_m,"Selected_CPI.csv")
write.csv(Population_m,"Selected_pop.csv")




#ARIMAX using ARIMA() 
A
checkresiduals(A)

#Cointegration Test
df=read_xlsx(file.choose(),sheet=1,col_names = T)
attach(df)
adf.test(df$Unemp)
diff=diff(df$Unemp,differences = 1)
adf.test(diff)
coint=data.frame(df$Unemp,df$Inflation)
VARselect(coint,lag.max = 10,type = "const")
VARselect(coint,lag.max = 10,type = "trend")
VARselect(coint,lag.max = 10,type = "both")
VARselect(coint,lag.max = 10,type = "none")

cointest=ca.jo(coint,K=7,type = "eigen",ecdet = "trend",spec = "transitory")
cointest
cointest@teststat[2]
cointest@teststat[1]
cointest@cval
#r=2
cointest=ca.jo(coint,K=7,type = "trace",ecdet = "trend",spec = "transitory")
cointest
cointest@cval
summary(cointest)



ln_un=log(df$Unemployment,base = exp(1))
adf.test(ln_un)
diff=diff(ln_un,differences = 2)
adf.test(diff)
auto.arima(ln_un,ic="aicc")
coint=data.frame(df$`ln(CPI)`,df$`ln(Unemp)`)
cointegration=ca.jo(coint,type = "trace",ecdet = "none",spec = "longrun")
summary(cointegration)



library(urca)
install.packages("vars")
library(vars)
VARselect(coint,lag.max = 10,type = "const")
VARselect(coint,lag.max = 10,type = "trend")
VARselect(coint,lag.max = 10,type = "both")
VARselect(coint,lag.max = 10,type = "none")

cointest=ca.jo(coint,K=7,type = "eigen",ecdet = "trend",spec = "transitory")
cointest
cointest@teststat[2]
cointest@teststat[1]
cointest@cval
#r=2
cointest=ca.jo(coint,K=7,type = "trace",ecdet = "trend",spec = "transitory")
cointest
cointest@cval









df
coint1=data.frame(df$`ln(CPI)`,df$`ln(Unemp)`)
VARselect(coint1,lag.max = 10,type = "none")
cointegrationtest=ca.jo(coint1,type = "eigen",ecdet = "none",spec = "transitory",K =9 )
summary(cointegrationtest)
s=df$`ln(CPI)`+6.487816*df$`ln(Unemp)`
kpss.test(s)
adf.test(s)
diff_lncpi=diff(df$`ln(CPI)`,differences=1)
diff_lnunemp=diff(df$`ln(Unemp)`,differences=1)
coint1=data.frame(diff_lncpi,diff_lnunemp)
VARselect(coint1,lag.max = 10,type = "trend")
cointegrationtest=ca.jo(coint1,type = "eigen",ecdet = "trend",spec = "longrun",K =8 )
summary(cointegrationtest)
s=diff_lncpi-0.8316911*diff_lnunemp
plot(s,type="l")
kpss.test(s)
adf.test(s)
predict(s)

#VAR Model
library(urca)
library(vars)
library(xlsxjars)
library(readxl)
data=read_xlsx(file.choose(),col_names = TRUE)
plot(data$`ln(Unemp)`)
diff_m1=diff(data$`ln(M1)`,differences=1)     
diff_GDP=diff(data$`ln(GDP)`,differences=1)
xx=exp(data$`ln(GDP)`)
auto.arima(xx)
yy=exp(data$`ln(Consumption)`)
auto.arima(yy)
diff_cons=diff(data$`ln(Consumption)`,differences=1)     
diff_unemp=diff(data$`ln(Unemp)`,differences=1)     
diff_cpi=diff(data$`ln(CPI)`,differences=1)     
plot(diff_unemp,type="l")
library(forecast)
library(tseries)
library(TSA)
adf.test(diff_unemp)
kpss.test(diff_unemp)
pp.test(diff_unemp)
var_data=cbind(diff_cpi,diff_m1,diff_cons,diff_GDP,diff_unemp)
VARselect(var_data,lag.max=10,type="const")
fit=VAR(var_data,p = 4)
fit
pred=predict(fit,n.ahead = 5)
pred
fitted(fit)
write.csv(fitted(fit),"FitVAR.csv")

#Cointegration

library(urca)
library(vars)
library(readxl)
data=read_xlsx(file.choose(),col_names = TRUE)
coint=cbind(data$`ln(CPI)`,data$`ln(Unemp)`)
cointegration=ca.jo(coint,type=c("trace"),ecdet=c("none"),spec=c("transitory"))