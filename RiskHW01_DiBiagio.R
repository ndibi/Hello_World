#Risk: HW01 - Nicholai DiBiagio  

# Clear workspace
rm(list=ls())

#install packages
install.packages('devtools')
install.packages(c('zoo','Quandl'))

# Load the Quandl and TTR packages (you might need to install)
library(zoo)
library(Quandl)
library(TTR)

# call a script to load my API key
Quandl.api_key("sKYQdCFxDsBeDL6rA9xx")

#download commodity data:

#energy futurres: crude oil(cl1), brent(b1), heating oil(o4), nat gas(ng1), gasoline(rb1)
cl1 <- Quandl("CHRIS/CME_CL1",type="zoo",start_date="1990-01-01", end_date="2019-12-31")
b1 <- Quandl("CHRIS/ICE_B1" ,type="zoo",start_date="1990-01-01", end_date="2019-12-31")
o4 <- Quandl("CHRIS/ICE_O4",type="zoo",start_date="1990-01-01", end_date="2019-12-31")
ng1 <- Quandl("CHRIS/CME_NG13" ,type="zoo",start_date="1990-01-01", end_date="2019-12-31")
rb1 <- Quandl("CHRIS/CME_RB1" ,type="zoo",start_date="1990-01-01", end_date="2019-12-31")

#treasury 10 Yr Note(ty1)
ty1 <- Quandl("CHRIS/CME_TY1" ,type="zoo",start_date="1990-01-01", end_date="2019-12-31")

#other commodity utures: gold(gc1), copper(hg1), palladium(pa1), hot-rolled steel(hr10), Coffee(kc2)
gc1 <- Quandl("CHRIS/CME_GC1",type="zoo",start_date="1990-01-01", end_date="2019-12-31")
hg1 <- Quandl("CHRIS/CME_HG1",type="zoo",start_date="1990-01-01", end_date="2019-12-31")
pa1 <- Quandl("CHRIS/CME_PA1",type="zoo",start_date="1990-01-01", end_date="2019-12-31")
hr10 <- Quandl("CHRIS/CME_HR10",type="zoo",start_date="1990-01-01", end_date="2019-12-31")
kc2 <- Quandl("CHRIS/ICE_KC2",type="zoo",start_date="1990-01-01", end_date="2019-12-31")

# create a function to convert prices to returns
prices2returns <- function(x) {
  diff(log(x))
}

#create function to make realized vol using runSD function from TTR
RV <- function(x) runSD(x, n=252)*sqrt(252)

# fix missing values for b1 & kc2
b1 <- b1[!is.na(b1$Settle)]
kc2 <- kc2[!is.na(kc2$Settle)]


# time series of returns
r_cl1 <- prices2returns(cl1$Settle)
r_b1 <- prices2returns(b1$Settle)
r_o4 <- prices2returns(o4$Settle)
r_ng1 <- prices2returns(ng1$Settle)
r_rb1 <- prices2returns(rb1$Settle)

r_ty1 <- prices2returns(ty1$Settle)

r_gc1 <- prices2returns(gc1$Settle)
r_hg1 <- prices2returns(hg1$Settle)
r_pa1 <- prices2returns(pa1$Settle)
r_hr10 <- prices2returns(hr10$Settle)
r_kc2 <- prices2returns(kc2$Settle)

# Create realized volatility
rv_cl1 <- RV(r_cl1)
rv_b1 <- RV(r_b1 )
rv_o4 <- RV(r_o4 )
rv_ng1 <- RV(r_ng1 )
rv_rb1 <- RV(r_rb1 )

rv_ty1 <- RV(r_ty1 )

rv_gc1 <- RV(r_gc1 )
rv_hg1 <- RV(r_hg1 )
rv_pa1 <- RV(r_pa1 )
rv_hr10 <- RV(r_hr10 )
rv_kc2 <- RV(r_kc2 )

#plot nat gas(ng1)
plot(ng1$Settle)

#plot ng1 returns and realized vol.
plot.zoo(
  cbind( r_ng1, rv_ng1), 
  plot.type = "m", 
  col = c("red","black"), 
  ylab = c("Returns", "RV"), 
  xlab = "Time", # x axis label
  main = "Natural Gas: Returns vs Realized Vol" 
)

# combine RV series by commodity types into seperate matrix
rv_energy <- cbind( rv_cl1, rv_b1, rv_ng1, rv_o4, rv_rb1)
rv_finance <- (rv_ty1)
rv_other <- cbind(rv_gc1, rv_hg1, rv_hr10, rv_pa1, rv_kc2)

# name the columsn of the RV matrix
names(rv_energy) <- c("WTI", "Brent", 'Henry Hub', 'Heating Oil', 'Gasoline')
names(rv_finance) <- ('10 Yr. T-Note')
names(rv_other) <- (c('Gold', 'Copper', 'Hot-Rolled Steel', 'Palladium', 'Soybeans'))

#plot energy group 
plot.zoo(
  rv_energy, 
  plot.type = "s", 
  col = 1:5, # colors of lines
  ylab = "RV", # y axis label
  xlab = "", # x axis label
  main = "Realized Vol: Energy Commodities" 
)
# add a legend using the names of the series
legend(x="topleft", legend=names(rv_energy), fill=1:5)

#plot finance group
plot.zoo(
  rv_finance, 
  plot.type = "s", 
  col = 1, 
  ylab = "RV", 
  xlab = "", 
  main = "Realized Vol: 10 Yr. T-Note" 
)


#plot others group
plot.zoo(
  rv_other, 
  plot.type = "s", 
  col = 1:5, 
  ylab = "RV", 
  xlab = "", 
  main = "Realized Vol: Other Commodities" 
)
# add a legend using the names of the series
legend(x="topright", legend=names(rv_other), fill=1:5)

#plot one chart with one of each commodity group
rv_combined <- cbind(rv_cl1,rv_kc2,rv_ty1)
names(rv_combined) <- c('WTI', 'Coffee', '10 Yr. T-note')

plot.zoo(
  rv_combined, 
  plot.type = "s", 
  col = 1:3, 
  ylab = "RV", 
  xlab = "", 
  main = "Realized Vol: Combination of groups" 
)
# add a legend using the names of the series
legend(x="topright", legend=names(rv_combined), fill=1:3)
 
