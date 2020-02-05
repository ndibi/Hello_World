#HW02 Risk Mgmt - Nicholai DiBiagio

##Q1 - Realzied Vol. by contract tenor:

# Clear workspace
rm(list=ls())

# Load packages
install.packages(c('devtools','Quandl','zoo'))
library(Quandl)
library(zoo)
library(TTR)
library(lubridate)

# Download data: Crude oil & Natural Gas contracts by tenor
Quandl.api_key("sKYQdCFxDsBeDL6rA9xx")

crude1 <- Quandl("CHRIS/CME_CL1" , type="zoo", start_date="1990-01-01", end_date="2019-12-31")
crude3 <- Quandl("CHRIS/CME_CL3" , type="zoo", start_date= "1990-01-01", end_date="2019-12-31")
crude6 <- Quandl("CHRIS/CME_CL6" , type="zoo", start_date= "1990-01-01", end_date="2019-12-31")
crude12 <- Quandl("CHRIS/CME_CL12" , type="zoo", start_date= "1990-01-01", end_date="2019-12-31")
crude24 <- Quandl("CHRIS/CME_CL24" , type="zoo", start_date= "1990-01-01", end_date="2019-12-31")

ng1 <- Quandl("CHRIS/CME_NG1" , type="zoo", start_date="1990-01-01", end_date="2019-12-31")
ng3 <- Quandl("CHRIS/CME_NG3" , type="zoo", start_date="1990-01-01", end_date="2019-12-31")
ng6 <- Quandl("CHRIS/CME_NG6" , type="zoo", start_date="1990-01-01", end_date="2019-12-31")
ng12 <- Quandl("CHRIS/CME_NG12" , type="zoo", start_date="1990-01-01", end_date="2019-12-31")
ng24 <- Quandl("CHRIS/CME_NG24" , type="zoo", start_date="1990-01-01", end_date="2019-12-31")


#Created validate for both cl and ng to check the datasets for missing data.
validate_crude <- c(is.na(crude1$Settle), is.na(crude3$Settle), is.na(crude6$Settle),
                 is.na(crude12$Settle), is.na(crude24$Settle)
)
validate_ng <- c(is.na(ng1$Settle), is.na(ng3$Settle), is.na(ng6$Settle),
                  is.na(ng12$Settle), is.na(ng24$Settle)
)
View(validate_crude[validate_crude == TRUE])
View(validate_ng[validate_ng == TRUE])

#After running the above we see there's no missing data for any of the two datasets


# create a function to convert prices to returns
prices2returns <- function(x) {
  diff(log(x))
}

#Calculate Daily Returns
rcrude1 <- prices2returns(na.omit(crude1$Settle))
rcrude3 <- prices2returns(na.omit(crude3$Settle))
rcrude6 <- prices2returns(na.omit(crude6$Settle))
rcrude12 <- prices2returns(na.omit(crude12$Settle))
rcrude24 <- prices2returns(na.omit(crude24$Settle))

rng1 <- prices2returns(na.omit(ng1$Settle))
rng3 <- prices2returns(na.omit(ng3$Settle))
rng6 <- prices2returns(na.omit(ng6$Settle))
rng12 <- prices2returns(na.omit(ng12$Settle))
rng24 <- prices2returns(na.omit(ng24$Settle))

#Create a dataframe of Std. Dev. for ng and cl 
sdData <- data.frame(
  tenor = c(1,3,6,12,24),
  crude = c( sd(rcrude1), sd(rcrude3), sd(rcrude6), sd(rcrude12), sd(rcrude24)),
  nat_gas = c( sd(rng1), sd(rng3), sd(rng6), sd(rng12), sd(rng24))
)
sdData

#Plot tenor vs. SD for both ng and crude
plot(sdData[,c(1,2)],main= 'Std. Dev. of Commodity Futures by Tenor', ylim=c(0.01,0.035),ylab= 'Std. Dev.', col=1, type="b")
lines(sdData[,c(1,3)] , col=2, type="b")
legend(x="topright", legend=colnames(sdData[2:3]), fill=1:2)

#Compute the realized volatilities on an annual basis for each tenor of each commodity

#Define function to calculate realized vol.
RV <- function(x) runSD(x, n=252)*sqrt(252)

#Calculate realized vol. for each commodity and tenor
RVcrude1 <- RV(rcrude1)
RVcrude3 <- RV(rcrude3)
RVcrude6 <- RV(rcrude6)
RVcrude12 <- RV(rcrude12)
RVcrude24 <- RV(rcrude24)

RVng1 <- RV(rng1)
RVng3 <- RV(rng3)
RVng6 <- RV(rng6)
RVng12 <- RV(rng12)
RVng24 <- RV(rng24)

#Plot the realized vol. for each commodity
RV_crude <- (cbind(RVcrude1, RVcrude3, RVcrude6, RVcrude12, RVcrude24))
RV_ng <- (cbind(RVng1, RVng3, RVng6, RVng12, RVng24))

plot.zoo(RV_crude,main = 'Realized Volatility by Tenor: Crude', xlab = 'Year', ylab = 'RV', plot.type="s", col = 1:5)
legend(x="topright", legend=colnames(RV_crude), fill=1:5)

plot.zoo(RV_ng,main = 'Realized Volatility by Tenor: Nat Gas', xlab = 'Year', ylab = 'RV', plot.type="s", col = 1:5)
legend(x="topright", legend=colnames(RV_ng), fill=1:5)



##Q3 - Rolling Contracts

#Download data as type xts - CL1, CL2, NG1, NG2
crude1_roll <- Quandl("CHRIS/CME_CL1" , type="xts", start_date="1990-01-01", end_date="2019-12-31")
crude2_roll <- Quandl("CHRIS/CME_CL2" , type="xts", start_date="1990-01-01", end_date="2019-12-31")

ng1_roll <- Quandl("CHRIS/CME_NG1" , type="xts", start_date="1990-01-01", end_date="2019-12-31")
ng2_roll <- Quandl("CHRIS/CME_NG2" , type="xts", start_date="1990-01-01", end_date="2019-12-31")

# When does open interest in C2 exceed C1?

dates <- "2019-01/2019-06"

ng_set <- cbind(ng1_roll,ng2_roll)
crude_set <- cbind(crude1_roll,crude2_roll)

ng_set$diff <- with(ng_set, Previous.Day.Open.Interest - Previous.Day.Open.Interest.1)
crude_set$diff <- with(crude_set, Previous.Day.Open.Interest - Previous.Day.Open.Interest.1)

plot(ng_set[dates,"diff"], main="Nat Gas: OpenInt1 - OpenInt2")
abline(h=0, col=2)
plot(crude_set[dates,"diff"], main="Crude: OpenInt1 - OpenInt2")
abline(h=0, col=2)


makeRolledIndex <- function(x1, x2, switchdays, subset=NULL) {
  require(lubridate)
  
  # Some checks on switchdays
  if (length(switchdays)!=2) error('must supply only 2 days -- a roll date and a settle date')
  rollday <- switchdays[1]
  setlday <- switchdays[2]
  if(rollday < 1 | rollday > 28) stop('rollday must be integer between 1 and 28')
  if(setlday < 1 | setlday > 28) stop('setlday must be integer between 1 and 28')
  if(rollday >= setlday) stop('rollday must be less than setlday')
  if(!is.integer(rollday)) warning('forced rollday to int. add L suffix (20L vs 20)')
  if(!is.integer(setlday)) warning('forced setlday to int. add L suffix (20L vs 20)')
  
  # combine contracts 1 and 2
  combined_contracts <- cbind(x1, x2)
  names(combined_contracts) <- c("s1", "s2")
  
  # subset x if subset is not null
  if (!is.null(subset)) combined_contracts <- combined_contracts[subset,]
  
  # add in an indicator for which contract we want and the numeric value of the date
  combined_contracts$use2nd <- ifelse(day(index(combined_contracts)) >= rollday & day(index(combined_contracts)) <= setlday, 1, 0)
  combined_contracts$date <- as.numeric(index(combined_contracts))
  
  # get the dates we roll & settle contracts each month
  rolldates <- as.Date(coredata( apply.monthly( combined_contracts[combined_contracts$use2nd == 1, "date"], min ) ))
  setldates <- as.Date(coredata( apply.monthly( combined_contracts[combined_contracts$use2nd == 1, "date"], max ) ))
  
  # Make indicator showing whether we roll
  combined_contracts$rollToday <- 0
  combined_contracts[rolldates, "rollToday"] <- 1
  
  # Make a variable to track when we switch between C1 and C2
  combined_contracts$switch <- 1
  combined_contracts[rolldates, "switch"] <- -1
  combined_contracts[setldates, "switch"] <- -1
  combined_contracts$whichContract <- cumprod(combined_contracts$switch)
  
  # make effective front-month = Settle1 if it is active & we should use it
  combined_contracts$front <- with(combined_contracts, ifelse(whichContract == 1 & rollToday == 0, combined_contracts[,1], combined_contracts[,2]) )
  
  # if we don't roll today, ratio = 1
  # if we DO roll today, ratio = F(Tn,Tn) / F(Tn,Tn+1).
  combined_contracts$ratio <- with(combined_contracts, ifelse(rollToday == 1, combined_contracts[,1]/combined_contracts[,2], 1) )
  
  # index is just the cumulative product times the front month contract
  combined_contracts$index <- with(combined_contracts, cumprod(ratio) * front)
  out <- combined_contracts[, c("s1", "s2", "rollToday", "ratio", "switch", "whichContract", "front", "index")]
  names(out) <- c("Settle.1", "Settle.2", "roll", "Price.Ratio",
                  "switch.contra
                  cts", "which.contract", "First.Contract", "Synthetic.Index")
  return(out)
}

switchdays <- c(22L, 28L)
rolled_crude <- makeRolledIndex(crude1_roll$Settle, crude2_roll$Settle, switchday=switchdays, subset = "1990/2015")
head(rolled_crude)
rolled_ng <- makeRolledIndex(ng1_roll$Settle, ng2_roll$Settle, switchday=switchdays, subset = "1990/2015")
head(rolled_ng)

#Create plots:

#Plot continous vs. synthetic for crude between 1990 and 2015
plot.zoo(rolled_crude[,7:8], main = 'Crude Futures Return: Continuous vs. Synthetic', plot.type="s",ylab = 'Return', col=1:2)
legend("topleft", legend = c('Continuous Contract', colnames(rolled_crude[,8])), fill=1:2, horiz=F)

#Plot continous vs. synthetic for natural gas between 1990 and 2015
plot.zoo(rolled_ng[,7:8],main = 'NG Futures Return: Continuous vs. Synthetic', plot.type="s",ylab = 'Return', col=1:2)
legend("topleft", legend = c('Continuous Contract', colnames(rolled_ng[,8])), fill=1:2, horiz=F)

Year2009<- "2009-01/2009-12"
#Plot continous vs. synthetic for crude in 2009
plot.zoo(rolled_crude[Year2009,7:8],main = 'Crude Futures Return: Continuous vs. Synthetic', plot.type="s", col=1:2,ylab = 'Return', xlab = '2009')
legend("topleft", legend = c('Continuous Contract', colnames(rolled_crude[,8])), fill=1:2, horiz=F)

#Plot continous vs. synthetic for natural gas in 2009
plot.zoo(rolled_ng[Year2009,7:8],main = 'NG Futures Return: Continuous vs. Synthetic', plot.type="s", col=1:2,ylab = 'Return', xlab = '2009')
legend("topleft", legend = c('Continuous Contract', colnames(rolled_crude[,8])), fill=1:2, horiz=F)
