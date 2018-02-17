# import Libs
library("lubridate", lib.loc="~/R/win-library/3.3")
library(readr)

# Read Files
Collections <- read_csv("C:/Users/arjit/Desktop/audit/arCollections.csv", 
                        col_types = cols(X1 = col_skip(), cust.no = col_character(), 
                                         dateColl = col_character(), invoice = col_character()))
View(Collections)

Confirmations <- read_csv("C:/Users/arjit/Desktop/audit/arConfirmations.csv", 
                          col_types = cols(X1 = col_skip(), cust.no = col_character(), 
                                           invoice = col_character()))

View(Confirmations)
str(Confirmations)

custCredit <- read_csv("C:/Users/arjit/Desktop/audit/custCredit.csv", 
                       col_types = cols(customer.no = col_character()))
View(custCredit)

empReimbursements <- read_csv("C:/Users/arjit/Desktop/audit/empReimbursements.csv", 
                              col_types = cols(Employee.No = col_character(), 
                                               Receipt.No = col_character()))
View(empReimbursements)

inventoryCounts <- read_csv("C:/Users/arjit/Desktop/audit/inventoryCounts.csv", 
                            col_types = cols(sku = col_character()))
View(inventoryCounts)

inventoryPerpetual <- read_csv("C:/Users/arjit/Desktop/audit/inventoryPerpetual.csv", 
                               col_types = cols(X1 = col_skip(), sku = col_character()))
View(inventoryPerpetual)

purchases <- read_csv("C:/Users/arjit/Desktop/audit/purchases.csv", 
                      col_types = cols(PO.no = col_character(), 
                                       X1 = col_skip(), date = col_character(), 
                                       sku = col_character()))
View(purchases)
sales <- read_csv("C:/Users/arjit/Desktop/audit/sales.csv", 
                  col_types = cols(X1 = col_skip(), cust.no = col_character(), 
                                   date = col_character(), invoice = col_character(), 
                                   sku = col_character()))
View(sales)




# Clean Files

sales$date<-as_date(sales$date)
Collections$dateColl<-as_date(Collections$dateColl)
empReimbursements$Amount <- empReimbursements$Amount
empReimbursements$Amount<-gsub('\\$','',empReimbursements$Amount)
empReimbursements$Amount<-as.numeric(empReimbursements$Amount)
purchases$date<-as_date(purchases$date)


# files for 2016
sales$year <- year(sales$date)
sales16 <- split(sales, sales$year)$'2016'

Collections$year<- year(Collections$dateColl)
Collections16 <- split(Collections, Collections$year)$'2016'
View(Collections16)
purchases$year <- year(purchases$date)
purchases16 <- split(purchases, purchases$year)$'2016'



# TEST FOR INTERNAL CONTROLS
#1
library(plyr, dplyr)
sales1 = split(sales16, sales16$cashtrue)[["FALSE"]]
sales1 = subset(sales16, select = c(date, cust.no, total))
names(sales1)[names(sales1) == "total"] = "trans"
sales1$trans = sales1$trans*-1
View(sales1)
str(sales1)
#Prepare Collections table
collections1 = merge(sales16, Collections16, by = "invoice", all.x = T)
collections1 = na.omit(collections1)
str(collections1)
collections1 = subset(collections1, select = c(dateColl, cust.no.x, amt.received))
names(collections1)[names(collections1) == "cust.no.x"] = "cust.no"
names(collections1)[names(collections1) == "dateColl"] = "date"
names(collections1)[names(collections1) == "amt.received"] = "trans"
str(collections1)
#TransactionsTable
transTable = rbind(sales1, collections1)
transTable = arrange(transTable, date)
#Create TransByCustomer
transByCustomer = split(transTable, transTable$cust.no)

#Loop through customers
badCreditAccount = data.frame()
for(i in 1:length(transByCustomer)) {
  customer = transByCustomer[[i]]
  customerNumber = transByCustomer[[i]][1,]$cust.no
  customer$subTotal = custCredit[as.numeric(customerNumber),]$limit
  #loop through customer
  for(n in 1:length(customer$subTotal)) {
    if(n != 1) {
      customer[n,]$subTotal = customer[n - 1,]$subTotal + customer[n,]$trans
      if(sign(customer[n,]$subTotal) == -1) {
        badCreditAccount = rbind(badCreditAccount, customer[n,])
        break
      }
    }
  }
}
View(badCreditAccount)


#2
#duplicate values
table(duplicated(sales16$invoice))
#False , no duplicated value


#omitted entries

findMissingEntries =function(max,set){
  good = 1:max
  test = as.numeric(set)
  missing = setdiff(good, set)
  print(missing)
  print ("Missing (head)")
  print(length(missing))
  head(missing) 
}

findMissingEntries(max = length(sales16$invoice), set = sales16$invoice)
#missing entries head
#180733 entries
#[1] "Missing (head)"
#[1]  9 12 13 15 26 38


#Transcation Cutoff

findSalesNotIn2016 = function(accounts) {
  x =sales
  x$year = year(sales$date)
  y = split(x, x$year)
  z = rbind(y[["2015"]], y[["2017"]])
  print("Sales not in 2016")
  print(z)
  print ("Sales not in 2016 (head)")
  head(z)
}
findSalesNotIn2016(accounts)
#[1] "Sales not in 2016 (head)"
# # A tibble: 6 × 9
# invoice   sku   qty cashtrue       date unitprice   total cust.no  year
# <chr> <chr> <int>    <int>     <date>     <dbl>   <dbl>   <chr> <dbl>
#   1       9   445    42        0 2015-12-17      4.56  191.52     307  2015
# 2      12  1343    37        0 2015-12-13     18.93  700.41     544  2015
# 3      26    83   154        0 2015-12-12     20.71 3189.34     146  2015
# 4      38  1545    60        0 2015-12-29     27.55 1653.00     800  2015
# 5      46   699    46        0 2015-12-30     14.36  660.56     474  2015
# 6      75   896     5        0 2015-12-10     20.59  102.95      37  2015



#ACCOUNT RECEIVABLE AUDIT
#1
str(Collections)
str(custCredit)
str(sales)
splitSalesbyTransaction = split(sales16, sales16$cashtrue) # splitting salezs by cashtrue
str(splitSalesbyTransaction)
credit = splitSalesbyTransaction$`0`
str(credit)
allCreditAccounts = merge(credit, Collections16, by="invoice", all.x = T)
View(allCreditAccounts)
allCreditAccounts$notCollected = is.na(allCreditAccounts$dateColl)
table(allCreditAccounts$notCollected)
allCreditAccountsbyCollection = split(allCreditAccounts, allCreditAccounts$notCollected)
str(allCreditAccountsbyCollection)
unpaidAccountsRecievable = allCreditAccountsbyCollection[["TRUE"]]
unpaidAccountsRecievable




#2
#Unpaid Account Age
endDateVector = rep(ymd("2016/12/31"), length(unpaidAccountsRecievable$invoice))
unpaidAccountsRecievable$endDate = endDateVector
unpaidAccountsRecievable$daysSincePurchase = unpaidAccountsRecievable$endDate - unpaidAccountsRecievable$date
unpaidAccountsRecievable$interval = findInterval(unpaidAccountsRecievable$daysSincePurchase, c(90, 180))
str(unpaidAccountsRecievable)
print("Uncollected AR")
print(sum(unpaidAccountsRecievable$total))
#333286020
doubtfulTotals = aggregate(total~unpaidAccountsRecievable$interval, unpaidAccountsRecievable, sum)
print(0.3*doubtfulTotals$total[2] + 0.5*doubtfulTotals$total[3])
print("Allowance for Doubtful Accounts")
#237693229


#3
View(badCreditAccount)

#4
#Sales Cutoff

findSalesNotIn2016 = function(accounts) {
  x =sales
  x$year = year(sales$date)
  y = split(x, x$year)
  z = rbind(y[["2015"]], y[["2017"]])
  print("Sales not in 2016")
  print(z)
  print ("Sales not in 2016 (head)")
  head(z)
}
findSalesNotIn2016(accounts)


#5
# all the customers are having negative balances


#6
str(unpaidAccountsRecievable)
d= 1000000/sum(unpaidAccountsRecievable$total)
#part a
install.packages("pwr")
library(pwr)
pwr.t.test(n = NULL, d = 0.0023, sig.level = 0.05, power = .8, type = "one.sample","greater")
# One-sample t test power calculation 
# 
# n = 686952.2
# d = 0.003
# sig.level = 0.05
# power = 0.8
# alternative = greater

#changing tolerable error
pwr.t.test(n = NULL, d = 0.09, sig.level = 0.05, power = .8,  "one.sample",  "greater")
sum(unpaidAccountsRecievable$total)*0.09
?sample
#part b
allconfirm<- merge(Confirmations, Collections16, by="invoice")
str(allconfirm)
TE=1000000
TES=TE/length(allconfirm$invoice)
ss = round(8*(sd(allconfirm$amt.received.x)/TES)^2,0)
set.seed(9999)
allconfirm_sample = allconfirm[sample(allconfirm$amt.received.x, 500, replace=FALSE),]
str(allconfirm_sample)

m<-mean(unpaidAccountsRecievable$total)

t.test(allconfirm_sample$amt.received.x, mu = m, conf.level=.95)

#7
#for every record we we will allow 2.43 mean tolerable error
#965.2407+2.43< 987.92  so the account has "intolerable error" at the 95% confidence limit


