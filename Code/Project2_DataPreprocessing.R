# Math 239 Project 2 Company complaints
# Data preprocessing

setwd("/Users/XC/Dropbox/SJSU/Courses/Cmpe 239 2016 Spring/Project2/Code")

library(randomForest)
library(data.table)
library(bit64)
CC.Data <- fread("../Data/company_complaints_2016.csv",header = TRUE, stringsAsFactors = TRUE,na.strings = "")
CC.Data$complaint_id <- NULL
CC.Data$zipcode <- NULL
CC.Data$date_received <- NULL
CC.Data[CC.Data=="<NA>"] = NA 

CC.Data <- as.matrix(CC.Data)

CC.Data[is.na(CC.Data)] <- "Blank"
CC.Data <- as.data.frame(CC.Data )


CC.Data$consumer_complaint_narrative[CC.Data$consumer_complaint_narrative != "Blank"] <- "Yes"
CC.Data <- as.matrix(CC.Data)
CC.Data[is.na(CC.Data)] <- "Yes"
CC.Data <- as.data.frame(CC.Data )

# delete row: 
# 3603, 5644, 6135, 11988, 29215, 29715, 32589, 
# because including illigal characters sub_product
CC.Data <- CC.Data[-c(3603, 5644, 6135, 11988, 29215, 29715, 32589),]
CC.Data <- as.data.frame(CC.Data)
table(CC.Data$`consumer_disputed?`)


# delete non-existing state
# sort(summary(CC.Data$state))

index1 <- which(CC.Data$state == "AA"|CC.Data$state == "MP"|CC.Data$state == "AS"|
                  CC.Data$state == "MH"|CC.Data$state == "FM"|CC.Data$state == "FU"|
                  CC.Data$state == "AE"|CC.Data$state == "AP"|CC.Data$state == "VI"|
                  CC.Data$state == "WY"|CC.Data$state == "AK"|CC.Data$state == "ND")

CC.Data <- CC.Data[-index1,]

## DATE into four categories

CC.Data$date_sent_to_company <- as.Date(CC.Data$date_sent_to_company, format = "%m/%d/%Y")
CC.Data$date_sent_to_company <- factor(format(CC.Data$date_sent_to_company, "%m"))



########### select top 20 companies data ##

sort(summary(CC.Data$company)[c(1:20)],decreasing = TRUE)


subset.20 <- subset(CC.Data, CC.Data$company == "Experian" |CC.Data$company == "Equifax"|
                      CC.Data$company == "TransUnion Intermediate Holdings, Inc." |CC.Data$company == "Bank of America"|
                      CC.Data$company == "Wells Fargo & Company" |CC.Data$company == "JPMorgan Chase & Co."|
                      CC.Data$company == "Citibank" |CC.Data$company == "Ocwen"|
                      CC.Data$company == "Capital One" |CC.Data$company == "Nationstar Mortgage"|
                      CC.Data$company == "Synchrony Financial" |CC.Data$company == "U.S. Bancorp"|
                      CC.Data$company == "Navient Solutions, Inc." |CC.Data$company == "Ditech Financial LLC"|
                      CC.Data$company == "Amex" |CC.Data$company == "Encore Capital Group"|
                      CC.Data$company == "PNC Bank N.A." |CC.Data$company == "Discover"|
                      CC.Data$company == "Select Portfolio Servicing, Inc" |CC.Data$company == "Portfolio Recovery Associates, Inc."
)
## DELETE ISSUE

sort(summary(subset.20$issue),decreasing = TRUE)

index2 <- which(subset.20$issue == "Received a loan I didn't apply for"|subset.20$issue == "Lender repossessed or sold the vehicle"|
                  subset.20$issue == "Charged bank acct wrong day or amt"|subset.20$issue == "Disclosures"|
                  subset.20$issue == "Incorrect/missing disclosures or info"|subset.20$issue == "Lost or stolen money order"|
                  subset.20$issue == "Can't stop charges to bank account"|subset.20$issue == "Excessive fees"|
                  subset.20$issue == "Lost or stolen check"|subset.20$issue == "Wrong amount charged or received"|
                  subset.20$issue == "Convenience checks"|subset.20$issue == "Incorrect exchange rate"|
                  subset.20$issue == "Can't contact lender"|subset.20$issue == "Charged fees or interest I didn't expect"|
                  subset.20$issue == "Payment to acct not creditedr"|subset.20$issue == "Unexpected/Other fees"|
                  subset.20$issue == "Applied for loan/did not receive money"|subset.20$issue == "Cash advance fee"|
                  subset.20$issue == "Overdraft, savings or rewards features"|subset.20$issue == "Advertising, marketing or disclosures"|
                  subset.20$issue == "Overlimit fee"|subset.20$issue == "Other service issues"|
                  subset.20$issue == "Adding money"|subset.20$issue == "Fees"|
                  subset.20$issue == "Balance transfer fee"|subset.20$issue == "Account terms and changes"
)

new <- subset.20[-index2, ]
subset.20 <- subset.20[-index2,]

# delete sub-issue: less than 19

sort(summary(subset.20$sub_issue),decreasing = TRUE)

index3 <- which(subset.20$sub_issue == "Charged fees or interest I didn't expect"|subset.20$sub_issue == "Payment to acct not credited"|
                  subset.20$sub_issue == "Can't stop charges to bank account"|subset.20$sub_issue == "Charged bank acct wrong day or amt"|
                  subset.20$sub_issue == "Applied for loan/did not receive money"|subset.20$sub_issue == "Can't contact lender"|
                  subset.20$sub_issue == "Qualify for a better loan than offered"|subset.20$sub_issue == "Contacted me instead of my attorney"|
                  subset.20$sub_issue == "Attempted to/Collected exempt funds"|subset.20$sub_issue == "Indicated shouldn't respond to lawsuit"|
                  subset.20$sub_issue == "Used obscene/profane/abusive language"|subset.20$sub_issue == "Can't qualify for a loan"|
                  subset.20$sub_issue == "Report shared with employer w/o consent"|subset.20$sub_issue == "Threatened arrest/jail if do not pay"|
                  subset.20$sub_issue == "Indicated committed crime not payingr"|subset.20$sub_issue == "Sued where didn't live/sign for debt"|
                  subset.20$sub_issue == "Called outside of 8am-9pm"|subset.20$sub_issue == "Contacted employer after asked not to"|
                  subset.20$sub_issue == "Received a loan I didn't apply for")


subset.20.2 <- subset.20[-index3,]
dim(subset.20.2)

## delete sub_product < 29
sort(summary(subset.20.2$sub_product), decreasing = TRUE)

# 3603, 5644, 6135, 11988, 29215, 29715, 32589, 
index4 <- which(subset.20.2$sub_product == "Vehicle lease "|subset.20.2$sub_product == "Gift or merchant card"|
                  subset.20.2$sub_product == "Medical"|subset.20.2$sub_product == "Check cashing"|
                  subset.20.2$sub_product == "Other special purpose card"|subset.20.2$sub_product == "Refund anticipation check"|
                  subset.20.2$sub_product == "ID prepaid card"|subset.20.2$sub_product == "Debt settlement"|
                  subset.20.2$sub_product == "Payroll card"|subset.20.2$sub_product == "Title loan"|
                  subset.20.2$sub_product == "Payday loan"|
                  subset.20.2$sub_product == "Government benefit payment card "|subset.20.2$sub_product == "Mobile wallet"|
                  subset.20.2$sub_product == "Money order"|subset.20.2$sub_product == "Electronic Benefit Transfer / EBT card"|
                  subset.20.2$sub_product == "Foreign currency exchange"|subset.20.2$sub_product == "Second mortgage"|
                  subset.20.2$sub_product == "Transit card"|subset.20.2$sub_product == "Credit repair"|
                  subset.20.2$sub_product == "Pawn loan")


subset.20.3 <- subset.20.2[-index4,]

#############################################
## divide data into train dataset and test dataset
## 2/3 as training data, 1/3 as testing data
#############################################


rm(CC.Data)

CC.Data <- subset.20.3
CC.Data <- lapply(CC.Data, factor)
CC.Data <- as.data.frame(CC.Data)
class(CC.Data)

# size CC.Data at this step is 30095 by 15

## For all data
l.1 <- length(CC.Data$product)
set.seed(123456789)
total.index <- sample(1:l.1, 2*l.1/3,replace= F)
CC.train <- CC.Data[total.index,]
CC.test <- CC.Data[-total.index,]

# Dimension of CC.train and CC.test: 20063 by 15 and 10032 by 15
dim(CC.train)
dim(CC.test)


# save data into files
write.csv(CC.train, file = '../Data/CCTrain.csv', row.names = F)
write.csv(CC.test, file = '../Data/CCTest.csv', row.names = F)

