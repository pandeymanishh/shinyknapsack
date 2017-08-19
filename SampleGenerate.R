setwd("Shiny Tutorial/")
library(data.table)
alllist<-paste0("Company_",1:50)

syndicatedt<-CJ(c("issuer1","issuer2","issuer3","issuer4","issuer5"),as.character(c(Sys.Date(),Sys.Date()+20,Sys.Date()+50))
                ,c(1000,2000,5000,10000),alllist)

setnames(syndicatedt,c("Issuer","Launch_Date","Deal_Amount","Lender"))

#Add the proportion column
syndicatedt[,dec_prob:=runif(nrow(syndicatedt)),]

#Add the contribution
syndicatedt[,cont_prop:=runif(nrow(syndicatedt),min = 0,max = 0.3),]

#get the actual contribution
syndicatedt[,DollarAmt:=round(cont_prop*Deal_Amount,0)]

fwrite(syndicatedt,"samplefile.txt",sep="|",row.names=FALSE)
