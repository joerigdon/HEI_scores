##Load packages and functions
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Tables.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Figures.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Functions.R")
library('hei')

##Read in data
c1_bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c1_bl.csv", header=TRUE)
c2_bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c2_bl.csv", header=TRUE)
c3_bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c3_bl.csv", header=TRUE)
c4_bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c4_bl.csv", header=TRUE)
c5_bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c5_bl.csv", header=TRUE)
c6_bl = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c6_bl.csv", header=TRUE)

c1_m6 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c1_m6.csv", header=TRUE)
c2_m6 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c2_m6.csv", header=TRUE)
c3_m6 = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c3_m6.csv", header=TRUE)

##Look at data
summary(c1_bl$HEI)
summary(c2_bl$HEI)
summary(c3_bl$HEI)
summary(c4_bl$HEI)
summary(c5_bl$HEI)
summary(c6_bl$HEI)

summary(c1_m6$HEI)
summary(c2_m6$HEI)
summary(c3_m6$HEI)

##Average recalls within person, merge w/ diet, and summarize
perID = function(dta) {
    dta2 = aggregate(dta[, 3:16], by=list(dta$ID), function(x) mean(x, na.rm=TRUE))
    names(dta2)[1] = "ID"
    dta2
}

c1bl = perID(c1_bl)
c1bl$time = "a.Baseline"
c1bl$cohort = 1

c2bl = perID(c2_bl)
c2bl$time = "a.Baseline"
c2bl$cohort = 2

c3bl = perID(c3_bl)
c3bl$time = "a.Baseline"
c3bl$cohort = 3

c4bl = perID(c4_bl)
c4bl$time = "a.Baseline"
c4bl$cohort = 4

c5bl = perID(c5_bl)
c5bl$time = "a.Baseline"
c5bl$cohort = 5

c6bl = perID(c6_bl)
c6bl$time = "a.Baseline"
c6bl$cohort = 6

c1m6 = perID(c1_m6)
c1m6$time = "b.6months"
c1m6$cohort = 1

c2m6 = perID(c2_m6)
c2m6$time = "b.6months"
c2m6$cohort = 2

c3m6 = perID(c3_m6)
c3m6$time = "b.6months"
c3m6$cohort = 3

##Put together and then merge w/ assignment
dta = rbind(c1bl, c2bl, c3bl, c4bl, c5bl, c6bl, c1m6, c2m6, c3m6)
names(dta)[1] = "chives_id"

g = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/EVIDENCE/Data/DATA_2018-01-12.csv", header=TRUE)
g2 = g[, names(g) %in% c("chives_id", "randomize_group")]
b = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/EVIDENCE/Data/Blinded_2018-01-03.csv", header=TRUE)
g3 = merge(g2, b, by="randomize_group", all.x=TRUE)
g4 = g3[, -1]

dta2 = merge(dta, g4, by="chives_id", all.x=TRUE)

dta3 = dta2[!dta2$chives_id %in% c('CP1078', 'CP1115', 'CP1144', 'CP1148', 'CP3220'), ]

##Summarize in table
##By group
tabBL = mktab(data=dta3[dta3$time=="a.Baseline", ], var.names=c("HEI"), ind.cat=c(0), group.name=c("group"), cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=2)
tab6 = mktab(data=dta3[dta3$time=="b.6months", ], var.names=c("HEI"), ind.cat=c(0), group.name=c("group"), cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=2)
tab1 = rbind(tabBL, tab6[2, ])

##By cohort
tabBLc = mktab(data=dta3[dta3$time=="a.Baseline", ], var.names=c("HEI"), ind.cat=c(0), group.name=c("cohort"), cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=2)
tab6c = mktab(data=dta3[dta3$time=="b.6months", ], var.names=c("HEI"), ind.cat=c(0), group.name=c("cohort"), cfn=describeMean, miss="always", pval=FALSE, tot="last", digit=2)
tab2 = rbind(tabBLc, tab6c[2, ])

word.doc(obj.list=list(tab1, tab2), obj.title=c("Table 1: HEI score by study arm", "Table 2: HEI score by cohort"), dest="/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Tables/Prelim_HEI_2018-02-02.docx", ftype="Arial", col.odd="white")

##Radar plots for HEI
library(fmsb)

radar = function(dat,names.d,cols,n.axes,tit,lab.cex,axis.cex, pltys) {
    colnames(dat) = names.d
    radarchart(dat,pcol=cols,axistype=4,plty=pltys,seg=n.axes-1,plwd=2,cglty=1,cglcol="gray75",vlcex=lab.cex,calcex=axis.cex,axislabcol=1,caxislabels=round(seq(from=min(dat[1:2,]),to=max(dat[1:2,]),length=n.axes),2),title=tit)
}

##Radar plot of HEI subcomponents
dta4 = dta3[dta3$time=="a.Baseline", ]
bl = aggregate(dta4[, 2:14], list(dta4$group), function(x) mean(x, na.rm=TRUE))

dta5 = dta3[dta3$time=="b.6months", ]
m6 = aggregate(dta5[, 2:14], list(dta5$group), function(x) mean(x, na.rm=TRUE))

pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Figures/Radar_Baseline_2018-02-02.pdf")
par(mfrow=c(1,1))
radar(dat=rbind(rep(10,13), rep(0,13), bl[1, 2:14], bl[2, 2:14], bl[3, 2:14], bl[4, 2:14]),c("black", "red", "green", "blue"),names.d=names(bl)[2:14], n.axes=4, tit="Baseline", lab.cex=0.6,axis.cex=0.5, pltys=c(1,1,1,1))
legend('topright',legend=c("A", "B", "C", "D"), col=c("black", "red", "green", "blue"), lty=c(1,1,1,1), lwd=2, bty='n',cex=0.8)
dev.off()

pdf("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Figures/Radar_6months_2018-02-02.pdf")
radar(dat=rbind(rep(10,13), rep(0,13), m6[1, 2:14], m6[2, 2:14], m6[3, 2:14], m6[4, 2:14]),c("black", "red", "green", "blue"),names.d=names(m6)[2:14], n.axes=4, tit="6 Months", lab.cex=0.6,axis.cex=0.5, pltys=c(1,1,1,1))
legend('topright',legend=c("A", "B", "C", "D"), col=c("black", "red", "green", "blue"), lty=c(1,1,1,1), lwd=2, bty='n',cex=0.8)
dev.off()
