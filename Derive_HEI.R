##Load functions
source("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Code/NDSR_hei.R")

##Read in data (reports 4 and 9, plus food names)
#foods = read.csv("/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Code/foods.csv", header=TRUE)

##Cohort 1
##Baseline
c1blr4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/Baseline Data/ChivesC1BL04.csv', header=TRUE)
c1blr9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/Baseline Data/ChivesC1BL09.csv', header=TRUE)
c1_bl = hei_ndsr(c1blr4, c1blr9)
write.csv(c1_bl, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c1_bl.csv", row.names=FALSE)

##6 months
c1m6r4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/6 Month Data/ChiveC16Malld/ChiveC16Mall04.csv', header=TRUE)
c1m6r9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 1/6 Month Data/ChiveC16Malld/ChiveC16Mall09.csv', header=TRUE)
c1_m6 = hei_ndsr(c1m6r4, c1m6r9)
write.csv(c1_m6, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c1_m6.csv", row.names=FALSE)

##Cohort 2
##Baseline
c2blr4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/Baseline Data/ChiveC2BLalld/ChiveC2BLall04.csv', header=TRUE)
c2blr9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/Baseline Data/ChiveC2BLalld/ChiveC2BLall09.csv', header=TRUE)
c2_bl = hei_ndsr(c2blr4, c2blr9)
write.csv(c2_bl, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c2_bl.csv", row.names=FALSE)

##6 months
c2m6r4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/6 Month Data/ChiveC26Malld/ChiveC26Mall04.csv', header=TRUE)
c2m6r9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 2/6 Month Data/ChiveC26Malld/ChiveC26Mall09.csv', header=TRUE)
c2_m6 = hei_ndsr(c2m6r4, c2m6r9)
write.csv(c2_m6, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c2_m6.csv", row.names=FALSE)

##Cohort 3
##Baseline
c3blr4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/Baseline Data/ChiveC3BLalld/ChiveC3BLall04.csv', header=TRUE)
c3blr9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/Baseline Data/ChiveC3BLalld/ChiveC3BLall09.csv', header=TRUE)
c3_bl = hei_ndsr(c3blr4, c3blr9)
write.csv(c3_bl, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c3_bl.csv", row.names=FALSE)

##6 months
c3m6r4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/6 Month Data/ChiveC36Malld/ChiveC36Mall04.csv', header=TRUE)
c3m6r9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 3/6 Month Data/ChiveC36Malld/ChiveC36Mall09.csv', header=TRUE)
c3_m6 = hei_ndsr(c3m6r4, c3m6r9)
write.csv(c3_m6, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c3_m6.csv", row.names=FALSE)

##Cohort 4
##Baseline
c4blr4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/Baseline/ChiveC4BLalld/ChiveC4BLall04.csv', header=TRUE)
c4blr9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 4/Baseline/ChiveC4BLalld/ChiveC4BLall09.csv', header=TRUE)
c4_bl = hei_ndsr(c4blr4, c4blr9)
write.csv(c4_bl, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c4_bl.csv", row.names=FALSE)


##Cohort 5
##Baseline
c5blr4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/Baseline/ChiveC5BLalld/ChiveC5BLall04.csv', header=TRUE)
c5blr9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 5/Baseline/ChiveC5BLalld/ChiveC5BLall09.csv', header=TRUE)
c5_bl = hei_ndsr(c5blr4, c5blr9) #error here
write.csv(c5_bl, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c5_bl.csv", row.names=FALSE)

##Cohort 6
##Baseline
c6blr4 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Baseline/ChiveC6BLalld/ChiveC6BLall04.csv', header=TRUE)
c6blr9 = read.csv('/Volumes/projects/Sanjay Projects/CHIVES/Chives NDSR Data/Cohort 6/Baseline/ChiveC6BLalld/ChiveC6BLall09.csv', header=TRUE)
c6_bl = hei_ndsr(c6blr4, c6blr9)
write.csv(c6_bl, "/Users/jrigdon/Box sync/Rigdon/Sanjay/CHIVES/HEI/Datasets/c6_bl.csv", row.names=FALSE)
