# clean current work space
rm(list=ls(all=T))
options(stringsAsFactors = F)   # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation

library(descr)
library(survival)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(survminer) # Can make nicer kaplan meier plots than base R

# note - need to add the any event column this was done on the CSV document

setwd('~/Siontis research/VT ablation/')
# All data
# VTdata <- read.csv('12-10 R - Data extraction VT imaging Siontis.csv', na.strings = c("", "NA"))
VTdata <- read.csv('5-7-23 WS edits - Data extraction VT imaging Siontis.csv', na.strings = c("", "NA"))
  # Separate out repeats and non-repeats
  repeats <- VTdata[132:146,] # Grabs the repeat ablations of those w/ multiple ablations during our time period
    VTdata <- VTdata[-c(130:131),]
VTdata <- VTdata[1:129,] # Gets rid of people who had >1 ablation in our time period
View(VTdata)
View(repeats)

VTdata
Str(VTdata)
View(VTdata)

# To run demographics for those with CMR
VTdata <- subset(VTdata, subset = VTdata[,c(11)] == "1")
View(VTdata)

# To run demographics for those without CMR
VTdata <- subset(VTdata, subset = VTdata[,c(11)] == "0")
View(VTdata)

# To run demographics for those with imaging
VTdata <- subset(VTdata, subset = VTdata[,c(11)] == "1" | VTdata[,c(15)] == "1")
View(VTdata)

# To run demographics for those without imaging
VTdata <- subset(VTdata, subset = VTdata[,c(11)] == "0" & VTdata[,c(15)] == "0")
View(VTdata)

#Demographics / periprocedural / post procedural 
freq(VTdata[,c(1)]) # Sex, 1 = female, 2 = male
mean(VTdata[,c(2)]) # Mean age
  sd(VTdata[,c(2)])
mean(VTdata[,c(20)]) # EF at baseline
  sd(VTdata[,c(20)])
freq(VTdata[,c(3)]) # Proportion w/ ICD, 1 = yes
freq(VTdata[,c(6)]) # Redo ablations, 1 = yes
  Redo <- subset(VTdata, subset = VTdata[,c(6)] == "1")
  freq(Redo[,c(7)] == "1") # Redo was at Mayo True is at MAyo, False is at OSH
freq(VTdata[,c(9)]) # Type of cardiomyopathy; 1 = ischemic, 2 = other, 3 = mixed
  freq(VTdata[,c(10)]) # Type of nonischemic cardiomyopathy
freq(VTdata[,c(11)]) # MRI performed within 3 months
  MRIdone <- subset(VTdata, subset = VTdata[,c(11)] == "1") # Had an MRI done
  freq(MRIdone[,c(12)]) # MRI performed at Mayo  
freq(VTdata[,c(15)]) # CT performed within 3 months
  CTdone <- subset(VTdata, subset = VTdata[,c(15)] == "1") # Had a CT done
  freq(CTdone[,c(16)]) # 1 = done at Mayo, 0 = done elsewhere
  freq(VTdata[,c(17)]) # Type of CT
noimaging <- subset(VTdata, subset = VTdata[,c(11)] == "0" &  VTdata[,c(15)] == "0") # Had no imaging
  length(noimaging)
  nrow(noimaging)
mean(VTdata[,c(21)]) # RF time
  sd(VTdata[,c(21)])
mean(VTdata[,c(22)]) # Procedure duration
  sd(VTdata[,c(22)])
freq(VTdata[,c(23)]) # Epicardial access; 1 = yes
  freq(VTdata[,c(44)]) # epicardial ablation
freq(VTdata[,c(24)]) # Acute success; 1 = yes, 2 = partial, 3 = failure, 4 = not tested
freq(VTdata[,c(27)]) # VT recurrence 1 = yes, 2 = no
  hadrecurrence <- subset(VTdata, subset = VTdata[,c(27)] == "1")
  mean(hadrecurrence[,c(29)]) # Mean time to VT recurrence
  sd(hadrecurrence[,c(29)]) # SD of time to recurrence
freq(VTdata[,c(31)]) # Redo later; 1 = yes
freq(VTdata[,c(32)]) # Antiarrhythmic after procedure
  onantiarrhythmic <- subset(VTdata, subset = VTdata[,c(32)] == "1")
  freq(onantiarrhythmic[,c(34)]) # If on antiarrhythmic, was it for >1 year 0 = no, 1 = Y, 2 = <1yr, 3 = death/transplant, 4 = no record
freq(VTdata[,c(35)]) # Transplant at time of analysis
freq(VTdata[,c(39)]) # Dead? 1 = dead
mean(VTdata[,c(38)]) # Mean time to follow up / tx / death
  sd(VTdata[,c(38)]) # SD time to follow up / tx / death
summary(VTdata[,c(38)]) / 30
summary(VTdata[,c(38)])

mean(VTdata[,c(42)]) # Mean time to endpoint
sd(VTdata[,c(42)]) # SD time to endpoint
summary(VTdata[,c(42)])

# Subgroups of etiology
ischemic <- subset(VTdata,subset =  VTdata[,c(9)] == "1")
  freq(ischemic[,c(11)])
  freq(ischemic[,c(27)])
nonischemic <- subset(VTdata,subset =  VTdata[,c(9)] == "2")
freq(VTdata[,c(10)])
congenital <- VTdata[c(17, 105, 121, 124, 127),]
  freq(congenital[,c(11)])  
  freq(congenital[,c(27)])
dcm <- subset(VTdata,subset = VTdata[,c(10)] == "DCM")
  freq(dcm[,c(11)])
  freq(dcm[,c(27)])
hcm <- subset(VTdata,subset = VTdata[,c(10)] == "HCM")
  freq(hcm[,c(11)])
  freq(hcm[,c(27)])
sarcoid <- subset(VTdata, subset = VTdata[,c(10)] == "Sarcoid")
  freq(sarcoid[,c(11)])  
  freq(sarcoid[,c(27)])  
arvc <- subset(VTdata, subset = VTdata[,c(10)] == "ARVC")
  freq(arvc[,c(11)])  
  freq(arvc[,c(27)])
idiopathic <- VTdata[c()]
  
# Stats for table 1
#####  
  # To run demographics for those with CMR
  mri <- subset(VTdata, subset = VTdata[,c(11)] == "1")
  View(VTdata)
  
  # To run demographics for those without CMR
  nomri <- subset(VTdata, subset = VTdata[,c(11)] == "0")
  View(VTdata)
  
  # Sex
  freq(VTdata[,c(1)]) # Sex, 1 = female, 2 = male
  freq(mri[,c(1)]) # Sex, 1 = female, 2 = male
  freq(nomri[,c(1)]) # Sex, 1 = female, 2 = male
  shapiro.test(x = VTdata[,c(1)]) # Not normal
  sex <- matrix(c(41, 70, 7, 11), byrow = T, ncol = 2, nrow = 2)
  colnames(sex) <- c("mri", "No mri")
  rownames(sex) <- c("Male sex", "Female")
  sex
  chisq.test(sex)
  fisher.test(sex)
  model <- chisq.test(sex)
  model$expected
  
  # To run demographics for those with CMR
  mri <- subset(VTdata, subset = VTdata[,c(11)] == "1")
  View(VTdata)
  
  # To run demographics for those without CMR
  nomri <- subset(VTdata, subset = VTdata[,c(11)] == "0")
  View(VTdata)
  
  # Sex
  freq(VTdata[,c(1)]) # Sex, 1 = female, 2 = male
  freq(mri[,c(1)]) # Sex, 1 = female, 2 = male
  freq(nomri[,c(1)]) # Sex, 1 = female, 2 = male
  shapiro.test(x = VTdata[,c(1)]) # Not normal
  sex <- matrix(c(41, 70, 7, 11), byrow = T, ncol = 2, nrow = 2)
  colnames(sex) <- c("mri", "No mri")
  rownames(sex) <- c("Male sex", "Female")
  sex
  chisq.test(sex)
  fisher.test(sex)
  model <- chisq.test(sex)
  model$expected
  
  # Age
  mean(VTdata[,c(2)])
  sd(VTdata[,c(2)])
  plot(density(mri[,c(2)]))
  plot(density(nomri[,c(2)]))
  qqnorm(mri[,c(2)])
  qqline(mri[,c(2)])
  qqnorm(nomri[,c(2)])
  qqline(nomri[,c(2)])
  shapiro.test(x = mri[,c(2)])
  shapiro.test(x = nomri[,c(2)])
  boxplot(mri[,c(2)], nomri[,c(2)])
  wilcox.test(x = mri[,c(2)], 
              y = nomri[,c(2)],
              mu = 0, 
              conf.int = T,
              paired = F)
  
  t.test(x = a[,c(2)], y = d[,c(2)], paired = F)
  
  # Baseline EF
  mean(VTdata[,c(20)])
  sd(VTdata[,c(20)])
  
  shapiro.test(x = mri[,c(20)])
  shapiro.test(x = nomri[,c(20)])
  boxplot(mri[,c(20)], nomri[,c(20)])
  wilcox.test(x = mri[,c(20)], 
              y = nomri[,c(20)],
              mu = 0, 
              conf.int = T,
              paired = F)
  t.test(x = a[,c(20)], 
              y = d[,c(20)],
              mu = 0, 
              conf.int = T,
              paired = F)
  
  # Proportion w/ ICD
  freq(VTdata[,c(3)]) # Proportion w/ ICD, 1 = yes
  shapiro.test(x = VTdata[,c(3)]) # Not normal
  shapiro.test(x = a[,c(3)]) # Not normal
  shapiro.test(x = d[,c(3)]) # Not normal
  ICD <- matrix(c(43, 77, 5, 4), byrow = T, ncol = 2, nrow = 2)
  colnames(ICD) <- c("MRI", "No mri")
  rownames(ICD) <- c("ICD present", "no ICD")
  ICD
  chisq.test(ICD)
  fisher.test(ICD)
  
  # Redo ablations
  freq(VTdata[,c(6)]) # Redo ablations, 1 = yes
  Redo <- subset(VTdata, subset = VTdata[,c(6)] == "1")
  freq(Redo[,c(7)] == "1") # Redo was at Mayo True is at MAyo, False is at OSH
  
  redo <- matrix(c(14, 23, 34, 58), byrow = T, ncol = 2, nrow = 2)
  colnames(redomayo) <- c("MRI", "No mri")
  rownames(redomayo) <- c("redo @ mayo", "not")
  redomayo
  chisq.test(redomayo)
  fisher.test(redomayo)
  
  redomayo <- matrix(c(6, 13, 42, 68), byrow = T, ncol = 2, nrow = 2)
  colnames(redomayo) <- c("MRI", "No mri")
  rownames(redomayo) <- c("redo @ mayo", "not")
  redomayo
  chisq.test(redomayo)
  fisher.test(redomayo)
  
  redooutside <- matrix(c(8, 10, 40, 71), byrow = T, ncol = 2, nrow = 2)
  colnames(redooutside) <- c("MRI", "No MRI")
  rownames(redooutside) <- c("redo @ OSH", "not")
  redooutside
  chisq.test(redooutside)
  fisher.test(redooutside)
  
  # Type of cardiomyopathy
  freq(VTdata[,c(9)]) # Type of cardiomyopathy; 1 = ischemic, 2 = other, 3 = mixed
  
  typeofcm <- matrix(c(16, 36, 26, 38, 6, 7), byrow = T, ncol = 2)
  colnames(typeofcm) <- c("MRI", "no MRI")
  rownames(typeofcm) <- c("ischemic", "nonischemic", "mixed")
  typeofcm
  chisq.test(typeofcm)
  fisher.test(typeofcm)
  
  # By line
  ischemiccm <- matrix(c(28, 24, 45, 32), byrow = T, ncol = 2, nrow = 2)
  colnames(ischemiccm) <- c("Imaging", "No imaging")
  rownames(ischemiccm) <- c("Ischemic", "not ischemic")
  ischemiccm
  chisq.test(ischemiccm)
  fisher.test(ischemiccm)
  
  nonischemiccm <- matrix(c(38, 26, 35, 30), byrow = T, ncol = 2, nrow = 2)
  colnames(nonischemiccm) <- c("Imaging", "No imaging")
  rownames(nonischemiccm) <- c("nonischemic", "not nonischemic")
  nonischemiccm
  chisq.test(nonischemiccm)
  fisher.test(nonischemiccm)
  
  mixedcm <- matrix(c(7, 6, 66, 50), byrow = T, ncol = 2, nrow = 2)
  colnames(mixedcm) <- c("Imaging", "No imaging")
  rownames(mixedcm) <- c("mixedcm", "not mixedcm")
  mixedcm
  chisq.test(mixedcm)
  fisher.test(mixedcm)
  
  # MRI done
  freq(VTdata[,c(11)]) # MRI performed within 3 months
  MRIdone <- subset(VTdata, subset = VTdata[,c(11)] == "1") # Had an MRI done
  freq(MRIdone[,c(12)]) # MRI performed at Mayo  
  
  
  cMRI <- matrix(c(48, 48, 25, 81), byrow = T, ncol = 2, nrow = 2)
  colnames(cMRI) <- c("Imaging", "All")
  rownames(cMRI) <- c("MRIdone", "not")
  cMRI
  chisq.test(cMRI)
  fisher.test(cMRI)
  
  # CT done
  freq(VTdata[,c(15)]) # CT performed within 3 months
  CTdone <- subset(VTdata, subset = VTdata[,c(15)] == "1") # Had a CT done
  freq(CTdone[,c(16)]) # 1 = done at Mayo, 0 = done elsewhere
  freq(VTdata[,c(17)]) # Type of CT
  
  ct <- matrix(c(9, 22, 39, 59), byrow = T, ncol = 2, nrow = 2)
  colnames(ct) <- c("MRI", "No MRI")
  rownames(ct) <- c("epi access", "not")
  ct
  chisq.test(ct)
  fisher.test(ct)
  
  echo <- matrix(c(44, 69, 4, 12), byrow = T, ncol = 2, nrow = 2)
  colnames(echo) <- c("MRI", "no MRI")
  rownames(echo) <- c("TTE", "no TTE")
  echo
  chisq.test(echo)
  fisher.test(echo)
  
  # Had no imaging done
  noimaging <- subset(VTdata, subset = VTdata[,c(11)] == "0" &  VTdata[,c(15)] == "0") # Had no imaging
  length(noimaging)
  nrow(noimaging)
  
  # Epicardial access
  freq(VTdata[,c(23)]) # Epicardial access; 1 = yes
  epi <- matrix(c(9, 14, 39, 67), byrow = T, ncol = 2, nrow = 2)
  colnames(epi) <- c("MRI", "No MRI")
  rownames(epi) <- c("epi access", "not")
  epi
  chisq.test(epi)
  fisher.test(epi)
  
  # Epicardial ablation
  epiabl <- matrix(c(5,12,43, 69), byrow = T, ncol = 2, nrow = 2)
  colnames(epiabl) <- c("Imaging", "No imaging")
  rownames(epiabl) <- c("epi access", "not")
  epiabl
  chisq.test(epiabl)
  fisher.test(epiabl)
  
  # RFtime
  rfvt <- subset(VTdata, subset = !is.na(VTdata[,c(22)]))
  mean(rfvt[,c(22)])
  sd(rfvt[,c(22)])
  
  rfmr <- subset(a, subset = !is.na(a[,c(22)]))
  mean(rfmr[,c(22)])
  sd(rfmr[,c(22)])
  
  rfnomr <- subset(d, subset = !is.na(d[,c(22)]))
  mean(rfnomr[,c(22)])
  sd(rfnomr[,c(22)])
  
  shapiro.test(x = rfmr[,c(22)])
  shapiro.test(x = rfnomr[,c(22)])
  boxplot(rfmr[,c(22)], rfnomr[,c(22)])
  wilcox.test(x = rfmr[,c(22)], 
              y = rfnomr[,c(22)],
              mu = 0, 
              conf.int = T,
              paired = F)
  t.test(x = rfmr[,c(22)],y = rfnomr[,c(22)],paired = FALSE, mu = 0, conf.int = T)
  
  # Procedure duration
  pvt <- subset(VTdata, subset = !is.na(VTdata[,c(21)]))
  mean(pvt[,c(21)])
  sd(pvt[,c(21)])
  
  pmr <- subset(a, subset = !is.na(a[,c(21)]))
  mean(pmr[,c(21)])
  sd(pmr[,c(21)])
  
  pnomr <- subset(d, subset = !is.na(d[,c(21)]))
  mean(pnomr[,c(21)])
  sd(pnomr[,c(21)])
  
  shapiro.test(x = pmr[,c(21)])
  shapiro.test(x = pnomr[,c(21)])
  boxplot(pmr[,c(21)], pnomr[,c(21)])
  wilcox.test(x = pmr[,c(21)], 
              y = pnomr[,c(21)],
              mu = 0, 
              conf.int = T,
              paired = F)
  t.test(x = pmr[,c(21)],y = pnomr[,c(21)],paired = FALSE, mu = 0, conf.int = T)
  
  
  # Procedural success
  freq(VTdata[,c(24)])
  ps <- matrix(c(30, 50, 8, 19, 2, 0, 8, 12), byrow = T, ncol = 2)
  colnames(ps) <- c("MRI", "No mri")
  rownames(ps) <- c("acute success", "partial sucess", "Failure", "Not tested")
  ps
  chisq.test(ps)
  fisher.test(ps)
  
  # Acute success
  freq(VTdata[,c(24)]) # Acute success; 1 = yes, 2 = partial, 3 = failure, 4 = not tested
  as <- matrix(c(30, 50, 18, 31), byrow = T, ncol = 2, nrow = 2)
  colnames(as) <- c("Imaging", "No imaging")
  rownames(as) <- c("acute success", "not")
  as
  chisq.test(as)
  fisher.test(as)
  
  # Partial Success
  ps <- matrix(c(15, 12, 58, 44), byrow = T, ncol = 2, nrow = 2)
  colnames(ps) <- c("Imaging", "No imaging")
  rownames(ps) <- c("partial success", "not")
  ps
  chisq.test(ps)
  fisher.test(ps)
  
  # Failure
  fail <- matrix(c(1, 1, 72, 55), byrow = T, ncol = 2, nrow = 2)
  colnames(fail) <- c("Imaging", "No imaging")
  rownames(fail) <- c("acute failure", "not")
  fail
  chisq.test(fail)
  fisher.test(fail)
  
  # Not tested
  nt <- matrix(c(13, 7, 60, 49), byrow = T, ncol = 2, nrow = 2)
  colnames(nt) <- c("Imaging", "No imaging")
  rownames(nt) <- c("Not tested", "not")
  nt
  chisq.test(nt)
  fisher.test(nt)
  
  # VT recurrence
  freq(VTdata[,c(27)]) # VT recurrence 1 = yes, 2 = no
  vtrecur <- matrix(c(27, 47, 21, 34), byrow = T, ncol = 2, nrow = 2)
  colnames(vtrecur) <- c("MRI", "No MRI")
  rownames(vtrecur) <- c("VT recurrence", "not")
  vtrecur
  chisq.test(vtrecur)
  fisher.test(vtrecur)
  
  # Time to recurrence
  hadrecurrence <- subset(VTdata, subset = VTdata[,c(27)] == "1")
  mean(hadrecurrence[,c(29)]) # Mean time to VT recurrence
  sd(hadrecurrence[,c(29)]) # SD of time to recurrence
  
  imagingrecurrence <- subset(imaging, subset = imaging[,c(27)] == "1")
  noimagingrecurrence <- subset(noimaging, subset = noimaging[,c(27)] == "1")
  
  shapiro.test(x = imagingrecurrence[,c(29)])
  shapiro.test(x = noimagingrecurrence[,c(29)])
  boxplot(imagingrecurrence[,c(29)], noimagingrecurrence[,c(29)])
  wilcox.test(x = imagingrecurrence[,c(29)], 
              y = noimagingrecurrence[,c(29)],
              mu = 0, 
              conf.int = T,
              paired = F)
  t.test(x = imagingrecurrence[,c(29)],y = noimagingrecurrence[,c(29)],paired = FALSE, mu = 0, conf.int = T)
  
  # Repeat ablation later
  freq(VTdata[,c(31)]) # Redo later; 1 = yes
  repeatlater <- matrix(c(10, 9, 38, 72), byrow = T, ncol = 2, nrow = 2)
  colnames(repeatlater) <- c("MRI", "No MRI")
  rownames(repeatlater) <- c("repeat ablation after", "not")
  repeatlater
  chisq.test(repeatlater)
  fisher.test(repeatlater)
  
  # Antiarrhythmic
  ar <- matrix(c(21, 30, 6, 15, 7, 10, 6, 12, 8, 14), byrow = T, ncol = 2, nrow = 5)
  colnames(ar) <- c("MRI", "NoMRI")
  rownames(ar) <- c("on", "<1yr", "<1yr death/tx", "not on/norec", "not prescribed")
  ar
  chisq.test(ar)
  fisher.test(ar)
  
  # Antiarrhythmic after procedure
  freq(VTdata[,c(32)]) # Antiarrhythmic after procedure
  onantiarrhythmic <- subset(VTdata, subset = VTdata[,c(32)] == "1")
  freq(onantiarrhythmic[,c(34)]) # If on antiarrhythmic, was it for >1 year 0 = no, 1 = Y, 2 = <1yr, 3 = death/transplant, 4 = no record
  # Antiarrhythmic prescribed
  arp <- matrix(c(40, 67, 8, 14), byrow = T, ncol = 2, nrow = 2)
  colnames(arp) <- c("MRI", "No MRI")
  rownames(arp) <- c("ar prescribed", "not")
  arp
  chisq.test(arp)
  fisher.test(arp)
  # On antiarrhythmic >1 yr
  onar <- matrix(c(21, 30, 19, 37), byrow = T, ncol = 2, nrow = 2)
  colnames(onar) <- c("MRI", "No mri")
  rownames(onar) <- c("onar", "not")
  onar
  chisq.test(onar)
  fisher.test(onar)
  # <1 yr
  notayear <- matrix(c(6, 15, 34, 52), byrow = T, ncol = 2, nrow = 2)
  colnames(notayear) <- c("MRI", "No MRI")
  rownames(notayear) <- c("<1 yr since ablation", "not")
  notayear
  chisq.test(notayear)
  fisher.test(notayear)
  # <1 yr before death/transplant
  dt <- matrix(c(7, 10, 33, 57), byrow = T, ncol = 2, nrow = 2)
  colnames(dt) <- c("MRI", "No MRI")
  rownames(dt) <- c("<1 yr b4 death/transplant", "not")
  dt
  chisq.test(dt)
  fisher.test(dt)
  # Not on antiarrhythmic / no records
  nope <- matrix(c(6, 12, 34, 55), byrow = T, ncol = 2, nrow = 2)
  colnames(nope) <- c("MRI", "No MRI")
  rownames(nope) <- c("not on AR / no records", "not")
  nope
  chisq.test(nope)
  fisher.test(nope)
  
  # Transplant
  freq(VTdata[,c(35)]) # Transplant at time of analysis
  transplant <- matrix(c(4, 5, 44, 76), byrow = T, ncol = 2, nrow = 2)
  colnames(transplant) <- c("MRI", "No MRI")
  rownames(transplant) <- c("transplant", "not")
  transplant
  chisq.test(transplant)
  fisher.test(transplant)
  
  # Death
  freq(VTdata[,c(39)]) # Dead? 1 = dead
  death <- matrix(c(5, 11, 43, 70), byrow = T, ncol = 2, nrow = 2)
  colnames(death) <- c("MRI", "No MRI")
  rownames(death) <- c("death", "not")
  death
  chisq.test(death)
  fisher.test(death)
  
  # Time to endpoint
  mean(VTdata[,c(38)]) # Mean time to endpoint
  sd(VTdata[,c(38)]) # SD time to endpoint
  shapiro.test(x = imaging[,c(38)])
  shapiro.test(x = noimaging[,c(38)])
  boxplot(imaging[,c(38)], noimaging[,c(38)])
  wilcox.test(x = imaging[,c(38)], 
              y = noimaging[,c(38)],
              mu = 0, 
              conf.int = T,
              paired = F)
  t.test(x = imaging[,c(38)], 
         y = noimaging[,c(38)],
         mu = 0, 
         conf.int = T,
         paired = F)
  
# Coding for recurrence by imaging - table 4
#####
# Coding for recurrence by imaging - table 4
# col 42 is time to recurrence/transplant/death
mean(VTdata[,c(42)])
sd(VTdata[,c(42)])  
    
freq(VTdata[,c(27)]) # VT recurrence
hadrecurrence <- subset(VTdata, subset = VTdata[,c(27)] == "1")
mean(hadrecurrence[,c(29)]) # Mean time to VT recurrence
sd(hadrecurrence[,c(29)]) # SD of time to recurrence

  # What is recurrence when MRI obtained
  MRIdone <- subset(VTdata, subset = VTdata[,c(11)] == "1")
  freq(MRIdone[,c(27)])
  MRIhadrecurrence <- subset(MRIdone, subset = MRIdone[,c(27)] == "1")
  mean(MRIhadrecurrence[,c(29)]) # Mean time to VT recurrence
  sd(MRIhadrecurrence[,c(29)]) # SD of time to recurrence
  mean(MRIdone[,c(42)])
  sd(MRIdone[,c(42)]) 
    # What is recurrence when MRI not obtained
    noMRI <- subset(VTdata, subset = VTdata[,c(11)] == "0")
    freq(noMRI[,c(27)])
    noMRIhadrecurrence <- subset(noMRI, subset = noMRI[,c(27)] == "1")
    mean(noMRIhadrecurrence[,c(29)]) # Mean time to VT recurrence
    sd(noMRIhadrecurrence[,c(29)]) # SD of time to recurrence
    mean(noMRI[,c(42)])
    sd(noMRI[,c(42)])
    
  # What is recurrence when CT obtained
  CTdone <- subset(VTdata, subset = VTdata[,c(15)] == "1") # Had a CT done
  freq(CTdone[,c(27)])
  CThadrecurrence <- subset(CTdone, subset = CTdone[,c(27)] == "1")
  mean(CThadrecurrence[,c(29)]) # Mean time to VT recurrence
  sd(CThadrecurrence[,c(29)]) # SD of time to recurrence
    mean(CTdone[,c(42)])
    sd(CTdone[,c(42)])
    # What is recurrence when CT not obtained
    noCT <- subset(VTdata, subset = VTdata[,c(15)] == "0")
    freq(noCT[,c(27)])
    noCThadrecurrence <- subset(noCT, subset = noCT[,c(27)] == "1")
    mean(noCThadrecurrence[,c(29)]) # Mean time to VT recurrence
    sd(noCThadrecurrence[,c(29)]) # SD of time to recurrence
    mean(noCT[,c(42)])
    sd(noCT[,c(42)])  
    
  # What is recurrence when any imaging obtained
  anyimaging <- subset(VTdata, subset = VTdata[,c(11)] == "1" | VTdata[,c(15)] == "1") # Had any imaging
  View(anyimaging)
  freq(anyimaging[,c(27)])
  anyhadrecurrence <- subset(anyimaging, subset = anyimaging[,c(27)] == "1")
  mean(anyhadrecurrence[,c(29)]) # Mean time to VT recurrence
  sd(anyhadrecurrence[,c(29)]) # SD of time to recurrence
  mean(anyimaging[,c(42)])
  sd(anyimaging[,c(42)])
  
    # What is recurrence when no imaging obtained
    noimaging <- subset(VTdata, subset = VTdata[,c(11)] == "0" &  VTdata[,c(15)] == "0") # Had no imaging
    freq(noimaging[,c(27)])
    nonehadrecurrence <- subset(noimaging, subset = noimaging[,c(27)] == "1")
    mean(nonehadrecurrence[,c(29)]) # Mean time to VT recurrence
    sd(nonehadrecurrence[,c(29)]) # SD of time to recurrence
    mean(noimaging[,c(42)])
    sd(noimaging[,c(42)])
    
  # Stats for these
    # Those with MRI vs. Those without
      mri <- matrix(c(27, 47, 21, 34), byrow = T, ncol = 2, nrow = 2)
      colnames(mri) <- c("With cMRI", "W/o cMRI")
      rownames (mri) <- c("VT recurennce", "No VT Recurrance")
      mri
      chisq.test(mri)
      fisher.test(mri)
    # Those with CT vs. those without
      ct <- matrix(c(17, 57, 17, 38), byrow = T, ncol = 2, nrow = 2)
      colnames(ct) <- c("With cCT", "W/o cCT")
      rownames (ct) <- c("VT recurennce", "No VT Recurrance")
      ct
      chisq.test(ct)
      fisher.test(ct)
    # Those with any imaging vs. those with none
      any <- matrix(c(39, 35, 34, 21), byrow = T, ncol = 2, nrow = 2)
      colnames(any) <- c("With any imaging", "W/o any imaging")
      rownames (any) <- c("VT recurennce", "No VT Recurrance")
      any
      chisq.test(any)
      fisher.test(any)
    
    # Time to endpoint
    hadrecurrence <- subset(VTdata, subset = VTdata[,c(27)] == "1")
    # MRI vs no MRI
    MRIdone <- subset(VTdata, subset = VTdata[,c(11)] == "1")
    MRIhadrecurrence <- subset(MRIdone, subset = MRIdone[,c(27)] == "1")
    noMRI <- subset(VTdata, subset = VTdata[,c(11)] == "0")
    noMRIhadrecurrence <- subset(noMRI, subset = noMRI[,c(27)] == "1")
    shapiro.test(x = MRIhadrecurrence[,c(29)])
    shapiro.test(x = noMRIhadrecurrence[,c(29)])
    boxplot(MRIhadrecurrence[,c(29)], noMRIhadrecurrence[,c(29)])
    wilcox.test(x = MRIhadrecurrence[,c(29)], 
                y = noMRIhadrecurrence[,c(29)],
                mu = 0, 
                conf.int = T,
                paired = F)
    t.test(x = MRIhadrecurrence[,c(29)], 
           y = noMRIhadrecurrence[,c(29)],
           mu = 0, 
           conf.int = T,
           paired = F)
    # CT vs. no CT
    CTdone <- subset(VTdata, subset = VTdata[,c(15)] == "1") # Had a CT done
    CThadrecurrence <- subset(CTdone, subset = CTdone[,c(27)] == "1")
    noCT <- subset(VTdata, subset = VTdata[,c(15)] == "0")
    noCThadrecurrence <- subset(noCT, subset = noCT[,c(27)] == "1")
    shapiro.test(x = CThadrecurrence[,c(29)])
    shapiro.test(x = noCThadrecurrence[,c(29)])
    boxplot(CThadrecurrence[,c(29)], noCThadrecurrence[,c(29)])
    wilcox.test(x = CThadrecurrence[,c(29)], 
                y = noCThadrecurrence[,c(29)],
                mu = 0, 
                conf.int = T,
                paired = F)
    # Any imaging vs. no imaging
    anyimaging <- subset(VTdata, subset = VTdata[,c(11)] == "1" | VTdata[,c(15)] == "1") # Had any imaging
    anyhadrecurrence <- subset(anyimaging, subset = anyimaging[,c(27)] == "1")
    noimaging <- subset(VTdata, subset = VTdata[,c(11)] == "0" &  VTdata[,c(15)] == "0") # Had no imaging
    nonehadrecurrence <- subset(noimaging, subset = noimaging[,c(27)] == "1")
    shapiro.test(x = anyhadrecurrence[,c(29)])
    shapiro.test(x = nonehadrecurrence[,c(29)])
    boxplot(anyhadrecurrence[,c(29)], nonehadrecurrence[,c(29)])
    wilcox.test(x = anyhadrecurrence[,c(29)], 
                y = nonehadrecurrence[,c(29)],
                mu = 0, 
                conf.int = T,
                paired = F)
    t.test(x = anyhadrecurrence[,c(29)], 
           y = nonehadrecurrence[,c(29)],
           mu = 0, 
           conf.int = T,
           paired = F)
# RF time - Note you have to manually get rid of Na or . so double check before final run through
# Procedure duration - Note you have to manually get rid of Na or . so double check before final run through
  # For all
    RFall <- VTdata[,c(22)]
    RFall  
    mean(as.numeric(RFall[-c(12, 31, 111)]))
    sd(as.numeric(RFall[-c(12, 31, 111)]))
    proctimeall <- VTdata[,c(21)]
    mean(as.numeric(proctimeall[-c(12)]))  
    sd(as.numeric(proctimeall[-c(12)]))  
  # For MRI done
   MRIdone <- subset(VTdata, subset = VTdata[,c(11)] == "1")
     RFmridone <- MRIdone[,c(22)]
       RFmridone
          a <- as.numeric(RFmridone[-c(8)])
       mean(as.numeric(RFmridone[-c(8)])) # Manually replace NA or . here
       sd(as.numeric(RFmridone[-c(8)])) # Manually replace NA or . here
     proctimemridone <- MRIdone[,c(21)]
       mean(proctimemridone)  
       sd(proctimemridone)
          b <- as.numeric(proctimemridone)
  # For no MRI done
       noMRIdone <- subset(VTdata, subset = VTdata[,c(11)] == "0")
       RFnomridone <- noMRIdone[,c(22)]
       RFnomridone
       mean(as.numeric(RFnomridone[-c(7, 69)])) # Manually replace NA or . here
       sd(as.numeric(RFnomridone[-c(7, 69)])) # Manually replace NA or . here
          c <- as.numeric(RFnomridone[-c(7, 69)])
       proctimenomridone <- noMRIdone[,c(21)]
       proctimenomridone
       mean(as.numeric(proctimenomridone[-c(7)]))  
       sd(as.numeric(proctimenomridone[-c(7)]))
          d <- as.numeric(proctimenomridone[-c(7)])
       boxplot(a,c)
        t.test(a,c,mu = 0, paired = F) 
       boxplot(b,d)
        t.test(b,d,mu = 0, paired = F)   
      # For CT done
    CTdone <- subset(VTdata, subset = VTdata[,c(15)] == "1") # Had a CT done
      RFctdone <- CTdone[,c(22)]
       RFctdone
       mean(as.numeric(RFctdone[-c(6)])) # Manually replace NA or . here
       sd(as.numeric(RFctdone[-c(6)])) # Manually replace NA or . here
     proctimectdone <- CTdone[,c(21)]
       proctimectdone
       mean(proctimectdone[-c(6)]) # Manually replace NA or . here
       sd(proctimectdone[-c(6)]) # Manually replace NA or . here
    # For no imaging done
    noimaging <- subset(VTdata, subset = VTdata[,c(11)] == "0" &  VTdata[,c(15)] == "0") # Had no imaging
      RFnoimaging <- noimaging[,c(22)]
        RFnoimaging
        mean(as.numeric(RFnoimaging[-c(46)])) # Manually replace NA or . here
        sd(as.numeric(RFnoimaging[-c(46)])) # Manually replace NA or . here
      proctimenoimaging <- noimaging[,c(21)]
        mean(proctimenoimaging)
        sd(proctimenoimaging)
        
    # Stats for this graph
        # Time to endpoint
        
        mean(VTdata[,c(38)]) # Mean time to endpoint
        sd(VTdata[,c(38)]) # SD time to endpoint
        
        
        
        shapiro.test(x = imaging[,c(38)])
        shapiro.test(x = noimaging[,c(38)])
        boxplot(imaging[,c(38)], noimaging[,c(38)])
        wilcox.test(x = imaging[,c(38)], 
                    y = noimaging[,c(38)],
                    mu = 0, 
                    conf.int = T,
                    paired = F)

# Subgroup - first ablations vs repeat ablations
# Subgroup - repeats vs first timers
#####
dim(VTdata)
# Ischemic vs. nonischemic
# Subgroup analysis #1
ischemic <- subset(VTdata, subset = VTdata[,c(9)] == "1")
dim(ischemic)
View(ischemic)
nonischemic <- subset(VTdata, subset = VTdata[,c(9)] == "2")
dim(nonischemic)
View(nonischemic)
# Grab either first or repeats as VT data
  VTdata <- ischemic
  VTdata <- nonischemic

# individual vs repeats
# Subgroup analysis #2
first <- subset(VTdata, subset = VTdata[,c(6)] == "0")
  dim(first)
  View(first)
repeats <- subset(VTdata, subset = VTdata[,c(6)] == "1") # includes multiple repeat ablations
  dim(repeats)
  View(repeats)
# Grab either first or repeats as VT data
  VTdata <- first
  VTdata <- repeats
    
# Code to put your subgroup through
    # column 27 is recurrence yes/no, 29 is days to recurrence
    # Column 24 is procedure success, 1 is complete and 2 is partial
      # Number with both imaging 
      bothimaging <- subset(VTdata, subset = VTdata[,c(11)] == "1" &  VTdata[,c(15)] == "1") # Had no imaging
      dim(bothimaging)
    # For all
    freq(VTdata[,c(27)]) # Frequency of recurrence
    recurrence <- subset(VTdata, subset = VTdata[,c(27)] == "1") # Grab ones that recur
      dim(recurrence)
    daystorecurrence <- recurrence[,c(29)]
    mean(daystorecurrence)
    sd(daystorecurrence)
    freq(VTdata[,c(24)]) # Acute success
    
    # For all with cMRI
    MRIdone <- subset(VTdata, subset = VTdata[,c(11)] == "1") # Had an MRI done
    dim(MRIdone) # 1st value is total number that had MRIs done
    freq(MRIdone[,c(27)])
    recurrence <- subset(MRIdone, subset = MRIdone[,c(27)] == "1")
      dim(recurrence) # 1st value is total number of MRI's done that had recurrence
    daystorecurrence <- recurrence[,c(29)]
    mean(daystorecurrence)
    sd(daystorecurrence)
    freq(MRIdone[,c(24)]) # Acute success
    
    # For all with CT
    CTdone <- subset(VTdata, subset = VTdata[,c(15)] == "1") # Had a CT done
    dim(CTdone) # 1st value is total number that had CT done
    freq(CTdone[,c(27)])
    recurrence <- subset(CTdone, subset = CTdone[,c(27)] == "1")
      dim(recurrence) # 1st value is total number of MRI's done that had recurrence
    daystorecurrence <- recurrence[,c(29)]
    mean(daystorecurrence)
    sd(daystorecurrence)
    freq(CTdone[,c(24)]) # Acute success
    
    # For all with any imaging
    anyimaging <- subset(VTdata, subset = VTdata[,c(11)] == "1" | VTdata[,c(15)] == "1") # Had any imaging  
    recurrence <- subset(anyimaging, subset = anyimaging[,c(27)] == "1")
    dim(recurrence)
    daystorecurrence <- recurrence[,c(29)]
    mean(daystorecurrence)
    sd(daystorecurrence)
    freq(anyimaging[,c(24)]) # Acute success
    
    # For all without any imaging
    noimaging <- subset(VTdata, subset = VTdata[,c(11)] == "0" &  VTdata[,c(15)] == "0") # Had no imaging
    freq(noimaging[,c(27)])
    recurrence <- subset(noimaging, subset = noimaging[,c(27)] == "1")
    dim(recurrence)
    daystorecurrence <- recurrence[,c(29)]
    mean(daystorecurrence)
    sd(daystorecurrence)
    freq(noimaging[,c(24)])
    
#####
# Statistics for subgroups
  
  # Statistics for ischemic vs. nonischemic
    ischemic <- subset(VTdata, subset = VTdata[,c(9)] == "1")
    dim(ischemic)
    View(ischemic)
    nonischemic <- subset(VTdata, subset = VTdata[,c(9)] == "2")
    dim(nonischemic)
    View(nonischemic)
    
    # Ischemic with and without MRI
      ischemicmri <- subset(ischemic, subset = ischemic[,c(11)] == "1")
      ischemicnomri <- subset(ischemic, subset = ischemic[,c(11)] == "0")
    # nonischemic with and without MRI
      nonischemicmri <- subset(nonischemic, subset = nonischemic[,c(11)] == "1")
      nonischemicnomri <- subset(nonischemic, subset = nonischemic[,c(11)] == "0")
  
    # For all
    freq(VTdata[,c(27)]) # Frequency of recurrence
    freq(ischemic[,c(27)])
    freq(nonischemic[,c(27)])
    freq(ischemicmri[,c(27)])
    freq(ischemicnomri[,c(27)])
    # Build 2x2 table of recurrences
    recurischemic <- matrix(c(
      sum(ischemicmri[,c(27)]),
      sum(ischemicnomri[,c(27)]),
      nrow(ischemicmri) - sum(ischemicmri[,c(27)]),
      nrow(ischemicnomri) - sum(ischemicnomri[,c(27)])
    ), byrow = T, ncol = 2, nrow = 2)
    colnames(recurischemic) <- c("MRI", "no MRI")
    rownames(recurischemic) <- c("Recurrence", "No recurrence")
    recurischemic
    chisq.test(recurischemic)
    fisher.test(recurischemic)
    # Build vectors of days to recurrence
    recurrenceischemicmri <- subset(ischemicmri, subset = ischemicmri[,c(27)] == "1") # Grab ones that recur
    daystorecurrenceischemicmri <- recurrenceischemicmri[,c(29)]
    mean(daystorecurrenceischemicmri)
    sd(daystorecurrenceischemicmri)
    recurrenceischemicnomri <- subset(ischemicnomri, subset = ischemicnomri[,c(27)] == "1") # Grab ones that recur
    daystorecurrenceischemicnomri <- recurrenceischemicnomri[,c(29)]
    mean(daystorecurrenceischemicnomri)
    sd(daystorecurrenceischemicnomri)
    # Compare vectors of days to recurrence
    shapiro.test(daystorecurrenceischemicmri)
    shapiro.test(daystorecurrenceischemicnomri)  
    boxplot(daystorecurrenceischemicmri, daystorecurrenceischemicnomri)
    wilcox.test(x = daystorecurrenceischemicmri, 
                y = daystorecurrenceischemicnomri,
                mu = 0, 
                conf.int = T,
                paired = F)
    t.test(x = daystorecurrenceischemicmri, 
           y = daystorecurrenceischemicnomri,
           mu = 0, 
           conf.int = T,
           paired = F)
    # Compare acute success
    freq(ischemicmri[,c(24)])
    freq(ischemicnomri[,c(24)])
    acutesucessischemic <- matrix(c(12, 27, 4, 9), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessischemic) <- c("MRI", "no MRI")
    acutesucessischemic
    chisq.test(acutesucessischemic)
    fisher.test(acutesucessischemic)
    # For just acute sucess
    acutesucessischemic <- matrix(c(10, 17, 6, 19), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessischemic) <- c("MRI", "no MRI")
    acutesucessischemic
    chisq.test(acutesucessischemic)
    fisher.test(acutesucessischemic)
    
  # nonischemic
    freq(nonischemicmri[,c(27)])
    freq(nonischemicnomri[,c(27)])
    # Build 2x2 table of recurrences
    recurnonischemic <- matrix(c(
      sum(nonischemicmri[,c(27)]),
      sum(nonischemicnomri[,c(27)]),
      nrow(nonischemicmri) - sum(nonischemicmri[,c(27)]),
      nrow(nonischemicnomri) - sum(nonischemicnomri[,c(27)])
    ), byrow = T, ncol = 2, nrow = 2)
    colnames(recurnonischemic) <- c("MRI", "no MRI")
    rownames(recurnonischemic) <- c("Recurrence", "No recurrence")
    recurnonischemic
    chisq.test(recurnonischemic)
    fisher.test(recurnonischemic)
    # Build vectors of days to recurrence
    recurrencenonischemicmri <- subset(nonischemicmri, subset = nonischemicmri[,c(27)] == "1") # Grab ones that recur
    daystorecurrencenonischemicmri <- recurrencenonischemicmri[,c(29)]
    mean(daystorecurrencenonischemicmri)
    sd(daystorecurrencenonischemicmri)
    recurrencenonischemicnomri <- subset(nonischemicnomri, subset = nonischemicnomri[,c(27)] == "1") # Grab ones that recur
    daystorecurrencenonischemicnomri <- recurrencenonischemicnomri[,c(29)]
    mean(daystorecurrencenonischemicnomri)
    sd(daystorecurrencenonischemicnomri)
    # Compare vectors of days to recurrence
    shapiro.test(daystorecurrencenonischemicmri)
    shapiro.test(daystorecurrencenonischemicnomri)  
    boxplot(daystorecurrencenonischemicmri, daystorecurrencenonischemicnomri)
    wilcox.test(x = daystorecurrencenonischemicmri, 
                y = daystorecurrencenonischemicnomri,
                mu = 0, 
                conf.int = T,
                paired = F)
    t.test(x = daystorecurrencenonischemicmri, 
           y = daystorecurrencenonischemicnomri,
           mu = 0, 
           conf.int = T,
           paired = F)
    # Compare acute success
    freq(nonischemicmri[,c(24)])
    freq(nonischemicnomri[,c(24)])
    acutesucessnonischemic <- matrix(c(21, 34, 5, 4), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessnonischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessnonischemic) <- c("MRI", "no MRI")
    acutesucessnonischemic
    chisq.test(acutesucessnonischemic)
    fisher.test(acutesucessnonischemic)
    # For just acute sucess
    acutesucessnonischemic <- matrix(c(14, 28, 12, 10), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessnonischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessnonischemic) <- c("MRI", "no MRI")
    acutesucessnonischemic
    chisq.test(acutesucessnonischemic)
    fisher.test(acutesucessnonischemic)
    
###    
    # Ischemic vs nonischemic CTs
    # Ischemicct
    cct3mo <- (VTdata[,c(56)] < 94) * VTdata[,c(53)]
    ischemic <- subset(VTdata, subset = VTdata[,c(9)] == "1")
    nonischemic <- subset(VTdata, subset = VTdata[,c(9)] == "2")
    View(ischemic)
    
    # ischemic with and without CT
    ischemicct <- subset(ischemic, subset = ischemic[,c(15)] == "1")
    ischemicnoct <- subset(ischemic, subset = ischemic[,c(15)] == "0")
    View(ischemicct)
    View(ischemicnoct)
    dim(ischemicct)
    dim(ischemicnoct)
    
    # nonischemic with and without CT
    nonischemicct <- subset(nonischemic, subset = nonischemic[,c(15)] == "1")
    nonischemicnoct <- subset(nonischemic, subset = nonischemic[,c(15)] == "0")
    View(nonischemicct)
    View(nonischemicnoct)
    dim(nonischemicnoct)
    
    
    # Ischemic with and without MRI
    ischemicmri <- subset(ischemic, subset = ischemic[,c(11)] == "1")
    ischemicnomri <- subset(ischemic, subset = ischemic[,c(11)] == "0")
    # nonischemic with and without MRI
    nonischemicmri <- subset(nonischemic, subset = nonischemic[,c(11)] == "1")
    nonischemicnomri <- subset(nonischemic, subset = nonischemic[,c(11)] == "0")
    
    # VT recurrence
    freq(ischemic[,c(27)])
    freq(nonischemic[,c(27)])
    freq(ischemicct[,c(27)])
    freq(ischemicnoct[,c(27)])
    
    freq(nonischemic[,c(27)])
    freq(nonischemicct[,c(27)])
    freq(nonischemicnoct[,c(27)])
    
    recurischemic <- matrix(c(
      sum(ischemicct[,c(27)]),
      sum(ischemicnoct[,c(27)]),
      nrow(ischemicct) - sum(ischemicct[,c(27)]),
      nrow(ischemicnoct) - sum(ischemicnoct[,c(27)])
    ), byrow = T, ncol = 2, nrow = 2)
    colnames(recurischemic) <- c("ct", "no ct")
    rownames(recurischemic) <- c("Recurrence", "No recurrence")
    recurischemic
    chisq.test(recurischemic)
    fisher.test(recurischemic)
    
    recurnonischemic <- matrix(c(
      sum(nonischemicct[,c(27)]),
      sum(nonischemicnoct[,c(27)]),
      nrow(nonischemicct) - sum(nonischemicct[,c(27)]),
      nrow(nonischemicnoct) - sum(nonischemicnoct[,c(27)])
    ), byrow = T, ncol = 2, nrow = 2)
    colnames(recurnonischemic) <- c("ct", "no ct")
    rownames(recurnonischemic) <- c("Recurrence", "No recurrence")
    recurnonischemic
    chisq.test(recurnonischemic)
    fisher.test(recurnonischemic)
    
    # Acute success
    # Ischemic with and without CT
    freq(ischemicct[,c(24)])
    freq(ischemicnoct[,c(24)])
    acutesucessischemic <- matrix(c(9, 5, 18, 20), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessischemic) <- c("CT", "no CT")
    acutesucessischemic
    chisq.test(acutesucessischemic)
    fisher.test(acutesucessischemic)
    
    # nonischemic with and without CT
    freq(nonischemicct[,c(24)])
    freq(nonischemicnoct[,c(24)])
    acutesucessnonischemic <- matrix(c(8, 9, 34, 13), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessnonischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessnonischemic) <- c("CT", "no CT")
    acutesucessnonischemic
    chisq.test(acutesucessnonischemic)
    fisher.test(acutesucessnonischemic)
    
    ##
    # Time to endpoint
    summary(ischemicct[,c(42)])
    summary(ischemicnoct[,c(42)])
    summary(nonischemicct[,c(42)])
    summary(nonischemicnoct[,c(42)])
    
    summary(ischemicmri[,c(42)])
    summary(ischemicnomri[,c(42)])
    summary(nonischemicmri[,c(42)])
    summary(nonischemicnomri[,c(42)])
    
    
    
#####
    # Statistics for subgroup analysis #2 - first time ablations vs. repeats
    first <- subset(VTdata, subset = VTdata[,c(6)] == "0")
    dim(first)
    View(first)
    repeats <- subset(VTdata, subset = VTdata[,c(6)] == "1") # includes multiple repeat ablations
    dim(repeats)
    View(repeats)
    
    # First with and without MRI
    firstmri <- subset(first, subset = first[,c(11)] == "1")
    firstnomri <- subset(first, subset = first[,c(11)] == "0")
    # repeats with and without MRI
    repeatmri <- subset(repeats, subset = repeats[,c(11)] == "1")
    repeatnomri <- subset(repeats, subset = repeats[,c(11)] == "0")
    
    # For all
    freq(firstmri[,c(27)])
    freq(firstnomri[,c(27)])
    # Build 2x2 table of recurrences
    firstrecur <- matrix(c(
      sum(firstmri[,c(27)]),
      sum(firstnomri[,c(27)]),
      nrow(firstmri) - sum(firstmri[,c(27)]),
      nrow(firstnomri) - sum(firstnomri[,c(27)])
    ), byrow = T, ncol = 2, nrow = 2)
    colnames(firstrecur) <- c("MRI", "no MRI")
    rownames(firstrecur) <- c("Recurrence", "No recurrence")
    firstrecur
    chisq.test(firstrecur)
    fisher.test(firstrecur)
    # Build vectors of days to recurrence
    recurrencefirstmri <- subset(firstmri, subset = firstmri[,c(27)] == "1") # Grab ones that recur
    daystorecurrencefirstmri <- recurrencefirstmri[,c(29)]
    mean(daystorecurrencefirstmri)
    sd(daystorecurrencefirstmri)
    recurrencefirstnomri <- subset(firstnomri, subset = firstnomri[,c(27)] == "1") # Grab ones that recur
    daystorecurrencefirstnomri <- recurrencefirstnomri[,c(29)]
    mean(daystorecurrencefirstnomri)
    sd(daystorecurrencefirstnomri)
    # Compare vectors of days to recurrence
    shapiro.test(daystorecurrencefirstmri)
    shapiro.test(daystorecurrencefirstnomri)  
    boxplot(daystorecurrencefirstmri, daystorecurrencefirstnomri)
    wilcox.test(x = daystorecurrencefirstmri, 
                y = daystorecurrencefirstnomri,
                mu = 0, 
                conf.int = T,
                paired = F)
    t.test(x = daystorecurrencefirstmri, 
           y = daystorecurrencefirstnomri,
           mu = 0, 
           conf.int = T,
           paired = F)
    # Compare acute success
    freq(firstmri[,c(24)])
    freq(firstnomri[,c(24)])
    acutesucessischemic <- matrix(c(27, 48, 8, 9), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessischemic) <- c("MRI", "no MRI")
    acutesucessischemic
    chisq.test(acutesucessischemic)
    fisher.test(acutesucessischemic)
    # For just acute sucess
    acutesucessischemic <- matrix(c(19, 34, 16, 23), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucessischemic) <- c("Acute success", "Not accute success")
    colnames(acutesucessischemic) <- c("MRI", "no MRI")
    acutesucessischemic
    chisq.test(acutesucessischemic)
    fisher.test(acutesucessischemic)
    
  # repeats with and without MRI
    repeatmri <- subset(repeats, subset = repeats[,c(11)] == "1")
    repeatnomri <- subset(repeats, subset = repeats[,c(11)] == "0")
    # For all
    freq(repeatmri[,c(27)])
    freq(repeatnomri[,c(27)])
    # Build 2x2 table of recurrences
    repeatrecur <- matrix(c(
      sum(repeatmri[,c(27)]),
      sum(repeatnomri[,c(27)]),
      nrow(repeatmri) - sum(repeatmri[,c(27)]),
      nrow(repeatnomri) - sum(repeatnomri[,c(27)])
    ), byrow = T, ncol = 2, nrow = 2)
    colnames(repeatrecur) <- c("MRI", "no MRI")
    rownames(repeatrecur) <- c("Recurrence", "No recurrence")
    repeatrecur
    chisq.test(repeatrecur)
    fisher.test(repeatrecur)
    # Build vectors of days to recurrence
    recurrencerepeatmri <- subset(repeatmri, subset = repeatmri[,c(27)] == "1") # Grab ones that recur
    daystorecurrencerepeatmri <- recurrencerepeatmri[,c(29)]
    mean(daystorecurrencerepeatmri)
    sd(daystorecurrencerepeatmri)
    recurrencerepeatnomri <- subset(repeatnomri, subset = repeatnomri[,c(27)] == "1") # Grab ones that recur
    daystorecurrencerepeatnomri <- recurrencerepeatnomri[,c(29)]
    mean(daystorecurrencerepeatnomri)
    sd(daystorecurrencerepeatnomri)
    # Compare vectors of days to recurrence
    shapiro.test(daystorecurrencerepeatmri)
    shapiro.test(daystorecurrencerepeatnomri)  
    boxplot(daystorecurrencerepeatmri, daystorecurrencerepeatnomri)
    wilcox.test(x = daystorecurrencerepeatmri, 
                y = daystorecurrencerepeatnomri,
                mu = 0, 
                conf.int = T,
                paired = F)
    t.test(x = daystorecurrencerepeatmri, 
           y = daystorecurrencerepeatnomri,
           mu = 0, 
           conf.int = T,
           paired = F)
    # Compare acute success
    freq(repeatmri[,c(24)])
    freq(repeatnomri[,c(24)])
    acutesucess <- matrix(c(12, 20, 1, 4), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucess) <- c("Acute success", "Not accute success")
    colnames(acutesucess) <- c("MRI", "no MRI")
    acutesucess
    chisq.test(acutesucess)
    fisher.test(acutesucess)
    # For just acute sucess
    acutesucess <- matrix(c(11, 16, 2, 8), byrow = T, ncol = 2, nrow = 2)
    rownames(acutesucess) <- c("Acute success", "Not accute success")
    colnames(acutesucess) <- c("MRI", "no MRI")
    acutesucess
    chisq.test(acutesucess)
    fisher.test(acutesucess)
#####
# Kaplan Meier curves
# Column 11 is MRI
# Column 15 is CT
# Column 27 is recurrence
# Column 29 is time to VT
library(survival)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(survminer) # Can make nicer kaplan meier plots than base R

anyimaging <- VTdata[,c(41)] + 1    
anyimaging
?Surv
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ anyimaging, data = VTdata)
km.model1
summary(km.model1)

plot(km.model1,
     conf.int = F,
     xlab = "Follow up (Days)", 
     #ylab = "% without VT Recurrence", 
     main = "Survival free of VT recurrence, HTx, or death",
     mark.time = TRUE, # Tells you when things are censored
     las = 1,
     col = c("black", "gray"),
     lwd =2,
     
     ) # rotates y axis values
legend(18, 0.35,
  legend=c("No imaging", "Imaging"),
  lty=1,
  lwd=2,
  col=c("black", "gray"), bty="", cex = 0.9
)
# Log rank test H0 - survival is the same; Ha - survival between two groups isnt
# AKA mantle hansel method
survdiff(Surv(time = VTdata[,c(42)], event = VTdata[,c(43)]) ~ VTdata[,c(41)])
  
x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "CT and/or cMRI",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, HTx, or death",
           censor = TRUE,
           palette = c("black", "grey")
          
            #xscale
            #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
          
           
           )

kmcox <- coxph(Surv(time = VTdata[,c(42)],event = VTdata[,c(43)]) ~ VTdata[,c(41)], data = VTdata)
summary(kmcox)

# Log rank test H0 - survival is the same; Ha - survival between two groups isnt
# AKA mantle hansel method
survdiff(Surv(VTdata[,c(42)],VTdata[,c(43)]) ~ VTdata[,c(41)])

# Survival analysis for MRI vs. no MRI
MRI <- VTdata[,c(11)] + 1    
MRI
?Surv
km.model2 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ MRI, data = VTdata)
km.model2
summary(km.model2)

x11()
ggsurvplot(km.model2,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "CMR",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, HTx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           #xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
           
           
)

# Survival analysis for CT vs. no CT
CT <- VTdata[,c(15)] + 1    
CT
?Surv
km.model3 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ CT, data = VTdata)
km.model3
summary(km.model3)

x11()
ggsurvplot(km.model3,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "CT",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, HTx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           #xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
           
           
)

# Survival analysis for time to VT recurrence only
anyimaging <- VTdata[,c(41)] + 1    
anyimaging
?Surv
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ anyimaging, data = VTdata)
km.model1
summary(km.model1)

Viewx11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Imaging",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           #xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

MRI <- VTdata[,c(11)] + 1    
MRI
?Surv
km.model2 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ MRI, data = VTdata)
km.model2
summary(km.model2)

x11()
ggsurvplot(km.model2,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "CMR",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           #xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

CT <- VTdata[,c(15)] + 1    
CT
?Surv
km.model3 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ CT, data = VTdata)
km.model3
summary(km.model3)

x11()
ggsurvplot(km.model3,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "CT",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           #xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

# Table 1 - time to last follow up for MRI vs no MRI\

# Need to make a new event column
MRI <- VTdata[,c(11)] + 1 
death <- VTdata[]
?Surv
noevents <- rep(1, times = 129)
noevents
length(VTdata[,c(38)])

deathortransplant <- VTdata[,c(35)] + VTdata[,c(39)]
deathortransplant
km.model7 <- survfit(Surv(time = VTdata[,c(38)] , event = noevents) ~ MRI, data = VTdata)
km.model7
summary(km.model7)

x11()
ggsurvplot(km.model7,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "CT",
           legend.labs = c("No", "Yes"),
           title="Time to followup/death/Tx",
           censor = TRUE,
           palette = c("black", "grey")
           
           #xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
#####
# 4/2/2023
  
  # Log rank for those with any MRI

    # For NICM
      VTdata <- subset(VTdata, subset = VTdata[,c(9)] == "2") # Grabs nonischemic
      VTdata <- subset(VTdata, subset = VTdata[,c(9)] == "2" | VTdata[,c(9)] == "3") # Grabs nonischemic and mixed
      nonischemic <- subset(VTdata, subset = VTdata[,c(9)] == "2") # Grabs nonischemic
      str(nonischemic)
      View(nonischemic)
      dim(nonischemic)
      dim(VTdata)
    # Grab for ICM
      ischemic <- subset(VTdata, subset = VTdata[,c(9)] == "1") # grabs ischemic
      View(ischemic)
      VTdata <- subset(VTdata, subset = VTdata[,c(9)] == "1") # grabs ischemic
      VTdata <- subset(VTdata, subset = VTdata[,c(9)] == "1" | VTdata[,c(9)] == "3") # grabs ischemic with mixed
    # Grab for repeat ablations
      repeats <- subset(VTdata, subset = VTdata[,c(6)] == "1") # includes multiple repeat ablations
      VTdata <- subset(VTdata, subset = VTdata[,c(6)] == "1") # includes multiple repeat ablations  
      dim(VTdata)
    # Grab for first time ablations
      first <- subset(VTdata, subset = VTdata[,c(6)] == "0") # Grabs firsttime ablations
      VTdata <- subset(VTdata, subset = VTdata[,c(6)] == "0") # Grabs firsttime ablations
      
    
    
      # For triple endpoint
            a <-  VTdata[,c(48)]
            b <- replace(a, is.na(a), 0)
            c <- b[b > 0]
            mean(c)
            sd(c)
          # For any time
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ VTdata[,c(45)], data = VTdata)
            km.model1
            summary(km.model1)

            x11()
            ggsurvplot(km.model1,
              risk.table = TRUE,
              pval=TRUE,
              pval.method = TRUE,
              legend.title = "Cardiac MRI anytime",
              legend.labs = c("No", "Yes"),
              title="Survival free of VT recurrence, heart transplant, or death",
              censor = TRUE,
              palette = c("black", "grey")
           
              # xscale
              #xlim = c(0,500)
              #cumevents = TRUE,cumcensor = TRUE
                  )
            a <- subset(VTdata, subset = VTdata[,c(45)] == 1)
            freq(a[,c(24)]) # acute success
            freq(a[,c(27)]) # recurrence
            freq(a[,c(43)]) # Endpoint
            
            mri1month <- (VTdata[,c(48)] < 32)
            mri1month <- (VTdata[,c(48)] < 32) * VTdata[,c(45)]
            mri1month
            test <- replace(mri1month, is.na(mri1month), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last month",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, transplant, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
           a <- subset(VTdata, subset = VTdata[,c(48)] <32)
           freq(a[,c(24)]) # acute success
           freq(a[,c(27)]) # recurrence
           freq(a[,c(43)]) # Endpoint
# Use for AHA- ********************************************************  
            mri3months <- (VTdata[,c(48)] < 94) #* VTdata[,c(46)]
            mri3months
            test <- replace(mri3months, is.na(mri3months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            View(km.model1)
            x11()
            p1 <- ggsurvplot(km.model1,
                       surv.scale = "percent",
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       xlab = c("Years"),
                       ylab = element_blank(),
                       title="Survival free of VT recurrence, Tx, or death",
                       legend.labs = c("No", "Yes"),
                       legend.title = c("Cardiac MRI within 3 months"),
                       xscale = c("d_y"),
                       break.x.by = 365.25 * 0.5,
                       risk.table.title = c("At Risk"),
                       risk.table.fontsize = 4,
                       risk.table.y.text = FALSE,
                       risk.table.y.text.col = TRUE,
                       tables.y.text = FALSE,
                       tables.height = 0.15,
                       censor = TRUE,
                       font.x = 12,
                       font.y = 12,
                       font.tickslab = 16,
                      
                       palette = c("black", "grey"),
                       )
    
            a <- subset(VTdata, subset = VTdata[,c(48)] <93)
            freq(a[,c(24)]) # acute success
            freq(a[,c(27)]) # recurrence
            freq(a[,c(43)]) # Endpoint

            mri6months <- (VTdata[,c(48)] < 184) * VTdata[,c(45)]
            mri6months
            test <- replace(mri6months, is.na(mri6months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 6 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, transplant, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            a <- subset(VTdata, subset = VTdata[,c(48)] <184)
            freq(a[,c(24)]) # acute success
            freq(a[,c(27)]) # recurrence
            freq(a[,c(43)]) # Endpoint

            mri1year <- (VTdata[,c(48)] < 366) * VTdata[,c(45)]
            mri1year
            test <- replace(mri1year, is.na(mri1year), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last year",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, Tx, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            a <- subset(VTdata, subset = VTdata[,c(48)] <366)
            freq(a[,c(24)]) # acute success
            freq(a[,c(27)]) # recurrence
            freq(a[,c(43)]) # Endpoint
            
            mri18months <- (VTdata[,c(48)] < 549) * VTdata[,c(45)]
            mri18months
            test <- replace(mri18months, is.na(mri18months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 3 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, Tx, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )

      # For just VT recurrence
            mri1month <- (VTdata[,c(48)] < 32) * VTdata[,c(45)]
            mri1month
            test <- replace(mri1month, is.na(mri1month), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last month",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            
            mri3months <- (VTdata[,c(48)] < 93) * VTdata[,c(45)]
            mri3months
            test <- replace(mri3months, is.na(mri3months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 3 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )

            mri6months <- (VTdata[,c(48)] < 184) * VTdata[,c(45)]
            mri6months
            test <- replace(mri6months, is.na(mri6months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 6 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrences",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            
            mri1year <- (VTdata[,c(48)] < 366) * VTdata[,c(45)]
            mri1year
            test <- replace(mri1year, is.na(mri1year), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last year",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            
            mri18months <- (VTdata[,c(48)] < 549) * VTdata[,c(45)]
            mri18months
            test <- replace(mri18months, is.na(mri18months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 3 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, Tx, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
          # For any time
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ VTdata[,c(45)], data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI anytime",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            
        
#####
  # Log rank for any MRI's read at Mayo only
    # For any time
      # For triple endpoint
            mrimayo <- VTdata[,c(46)]
            mrimayo
            test <- replace(mrimayo, is.na(mrimayo), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI anytime",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.17
            mri1month <- (VTdata[,c(48)] < 32) * VTdata[,c(46)]
            mri1month
            test <- replace(mri1month, is.na(mri1month), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last month",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, transplant, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.69
            mri3months <- (VTdata[,c(48)] < 94) #* VTdata[,c(46)]
            mri3months
            test <- replace(mri3months, is.na(mri3months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 3 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, Tx, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.068
            mri6months <- (VTdata[,c(48)] < 184) * VTdata[,c(46)]
            mri6months
            test <- replace(mri6months, is.na(mri6months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 6 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, transplant, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.34
            mri1year <- (VTdata[,c(48)] < 366) * VTdata[,c(46)]
            mri1year
            test <- replace(mri1year, is.na(mri1year), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last year",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, Tx, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.068
            
            
            mri18months <- (VTdata[,c(48)] < 549) * VTdata[,c(45)]
            mri18months
            test <- replace(mri18months, is.na(mri18months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 3 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, Tx, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
        # For NICM
        # For ICM
      # For just VT recurrence
            mri1month <- (VTdata[,c(48)] < 32) * VTdata[,c(46)]
            mri1month
            test <- replace(mri1month, is.na(mri1month), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last month",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.08
            
            mri3months <- (VTdata[,c(48)] < 93) * VTdata[,c(46)]
            mri3months
            test <- replace(mri3months, is.na(mri3months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 3 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.034
            mri6months <- (VTdata[,c(48)] < 184) * VTdata[,c(46)]
            mri6months
            VTdata[,c(48)]
            test <- replace(mri6months, is.na(mri6months), 0)
            test
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 6 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrences",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.057
            
            mri1year <- (VTdata[,c(48)] < 366) * VTdata[,c(46)]
            mri1year
            test <- replace(mri1year, is.na(mri1year), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last year",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            # p = 0.068
            
            mri18months <- (VTdata[,c(48)] < 549) * VTdata[,c(45)]
            mri18months
            test <- replace(mri18months, is.na(mri18months), 0)
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI last 3 months",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence, Tx, or death",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
            test <- replace(VTdata[,c(46)], is.na(VTdata[,c(46)]), 0)
            test
            km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
            km.model1
            summary(km.model1)
            
            x11()
            ggsurvplot(km.model1,
                       risk.table = TRUE,
                       pval=TRUE,
                       pval.method = TRUE,
                       legend.title = "Cardiac MRI anytime",
                       legend.labs = c("No", "Yes"),
                       title="Survival free of VT recurrence",
                       censor = TRUE,
                       palette = c("black", "grey")
                       
                       # xscale
                       #xlim = c(0,500)
                       #cumevents = TRUE,cumcensor = TRUE
            )
        # For NICM
        # For ICM
   

old <- VTdata[,c(11)]
old
new <- VTdata[,c(48)] < 93
newnew <- replace(new, is.na(new),0)
newnew

old == newnew
#####

# Looking at cardiac CTs - for VT recurrence, death, or transplant
cctever <- (VTdata[,c(53)])
cctever
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ cctever, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT ever",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(53)] == 1)
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

cct1mo <- (VTdata[,c(56)] < 32) * VTdata[,c(53)]
cct1mo
test <- replace(cct1mo, is.na(cct1mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last month",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, transplant, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(56)] <32)
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint
# Use for AHA- ******************************************************** 
cct3mo <- (VTdata[,c(56)] < 94) * VTdata[,c(53)]
cct3mo
test <- replace(cct3mo, is.na(cct3mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
p2 <- ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 3 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(56)] <93)
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

cct6mo <- (VTdata[,c(56)] < 184) * VTdata[,c(53)]
cct6mo
test <- replace(cct6mo, is.na(cct6mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 6 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, transplant, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(56)] <184)
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

cct1yr <- (VTdata[,c(56)] < 366) * VTdata[,c(53)]
cct1yr
test <- replace(cct1yr, is.na(cct1yr), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last year",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(56)] <366)
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

# For just VT recurrence
cct1mo <- (VTdata[,c(56)] < 32) * VTdata[,c(53)]
cct1mo
test <- replace(cct1mo, is.na(cct1mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last month",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct3mo <- (VTdata[,c(56)] < 93) * VTdata[,c(53)]
cct3mo
test <- replace(cct3mo, is.na(cct3mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 3 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct6mo <- (VTdata[,c(56)] < 184) * VTdata[,c(53)]
cct6mo
test <- replace(cct6mo, is.na(cct6mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 6 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrences",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct1yr <- (VTdata[,c(56)] < 366) * VTdata[,c(53)]
cct1yr
test <- replace(cct1yr, is.na(cct1yr), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last year",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
# Ever had cardiac CT
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ VTdata[,c(53)], data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI anytime",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)






#####
# For any cardiac CT or cardiac angiogram

# Unable to code this yet - need a column of whichever is lower of time to recurrence in CTA or cCT - easier to jsut add it in


anyCTA <- (VTdata[,c(52)])
anyCTA
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ anyCTA, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "CT Angiogram (cardiac or chest) ever",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct1mo <- (VTdata[,c(56)] < 32) * VTdata[,c(53)]
cct1mo
test <- replace(cct1mo, is.na(cct1mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last month",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, transplant, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct3mo <- (VTdata[,c(56)] < 93) * VTdata[,c(53)]
cct3mo
test <- replace(cct3mo, is.na(cct3mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 3 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct6mo <- (VTdata[,c(56)] < 184) * VTdata[,c(53)]
cct6mo
test <- replace(cct6mo, is.na(cct6mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 6 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, transplant, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct1yr <- (VTdata[,c(56)] < 366) * VTdata[,c(53)]
cct1yr
test <- replace(cct1yr, is.na(cct1yr), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last year",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

# For just VT recurrence
cct1mo <- (VTdata[,c(56)] < 32) * VTdata[,c(53)]
cct1mo
test <- replace(cct1mo, is.na(cct1mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last month",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct3mo <- (VTdata[,c(56)] < 93) * VTdata[,c(53)]
cct3mo
test <- replace(cct3mo, is.na(cct3mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 3 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct6mo <- (VTdata[,c(56)] < 184) * VTdata[,c(53)]
cct6mo
test <- replace(cct6mo, is.na(cct6mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 6 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrences",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

cct1yr <- (VTdata[,c(56)] < 366) * VTdata[,c(53)]
cct1yr
test <- replace(cct1yr, is.na(cct1yr), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last year",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
# Ever had cardiac CT
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ VTdata[,c(53)], data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI anytime",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)








#####
# For any cardiac CT or cardiac MRI

# For any time
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ VTdata[,c(50)], data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI/CT ever",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, heart transplant, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(50)] == 1)
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

onemonth <- VTdata[,c(48)] <32 | VTdata[,c(56)] <32
test <- replace(onemonth, is.na(onemonth), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI/Ct last month",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, transplant, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(48)] <32 | VTdata[,c(56)] <32 )
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

threemonths <- VTdata[,c(48)] < 93 | VTdata[,c(56)] < 93
threemonths
test <- replace(threemonths, is.na(threemonths), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = FALSE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT last 3 months",
           xlab = "Time to endpoint (days)",
           #xscale = "d_m",
           surv.scale = "percent",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey"),
           pval.size = 5,
           
           #pval.coord = c(2, 2)
           #xlim = c(0,1000)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(48)] <93 | VTdata[,c(56)] <93 )
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

sixmonths <- VTdata[,c(48)] < 184 | VTdata[,c(56)] < 184
sixmonths
test <- replace(sixmonths, is.na(sixmonths), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT last 6 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, transplant, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(48)] <184 |  VTdata[,c(56)] <184 )
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint

oneyear <- VTdata[,c(48)] < 366 | VTdata[,c(56)] < 366
oneyear
test <- replace(oneyear, is.na(oneyear), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT last year",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)
a <- subset(VTdata, subset = VTdata[,c(48)] <366 |  VTdata[,c(56)] <366 )
freq(a[,c(24)]) # acute success
freq(a[,c(27)]) # recurrence
freq(a[,c(43)]) # Endpoint


# For just VT recurrence
onemonth <- VTdata[,c(48)] < 32 | VTdata[,c(56)] <32
onemonth
test <- replace(onemonth, is.na(onemonth), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT last month",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

threemonths <- VTdata[,c(48)] < 93 | VTdata[,c(56)] < 93
threemonths
test <- replace(threemonths, is.na(threemonths), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT last 3 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

sixmonths <- VTdata[,c(48)] < 184 | VTdata[,c(56)] < 184
sixmonths
test <- replace(sixmonths, is.na(sixmonths), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT last 6 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrences",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

oneyear <- VTdata[,c(48)] < 366 | VTdata[,c(56)] < 366
oneyear
test <- replace(oneyear, is.na(oneyear), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT last year",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

# For any time
km.model1 <- survfit(Surv(time = VTdata[,c(49)] , event = VTdata[,c(27)]) ~ VTdata[,c(50)], data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI or CT anytime",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)




#####
#playing with regression/models
ac <- (VTdata[,c(24)] == "1")* VTdata[,c(24)]

library(tidyverse)
# Linear model
model <- lm(formula = VTdata[,c(27)] ~ VTdata[,c(2)] + VTdata[,c(6)] # VT recurrence, age, redo
            + VTdata[,c(9)] + VTdata[,c(11)] + VTdata[,c(15)] + VTdata[,c(20)] # type of CM, LGE, CT, EF
            + VTdata[,c(23)] + VTdata[,c(24)] # Epi, Acute success
            ,data = VTdata
            )
summary(model)
# Logistic
model <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(2)] + VTdata[,c(6)] # VT recurrence, age, redo
            + VTdata[,c(9)] + VTdata[,c(11)] + VTdata[,c(15)] + VTdata[,c(20)] # type of CM, LGE, CT, EF
            + VTdata[,c(23)] + ac # Epi, Acute success
            ,data = VTdata,family = "binomial"
)
summary(model)
exp(cbind(OR = coef(model), confint(model))) # results in ORs and their CIs
#cor(VTdata[,c(24)], VTdata[,c(27)], method = "pearson")
confint(model, conf.level=0.95)
plot(model)
anova(model)

# Logistic regression for acute success
ac <- (VTdata[,c(24)] == "1")* VTdata[,c(24)]
ac

model <- glm(formula = ac ~ VTdata[,c(2)] + VTdata[,c(6)] # acute success, age, redo
             + VTdata[,c(9)] + VTdata[,c(11)] + VTdata[,c(15)] + VTdata[,c(20)] # type of CM, LGE, CT, EF
             + VTdata[,c(23)] + VTdata[,c(27)] # Epi, recurrence
             ,data = VTdata, family = "binomial"
)
summary(model)
exp(cbind(OR = coef(model), confint(model))) # results in ORs and their CIs
#cor(VTdata[,c(24)], VTdata[,c(27)], method = "pearson")
confint(model, conf.level=0.95)
plot(model)
anova(model)

modage <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(2)], family = "binomial")
modage <- glm(formula = ac ~ VTdata[,c(2)] , family = "binomial")
summary(modage)
confint(modage)

modredo <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(6)] , family = "binomial")
modredo <- glm(formula = ac ~ VTdata[,c(6)] , family = "binomial")
summary(modredo)
confint(modredo)

modcm <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(9)] , family = "binomial")
modcm <- glm(formula = ac ~ VTdata[,c(9)] , family = "binomial")
summary(modcm)
confint(modcm)
plot(modcm)

modct <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(11)] , family = "binomial")
modct <- glm(formula = ac ~ VTdata[,c(11)] , family = "binomial")
summary(modct)
confint(modct)

modmri <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(15)] , family = "binomial")
modmri <- glm(formula = ac ~ VTdata[,c(15)] , family = "binomial")
summary(modmri)
confint(modmri)

modef <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(20)] , family = "binomial")
modef <- glm(formula = ac ~ VTdata[,c(20)] , family = "binomial")
summary(modef)
confint(modef)

modepi <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(23)] , family = "binomial")
modepi <- glm(formula = ac ~ VTdata[,c(23)] , family = "binomial")
summary(modepi)
confint(modepi)

modac <- glm(formula = VTdata[,c(27)] ~ ac , family = "binomial")
modac <- glm(formula = ac ~ VTdata[,c(24)] , family = "binomial")
summary(modac)
confint(modac)
plot(modac)

modanyi <- glm(formula = VTdata[,c(27)] ~ VTdata[,c(41)] , family = "binomial")
modanyi <- glm(formula = ac ~ VTdata[,c(41)] , family = "binomial")
summary(modanyi)
confint(modanyi)

modelrecur <- glm(formula = ac ~ VTdata[,c(27)], family = "binomial")
summary(modelrecur)

modelefac <- glm(formula = VTdata[,c(27)] ~ ac + VTdata[,c(20)])
summary(modelefac)
plot(modelefac)

modelcmrecur <- glm(formula = ac ~ VTdata[,c(27)] + VTdata[,c(9)])
summary(modelcmrecur)
exp(cbind(OR = coef(modelcmrecur), confint(modelcmrecur))) # results in ORs and their CIs


# Cox proportional hazards model
# Cox doesn't estimate b0
  # So we cannot estimate the survival function
  # But we can estimate the hazard ratios - this is exp(coef)
  # Someone with MRI is 1.0268x as likely to have an event (VT recurrence, death, transplant)
  # Below it gives you the confidence interval for the hazard ratio
  # null hypothesis - beta1 = beta 2 = beta...n = zero vs. alternative one is not equal to zero
  # concordance - c statistic - goodness of fit statistic for survival analysis. equivalent to AUC for logistic regression
    # consider observation 1 and observation 2. which person does the model predict has a longer survival time? person 1 survived longer than person 2, and the model predicts that
    # Concordance is the fraction of percentage of pairs of observations that are concordant. 
    # Expect 50% - you get it right half the time
  # Test with Lr when you add things to nested models. 
cox.mod <- coxph()
?coxph

cox.mod <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ MRI + 
                   VTdata[,c(6)] + VTdata[,c(15)] + VTdata[,c(20)] + VTdata[,c(23)]
                 + VTdata[,c(24)] + VTdata[,c(32)], data = VTdata) # 6 - previous ablation, 15 - CT, 20 - EF, 23 - epi, 24 - AC, 32 - antiarrhythmic
summary(cox.mod)


# Using Likelihood ratio test to see if dropping a variable makes it better or worse
cox.mod <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ MRI + 
                   VTdata[,c(6)] + VTdata[,c(15)] + VTdata[,c(20)] + VTdata[,c(23)]
                 + VTdata[,c(24)] + VTdata[,c(32)], data = VTdata)
cox.mod2 <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ MRI + 
                            VTdata[,c(6)] + VTdata[,c(15)] + VTdata[,c(20)] + VTdata[,c(23)]
                          + VTdata[,c(32)], data = VTdata)

# P value is small, there is a statistically significant difference between the two
anova(cox.mod2, cox.mod, test = "LRT")

sex <- as.factor(VTdata[,c(1)])
age <- VTdata[,c(2)]
icd <- as.factor(VTdata[,c(3)])
icd
prevabl <- as.factor(VTdata[,c(6)])
typecm <- as.factor(VTdata[,c(9)]) # Cant use this as its 3 groups, and by doing a group its relative to the first one
MRI <- as.factor(VTdata[,c(11)])
CT <- as.factor(VTdata[,c(15)])
EF <- VTdata[,c(20)]
epi <- as.factor(VTdata[,c(44)])
Acutesucess <- (VTdata[,c(24)] == "1")*VTdata[,c(24)]
Acutesucess <- as.factor(Acutesucess)
Acutesucess
ARprescribed <- as.factor((VTdata[,c(32)] == "1"))
AR1year <- (VTdata[,c(34)] == "1") * VTdata[,c(34)]
  AR1year[is.na(AR1year)] = 0
AR1year <- as.factor(AR1year)
AR1year


ct30 <- VTdata

final.model <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ MRI + sex + age 
                      + prevabl + CT + EF + epi + Acutesucess  + AR1year, data = VTdata)

summary(final.model)


mri3month <- (VTdata[,c(48)] < 94) * VTdata[,c(46)]
mri3month <- replace(mri3month, is.na(mri3month), 0)
mri3month <- as.factor(mri3month)
mri3month
cct3month <- (VTdata[,c(56)] < 94) * VTdata[,c(53)]
cct3month <- replace(cct3month, is.na(cct3month), 0)
cct3month <- as.factor(cct3month)
cct3month
echo <- replace(VTdata[,c(62)], is.na(VTdata[,c(62)]), 100)
echo <- as.numeric(echo < 94)
echo <- as.factor(echo)
echo

icm <- as.numeric(VTdata[,c(9)] == 1)
icm
nicm <- as.numeric(VTdata[,c(9)] == 2)
nicm
mixed <- as.numeric(VTdata[,c(9)] == 3)
mixed

# Final AHA model *****************************************************
final.model <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ mri3month + age 
                     + prevabl + cct3month + EF + epi + echo + Acutesucess + nicm, data = VTdata)

summary(final.model)

 ?coxph

cox.zph(final.model)

# ischemic model
i <- subset(VTdata,subset =  VTdata[,c(9)] == "1")
sex <- as.factor(i[,c(1)])
age <- i[,c(2)]
prevabl <- as.factor(i[,c(6)])
EF <- i[,c(20)]
epi <- as.factor(i[,c(44)])
Acutesucess <- (i[,c(24)] == "1")*i[,c(24)]
Acutesucess <- as.factor(Acutesucess)
mri3month <- (i[,c(48)] < 94) * i[,c(46)]
mri3month <- replace(mri3month, is.na(mri3month), 0)
mri3month <- as.factor(mri3month)
mri3month
cct3month <- (i[,c(56)] < 94) * i[,c(53)]
cct3month <- replace(cct3month, is.na(cct3month), 0)
cct3month <- as.factor(cct3month)
cct3month
echo <- replace(i[,c(62)], is.na(i[,c(62)]), 100)
echo <- as.numeric(echo < 94)
echo <- as.factor(echo)
echo

final.model <- coxph(Surv(time = i[,c(42)] , event = i[,c(43)]) ~ mri3month + age 
                     + prevabl + cct3month + EF + epi + echo + Acutesucess, data = VTdata)
summary(final.model)

# non-ischemic model
ni <- subset(VTdata,subset =  VTdata[,c(9)] == "2")
sex <- as.factor(ni[,c(1)])
age <- ni[,c(2)]
prevabl <- as.factor(ni[,c(6)])
EF <- ni[,c(20)]
epi <- as.factor(ni[,c(44)])
Acutesucess <- (ni[,c(24)] == "1")*ni[,c(24)]
Acutesucess <- as.factor(Acutesucess)
mri3month <- (ni[,c(48)] < 94) * ni[,c(46)]
mri3month <- replace(mri3month, is.na(mri3month), 0)
mri3month <- as.factor(mri3month)
mri3month
cct3month <- (ni[,c(56)] < 94) * ni[,c(53)]
cct3month <- replace(cct3month, is.na(cct3month), 0)
cct3month <- as.factor(cct3month)
cct3month
echo <- replace(ni[,c(62)], is.na(ni[,c(62)]), 100)
echo <- as.numeric(echo < 94)
echo <- as.factor(echo)
echo

final.model <- coxph(Surv(time = ni[,c(42)] , event = ni[,c(43)]) ~ mri3month + age 
                     + prevabl + cct3month + EF + epi + echo + Acutesucess, data = VTdata)
summary(final.model)




# Imaging within 1 month
mri1month <- (VTdata[,c(48)] < 32) * VTdata[,c(46)]
mri1month <- replace(mri1month, is.na(mri1month), 0)
mri1month
cct1month <- (VTdata[,c(56)] < 32) * VTdata[,c(53)]
cct1month <- replace(cct1month, is.na(cct1month), 0)
cct1month
final.model <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ mri1month + sex + age 
                     + prevabl + cct1month + EF + epi + Acutesucess  + AR1year, data = VTdata)
summary(final.model)

mri3month <- (VTdata[,c(48)] < 94) * VTdata[,c(46)]
mri3month <- replace(mri3month, is.na(mri3month), 0)
mri3month
cct3month <- (VTdata[,c(56)] < 94) * VTdata[,c(53)]
cct3month <- replace(cct3month, is.na(cct3month), 0)
cct3month
final.model <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ mri3month + sex + age 
                     + prevabl + cct3month + EF + epi + Acutesucess  + AR1year, data = VTdata)
summary(final.model)

image1month <- (VTdata[,c(56)] * VTdata[,c(53)]) <32 | (VTdata[,c(48)] <32 * VTdata[,c(46)])
image1month <- replace(image1month, is.na(image1month), 0)
image1month


final.model <- coxph(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ image1month + sex + age 
                     + prevabl  + EF + epi + Acutesucess  + AR1year, data = VTdata)
summary(final.model)


# eliminated ICD and AR prescribed

#####
# Playing with repeated data

freq(VTdata[,c(27)])
freq(VTdata[,c(41)])
freq(VTdata[,c(43)])
freq(repeats[,c(27)])
freq(repeats[,c(41)])
freq(repeats[,c(43)])
#rvsf = repeats vs. first time ablation
#Anyimaging
rvsf <- matrix(c(73, 56, 7, 8), byrow = T, ncol = 2, nrow = 2)
colnames(rvsf) <- c("imaging", "noimaging")
rownames(rvsf) <- c("first", "Repeats")
rvsf
chisq.test(rvsf)
#VT recurrence
rvsf <- matrix(c(74, 55, 10, 5), byrow = T, ncol = 2, nrow = 2)
colnames(rvsf) <- c("VT recurrence", "No recur")
rownames(rvsf) <- c("first", "Repeats")
rvsf
chisq.test(rvsf)


#####

imaging3months <- (VTdata[,c(56)] * VTdata[,c(53)]) <93 | (VTdata[,c(48)] <93 * VTdata[,c(46)])
imaging3months <- replace(imaging3months, is.na(imaging3months), 0)
imaging3months

# For grabbing MRI or CCT < 3 months
a <- subset(VTdata, subset = 
                   (VTdata[,c(48)] < 93) | VTdata[,c(56)] < 93
)

dim(a)

# For grabbing MRI < 3 months
a <- subset(VTdata, subset = 
              (VTdata[,c(48)] < 93)
              )
View(a)
dim(b)
dim(VTdata)

# Grab not MRI <3 months
d <- subset(VTdata, subset = (VTdata[c(48)] > 93 ) | is.na(VTdata[c(48)]))
dim(d)
View(d)

?subset
VTdata <- subset(VTdata, subset = (VTdata[c(48)] > 93 ) | is.na(VTdata[c(48)]))

# For grabbing CCT < 3 months
c <- subset(VTdata, subset = 
              (VTdata[,c(56)] < 93)
)
dim(c)


# Procedural success
as.numeric(VTdata[,c(62)] <93)



a[,c(62)]
a[c(62)] < 93
as.numeric(a[c(62)] < 93)


as.numeric(d[c(62)] < 93)


freq(VTdata[,c(24)])
ps <- matrix(c(30, 50, 8, 19, 2, 0, 8, 12), byrow = T, ncol = 2)
colnames(ps) <- c("MRI", "No mri")
rownames(ps) <- c("acute success", "partial sucess", "Failure", "Not tested")
ps
chisq.test(ps)
fisher.test(ps)
    
#####
# Table of abstract
  # For KM curves, I use MRIs anywhere and CTs anywhere - MRI does'nt multiply by c(46) - mayo only, and CCT multiplies by c(53) - any CT
  # For model its mris only at Mayo - is multiplied by c(46). CCT is multiplied by c(53)
  # For table, follow the KM curve - anywhere

mri <- subset(VTdata, subset = VTdata[,c(48)] < 94)
nomri <- subset(VTdata, subset = VTdata[,c(48)] >= 94 | is.na(VTdata[,c(48)]))
ct <- subset(VTdata, subset = VTdata[,c(56)] < 94)
noct <- subset(VTdata, subset = VTdata[,c(56)] >= 94 | is.na(VTdata[,c(56)]))

# Table of abstract

# Age
summary(VTdata[,c(2)])
summary(mri[,c(2)])
summary(nomri[,c(2)])
  t.test(x = mri[,c(2)], y = nomri[,c(2)], paired = F)
summary(ct[,c(2)])
summary(noct[,c(2)])
  t.test(x = ct[,c(2)], y = noct[,c(2)], paired = F)

# Sex
freq(VTdata[,1])
freq(mri[,1])
freq(nomri[,1])
freq(ct[,1])
freq(noct[,1])
sex <- matrix(c(41, 70, 7, 11), byrow = T, ncol = 2, nrow = 2)
colnames(sex) <- c("mri", "No mri")
rownames(sex) <- c("Male sex", "Female")
sex
chisq.test(sex)
fisher.test(sex)
sex <- matrix(c(28, 83, 3, 15), byrow = T, ncol = 2, nrow = 2)
colnames(sex) <- c("ct", "No ct")
rownames(sex) <- c("Male sex", "Female")
sex
chisq.test(sex)
fisher.test(sex)

# ICD
freq(VTdata[,3])
freq(mri[,3])
freq(nomri[,3])
freq(ct[,3])
freq(noct[,3])
sex <- matrix(c(43, 77, 5, 4), byrow = T, ncol = 2, nrow = 2)
colnames(sex) <- c("mri", "No mri")
rownames(sex) <- c("ICD", "No ICD")
sex
chisq.test(sex)
fisher.test(sex)
sex <- matrix(c(29, 91, 2, 7), byrow = T, ncol = 2, nrow = 2)
colnames(sex) <- c("ct", "No ct")
rownames(sex) <- c("ICD", "No ICD")
sex
chisq.test(sex)
fisher.test(sex)

# EF
summary(VTdata[,c(20)])
summary(mri[,c(20)])
summary(nomri[,c(20)])
  t.test(x = mri[,c(20)], y = nomri[,c(20)], paired = F)
summary(ct[,c(20)])
summary(noct[,c(20)])
  t.test(x = ct[,c(20)], y = noct[,c(20)], paired = F)

# Prior VT ablation
freq(VTdata[,c(6)])
freq(mri[,c(6)])
freq(nomri[,c(6)])
  redomri <- matrix(c(14, 23, 34, 58), byrow = T, ncol = 2, nrow = 2)
  colnames(redomri) <- c("MRI", "No mri")
  rownames(redomri) <- c("prior", "no prior")
  redomri
  chisq.test(redomri)
freq(ct[,c(6)])
freq(noct[,c(6)])
  redoct <- matrix(c(11, 26, 20, 72), byrow = T, ncol = 2, nrow = 2)
  colnames(redoct) <- c("ct", "No ct")
  rownames(redoct) <- c("prior", "no prior")
  redoct
  chisq.test(redomri)

# Type of cardiomyopathy; 1 = ischemic, 2 = other, 3 = mixed
freq(VTdata[,c(9)]) 
freq(mri[,c(9)])
freq(nomri[,c(9)])
  typeofcm <- matrix(c(16, 36, 26, 38, 6, 7), byrow = T, ncol = 2)
  colnames(typeofcm) <- c("MRI", "no MRI")
  rownames(typeofcm) <- c("ischemic", "nonischemic", "mixed")
  typeofcm
  chisq.test(typeofcm)
  fisher.test(typeofcm)
freq(ct[,c(9)])
freq(noct[,c(9)])
  typeofcm <- matrix(c(11, 41, 17, 47, 3, 10), byrow = T, ncol = 2)
  colnames(typeofcm) <- c("CT", "no CT")
  rownames(typeofcm) <- c("ischemic", "nonischemic", "mixed")
  typeofcm
  chisq.test(typeofcm)
  fisher.test(typeofcm)

# Imaging < 3 months - 48 is days from MRI, 56 is days from CCT, 62 is days from TTE
  # For MRI vs no MRI
    # Compare CCT use
  mri[,48] < 94 | 
  
      freq(mri[,c(56)] < 94)
      freq(nomri[,c(56)] < 94)
        cct <- matrix(c(9, 22, 39, 59), byrow = T, ncol = 2, nrow = 2)
        colnames(cct) <- c("MRI", "No MRI")
        rownames(cct) <- c("CT", "not")
        cct
        chisq.test(cct)
        fisher.test(cct)
    # Compare TTE use
      freq(mri[,c(62)] < 94)
      freq(nomri[,c(62)] < 94)
        echo <- matrix(c(44, 69, 4, 12), byrow = T, ncol = 2, nrow = 2)
        colnames(echo) <- c("MRI", "no MRI")
        rownames(echo) <- c("TTE", "no TTE")
        echo
        chisq.test(echo)
        fisher.test(echo)
  # For CCT vs No CCT
    # Compare CMR use
        freq(ct[,c(48)] < 94)
        freq(noct[,c(48)] < 94)
          cMRI <- matrix(c(9, 39, 22, 59), byrow = T, ncol = 2, nrow = 2)
          colnames(cMRI) <- c("CT", "no CT")
          rownames(cMRI) <- c("MRIdone", "not")
          cMRI
          chisq.test(cMRI)
          fisher.test(cMRI)
    # Compare TTE use
        freq(ct[,c(62)] < 94)
        freq(noct[,c(62)] < 94)
          echo <- matrix(c(28, 85, 3, 13), byrow = T, ncol = 2, nrow = 2)
          colnames(echo) <- c("CT", "no CT")
          rownames(echo) <- c("TTE", "no TTE")
          echo
          chisq.test(echo)
          fisher.test(echo)
    # Any imaging
    

# Epicardial ablation
freq(VTdata[,c(44)])
freq(mri[,c(44)])
freq(nomri[,c(44)])
  epiabl <- matrix(c(5, 12, 43, 69), byrow = T, ncol = 2, nrow = 2)
  colnames(epiabl) <- c("MR", "No MR")
  rownames(epiabl) <- c("epi", "No epi")
  epiabl
  chisq.test(epiabl)
freq(ct[,c(44)])
freq(noct[,c(44)])
  epiabl <- matrix(c(7, 10, 24, 88), byrow = T, ncol = 2, nrow = 2)
  colnames(epiabl) <- c("CT", "No CT")
  rownames(epiabl) <- c("epi", "not")
  epiabl
  chisq.test(epiabl)
  fisher.test(epiabl)

# Acute success; 1 = yes, 2 = partial, 3 = failure, 4 = not tested
freq(VTdata[,c(24)])
freq(mri[,c(24)])
freq(nomri[,c(24)])
  as <- matrix(c(30, 50, 18, 31), byrow = T, ncol = 2, nrow = 2)
  colnames(as) <- c("mr", "No mr")
  rownames(as) <- c("acute success", "not")
  as
  chisq.test(as)
  fisher.test(as)
freq(ct[,c(24)])
freq(noct[,c(24)])
  as <- matrix(c(17, 63, 14, 35), byrow = T, ncol = 2, nrow = 2)
  colnames(as) <- c("ct", "No ct")
  rownames(as) <- c("acute success", "not")
  as
  chisq.test(as)
  fisher.test(as)

# VT recurrence
freq(VTdata[,c(27)])
freq(mri[,c(27)])
freq(nomri[,c(27)])
  vtrecur <- matrix(c(27, 47, 21, 34), byrow = T, ncol = 2, nrow = 2)
  colnames(vtrecur) <- c("MRI", "No MRI")
  rownames(vtrecur) <- c("VT recurrence", "not")
  vtrecur
  chisq.test(vtrecur)
  fisher.test(vtrecur)
freq(ct[,c(27)])
freq(noct[,c(27)])
  vtrecur <- matrix(c(15, 59, 16, 39), byrow = T, ncol = 2, nrow = 2)
  colnames(vtrecur) <- c("ct", "No ct")
  rownames(vtrecur) <- c("VT recurrence", "not")
  vtrecur
  chisq.test(vtrecur)
  fisher.test(vtrecur)
  
# Transplant
freq(VTdata[,c(35)]) # Transplant at time of analysis
freq(mri[,c(35)]) # Transplant at time of analysis
freq(nomri[,c(35)]) # Transplant at time of analysis
freq(ct[,c(35)]) # Transplant at time of analysis
freq(noct[,c(35)]) # Transplant at time of analysis
transplant <- matrix(c(4, 5, 44, 76), byrow = T, ncol = 2, nrow = 2)
colnames(transplant) <- c("MRI", "No MRI")
rownames(transplant) <- c("transplant", "not")
transplant
chisq.test(transplant)
fisher.test(transplant)
transplant <- matrix(c(2, 7, 29, 91), byrow = T, ncol = 2, nrow = 2)
colnames(transplant) <- c("vt", "No vt")
rownames(transplant) <- c("transplant", "not")
transplant
chisq.test(transplant)
fisher.test(transplant)

# Death
freq(VTdata[,c(39)]) # Dead? 1 = dead
freq(mri[,c(39)]) # Dead? 1 = dead
freq(nomri[,c(39)]) # Dead? 1 = dead
freq(ct[,c(39)]) # Dead? 1 = dead
freq(noct[,c(39)]) # Dead? 1 = dead
death <- matrix(c(5, 11, 43, 70), byrow = T, ncol = 2, nrow = 2)
colnames(death) <- c("MRI", "No MRI")
rownames(death) <- c("death", "not")
death
chisq.test(death)
fisher.test(death)
transplant <- matrix(c(3, 13, 28, 85), byrow = T, ncol = 2, nrow = 2)
colnames(transplant) <- c("ct", "No ct")
rownames(transplant) <- c("death", "not death")
transplant
chisq.test(transplant)
fisher.test(transplant)

# Follow up
summary(VTdata[,c(38)]) / 30
summary(mri[,c(38)]) / 30
summary(nomri[,c(38)]) / 30
summary(ct[,c(38)]) / 30
summary(noct[,c(38)]) / 30

t.test(x = mri[,c(38)], y = nomri[,c(38)], paired = F)
t.test(x = ct[,c(38)], y = noct[,c(38)], paired = F)

# Time to endpoint
summary(VTdata[,c(42)]) / 30
summary(mri[,c(42)]) / 30
summary(nomri[,c(42)]) / 30
summary(ct[,c(42)]) / 30
summary(noct[,c(42)]) / 30

# Use for AHA- ********************************************************  
mri3months <- (VTdata[,c(48)] < 94) #* VTdata[,c(46)]
mri3months
test <- replace(mri3months, is.na(mri3months), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac MRI last 3 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

# Use for AHA- ******************************************************** 
cct3mo <- (VTdata[,c(56)] < 94) * VTdata[,c(53)]
cct3mo
test <- replace(cct3mo, is.na(cct3mo), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test, data = VTdata)
km.model1
summary(km.model1)

x11()
ggsurvplot(km.model1,
           risk.table = TRUE,
           pval=TRUE,
           pval.method = TRUE,
           legend.title = "Cardiac CT last 3 months",
           legend.labs = c("No", "Yes"),
           title="Survival free of VT recurrence, Tx, or death",
           censor = TRUE,
           palette = c("black", "grey")
           
           # xscale
           #xlim = c(0,500)
           #cumevents = TRUE,cumcensor = TRUE
)

