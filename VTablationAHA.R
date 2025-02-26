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


####
mri3months <- (VTdata[,c(48)] < 94) #* VTdata[,c(46)]
mri3months
test1 <- replace(mri3months, is.na(mri3months), 0)
km.model1 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test1, data = VTdata)
km.model1
summary(km.model1)
p1 <- ggsurvplot(km.model1,
                 surv.scale = "percent",
                 risk.table = TRUE,
                 pval=TRUE,
                 pval.method = TRUE,
                 xlab = c("Years"),
                 ylab = element_blank(),
                 title="CMR within 3 months",
                 font.title = c(20, "bold", "black"),
                 legend.labs = c("No", "Yes"),
                 legend.title = c(""),
                 xscale = c("d_y"),
                 break.x.by = 365.25 * 0.5,
                 risk.table.title = c("At Risk"),
                 risk.table.fontsize = 4,
                 risk.table.y.text = FALSE,
                 risk.table.y.text.col = TRUE,
                 tables.y.text = FALSE,
                 tables.height = 0.15,
                 censor = TRUE,
                 font.x = 14,
                 font.y = 14,
                 font.tickslab = 16,
                 palette = c("black", "grey"),
)
p1

cct3mo <- (VTdata[,c(56)] < 94) * VTdata[,c(53)]
cct3mo
test2 <- replace(cct3mo, is.na(cct3mo), 0)
km.model2 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test2, data = VTdata)
km.model2
summary(km.model2)

x11()
p2 <- ggsurvplot(km.model2,
                 surv.scale = "percent",
                 risk.table = TRUE,
                 pval=TRUE,
                 pval.method = TRUE,
                 xlab = c("Years"),
                 ylab = element_blank(),
                 title="CCT within 3 months",
                 font.title = c(20, "bold", "black"),
                 legend.labs = c("No", "Yes"),
                 legend.title = c(""),
                 xscale = c("d_y"),
                 break.x.by = 365.25 * 0.5,
                 risk.table.title = c("At Risk"),
                 risk.table.fontsize = 4,
                 risk.table.y.text = FALSE,
                 risk.table.y.text.col = TRUE,
                 tables.y.text = FALSE,
                 tables.height = 0.15,
                 censor = TRUE,
                 font.x = 14,
                 font.y = 14,
                 font.tickslab = 16,
                 palette = c("black", "grey"),
)

threemonths <- VTdata[,c(48)] < 93 | VTdata[,c(56)] < 93
threemonths
test3 <- replace(threemonths, is.na(threemonths), 0)
km.model3 <- survfit(Surv(time = VTdata[,c(42)] , event = VTdata[,c(43)]) ~ test3, data = VTdata)
km.model3
summary(km.model3)

p3 <- ggsurvplot(km.model3,
                 surv.scale = "percent",
                 risk.table = TRUE,
                 pval=TRUE,
                 pval.method = TRUE,
                 xlab = c("Years"),
                 ylab = element_blank(),
                 title="CMR/CCT within 3 months",
                 font.title = c(20, "bold", "black"),
                 legend.labs = c("No", "Yes"),
                 legend.title = c(""),
                 xscale = c("d_y"),
                 break.x.by = 365.25 * 0.5,
                 risk.table.title = c("At Risk"),
                 risk.table.fontsize = 4,
                 risk.table.y.text = FALSE,
                 risk.table.y.text.col = TRUE,
                 tables.y.text = FALSE,
                 tables.height = 0.15,
                 censor = TRUE,
                 font.x = 14,
                 font.y = 14,
                 font.tickslab = 16,
                 palette = c("black", "grey"),
)

arrange_ggsurvplots(x = list(p1,p2,p3), ncol = 3, nrow =  1)
arrange_ggsurvplots(x = list(p1,p2), ncol = 2, nrow =  1)

### # Pie chart ###
library(ggplot2)
library(ggpubr)
library(dplyr)
library(ggrepel)
mri3month <- (VTdata[,c(48)] < 94)
mri3month <- replace(mri3month, is.na(mri3month), 0)
mri3month <- as.numeric(mri3month)
mri3month
cct3month <- (VTdata[,c(56)] < 94)
cct3month <- replace(cct3month, is.na(cct3month), 0)
cct3month <- as.numeric(cct3month)
cct3month
echo3month <- (VTdata[,62] < 94)
echo3month <- replace(echo3month, is.na(echo3month), 0)
echo3month <- as.numeric(echo3month)
echo3month
any <- 0
mri <- 0
cct <- 0
echo <- 0
for (i in 1:129) {
  if (mri3month[i] == 1) {
    mri <- mri + 1
  }
  if (cct3month[i] == 1) {
    cct <- cct + 1
  }
  if (echo3month[i] == 1) {
    echo <- echo + 1
  } 
  if (mri3month[i] == 1 | cct3month[i] == 1 | echo3month[i] == 1) {
    any <- any + 1
  }
}
mri
cct
echo
any

mri3month
cct3month
echo3month

mriplot <- data.frame(group = c("Yes", "No"), value = c(48, 81))
mriplotlabels <- mriplot %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.1*prop)
mriplotlabels
cctplot <- data.frame(group = c("Yes", "No"), value = c(31, 98))
cctplotlabels <- cctplot %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.1*prop)
cctplotlabels
echoplot <- data.frame(group = c("Yes", "No"), value = c(113, 16))
echoplotlabels <- echoplot %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.1*prop)
echoplotlabels
anyplot <- data.frame(group = c("Yes", "No"), value = c(119, 10))
anyplotlabels <- anyplot %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.1*prop)
anyplotlabels


## ##
# Use for graphs
mriplot <- data.frame(group = c("Yes", "No"), value = c(48, 81))
mriplotlabels <- mriplot %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))  %>%
  mutate(pct = round(100 * value / 129, digits = 0))
mriplotlabels
cctplot <- data.frame(group = c("Yes", "No"), value = c(31, 98))
cctplotlabels <- cctplot %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos)) %>%
  mutate(pct = round(100 * value / 129, digits = 0))
cctplotlabels
echoplot <- data.frame(group = c("Yes", "No"), value = c(113, 16))
echoplotlabels <- echoplot %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos)) %>%
  mutate(pct = round(100 * value / 129, digits = 0))
echoplotlabels
anyplot <- data.frame(group = c("Yes", "No"), value = c(119, 10))
anyplotlabels <- anyplot %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos)) %>%
  mutate(pct = round(100 * value / 129, digits = 0))
anyplotlabels


p1 <- ggplot(data = mriplot, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "CMR < 3 months") + 
  #geom_label_repel(data = mriplotlabels, 
   #                aes(y= pos, label = paste0(pct, "%")), 
    #                   size = 6, nudge_x = 1, show.legend = FALSE) +
  #geom_text(data = mriplotlabels, aes(y = ypos, label = paste(round(prop, digits = 0), "%") ), color = "black", size = 6) +
  geom_label(data = mriplotlabels, aes(label = paste(pct, "%")),
             position = position_stack(vjust = 0.5),
             color = "black", size = 12, show.legend = FALSE, fill = "white") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 24)
  )
p2 <- ggplot(data = cctplot, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "CCT < 3 months") + 
  #geom_text(data = cctplotlabels, aes(y = ypos, label = paste(round(prop, digits = 0), "%") ), color = "black", size = 6) +
  #geom_label_repel(data = cctplotlabels, 
   #                aes(y= pos, label = paste0(pct, "%")), 
    #               size = 6, nudge_x = 1, show.legend = FALSE) +
  geom_label(data = cctplotlabels, aes(label = paste(pct, "%")),
             position = position_stack(vjust = 0.5),
             color = "black", size = 12, show.legend = FALSE, fill = "white") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 24)
  )
p3 <- ggplot(data = echoplot, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "Echo < 3 months") + 
  #geom_text(data = echoplotlabels, aes(y = ypos, label = paste(round(prop, digits = 0), "%") ), color = "black", size = 6) +
 # geom_label_repel(data = echoplotlabels, aes(label = paste0(pct, "%")), nudge_x = 0.5,  size = 6, show.legend = FALSE) +
  geom_label(data = echoplotlabels, aes(label = paste(pct, "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 12, show.legend = FALSE, fill = "white") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 24)
  )
p4 <- ggplot(data = anyplot, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "Any imaging < 3 months") + 
  #geom_text(data = anyplotlabels, aes(y = ypos, label = paste(round(prop, digits = 0), "%") ), color = "black", size = 6) +
  #geom_label_repel(data = anyplotlabels, aes(y= pos, label = paste0(pct, "%")), size = 6, nudge_x = 1, show.legend = FALSE) +
  geom_label(data = anyplotlabels, aes(label = paste(pct, "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 12, show.legend = FALSE, fill = "white") +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 24),
    legend.key = element_blank()
  )
x11()
ggarrange(p1, p2, p3, p4, nrow = 1, labels = c("", "", "", ""))


#####
# Not used
ischemic <- subset(VTdata, subset = VTdata[,c(9)] == "1") # grabs ischemic
nonischemic <- subset(VTdata, subset= VTdata[,c(9)] == "2")
ischemicmri <- subset(ischemic, subset = ischemic[,c(11)] == "1")
ischemicnomri <- subset(ischemic, subset = ischemic[,c(11)] == "0")
nonischemicmri <- subset(nonischemic, subset = nonischemic[,c(11)] == "1")
nonischemicnomri <- subset(nonischemic, subset = nonischemic[,c(11)] == "0")
ischemicct <- subset(ischemic, subset = ischemic[,c(15)] == "1")
ischemicnoct <- subset(ischemic, subset = ischemic[,c(15)] == "0")
nonischemicct <- subset(nonischemic, subset = nonischemic[,c(15)] == "1")
nonischemicnoct <- subset(nonischemic, subset = nonischemic[,c(15)] == "0")

freq(ischemicmri[,c(43)])
freq(ischemicnomri[,c(43)])
freq(ischemicct[,c(43)])
freq(ischemicnoct[,c(43)])
freq(nonischemicmri[,c(43)])
freq(nonischemicnomri[,c(43)])
freq(nonischemicct[,c(43)])
freq(nonischemicnoct[,c(43)])

endischemicmri <- matrix(c(9, 7, 21, 15), byrow = T, ncol = 2, nrow = 2)
rownames(endischemicmri) <- c("Endpoint", "No Endpoint")
colnames(endischemicmri) <- c("MRI", "no MRI")
endischemicmri
chisq.test(endischemicmri)

endischemicct <- matrix(c(7, 7, 21, 17), byrow = T, ncol = 2, nrow = 2)
rownames(endischemicct) <- c("Endpoint", "No Endpoint")
colnames(endischemicct) <- c("CT", "no CT")
endischemicct
chisq.test(endischemicct)

endnonischemicmri <- matrix(c(16, 10, 23, 15), byrow = T, ncol = 2, nrow = 2)
rownames(endnonischemicmri) <- c("Endpoint", "No Endpoint")
colnames(endnonischemicmri) <- c("MRI", "no MRI")
endnonischemicmri
chisq.test(endnonischemicmri)

endnonischemicct <- matrix(c(8, 9, 31, 16), byrow = T, ncol = 2, nrow = 2)
rownames(endnonischemicct) <- c("Endpoint", "No Endpoint")
colnames(endnonischemicct) <- c("CT", "no CT")
endnonischemicct
chisq.test(endnonischemicct)
