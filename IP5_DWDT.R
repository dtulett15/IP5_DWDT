# Individual Project 4
# Author: Dylan Tulett
# Version: 1.0
# Date: March 2021

## 1.0 Reading in data
library(tidyverse)
library(psych)
library(foreign)
ckd<-read.arff('chronic_kidney_disease.arff')
length(ckd)
str(ckd)


## 2.0 Getting Variable Data
### 2.1 Getting and saving out variables missing data

md<-function(column){
  as.numeric(format(round(sum(is.na(column))/400, 3), nsmall=3))*100
}
md(ckd$rbc)

variable_name <- c(colnames(ckd))
variable_type <- c(sapply(ckd, class))
missing_data <- c(md(ckd$age),md(ckd$bp),md(ckd$sg),md(ckd$al),md(ckd$su),
                  md(ckd$rbc),md(ckd$pc),md(ckd$pcc),md(ckd$ba),
                  md(ckd$bgr),md(ckd$bu),md(ckd$sc),md(ckd$sod),md(ckd$pot),
                  md(ckd$hemo),md(ckd$pcv),md(ckd$wbcc),md(ckd$rbcc),
                  md(ckd$htn),md(ckd$dm),md(ckd$cad),md(ckd$appet),md(ckd$pe),
                  md(ckd$ane),md(ckd$class))

ckd_variables <- data.frame(variable_name,variable_type,missing_data)

write.csv(ckd_variables,file='ckd_variables.csv')

### 2.2 Saving summary data
summary_data <- describe(ckd)
write.csv(summary_data, file='ckd_summary_data.csv')

###2.3 Saving out proportion data
ckd_frequencies <- c((table(ckd$age)),(table(ckd$bp)),(table(ckd$sg)),(table(ckd$al)),(table(ckd$su)),
                     (table(ckd$rbc)),(table(ckd$pc)),(table(ckd$pcc)),(table(ckd$ba)),
                     (table(ckd$bgr)),(table(ckd$bu)),(table(ckd$sc)),(table(ckd$sod)),(table(ckd$pot)),
                     (table(ckd$hemo)),(table(ckd$pcv)),(table(ckd$wbcc)),(table(ckd$rbcc)),
                     (table(ckd$htn)),(table(ckd$dm)),(table(ckd$cad)),(table(ckd$appet)),(table(ckd$pe)),
                     (table(ckd$ane)),(table(ckd$class)))
ckd_proportion <- ckd_frequencies*100/400

proportion_data <- data.frame(variable_name, ckd_frequencies, ckd_proportion)
#Didn't Work. I'm not sure how to do this in R. It seems I have made ckd_frequencies
#and ckd_proportions as one dimensional lists, but I'm not sure how to change this.
#But if it had worked, this is how I would save it out.
write.csv(proportion_data, file='ckd_proportion_data.csv')




## 3.0 Visualizing Data
### 3.1 Displaying and saving the correlation chart
ckd_numerics <- subset(ckd, select = c(age,bp,bgr,bu,sc,sod,pot,hemo,pcv,wbcc,rbcc))
correlation_data=cor(ckd_numerics,use='complete.obs')
write.csv(correlation_data, file='ckd_corr.ckd')


### 3.2 Creating a Heatmap
ckd_heatmap <- heatmap(
  correlation_data,
  Colv = NA, Rowv = NA,
  main = 'Correlation Heatmap',
  xlab = 'Variables',
  ylab = 'Variables',
  )

### 3.3 Researching data distribution

#age vs. class
ggplot(data = ckd, mapping = aes(x=class, y=age, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Class')

#age vs al
ggplot(data = ckd, mapping = aes(x=al, y=age, fill=al)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Albumin')

#age vs su
ggplot(data = ckd, mapping = aes(x=su, y=age, fill=su)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Sugar')

#age vs sg
ggplot(data = ckd, mapping = aes(x=sg, y=age, fill=sg)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Specific Gravity')

#age vs rbc
ggplot(data = ckd, mapping = aes(x=rbc, y=age, fill=rbc)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Red Blood Cells')

#age vs pc
ggplot(data = ckd, mapping = aes(x=pc, y=age, fill=pc)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Puss Cells')

#age vs pcc
ggplot(data = ckd, mapping = aes(x=pcc, y=age, fill=pcc)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Puss Cell Clumps')

#age vs ba
ggplot(data = ckd, mapping = aes(x=ba, y=age, fill=ba)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Bacteria')

#age vs htn
ggplot(data = ckd, mapping = aes(x=htn, y=age, fill=htn)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Hypertension')

#age vs dm
ggplot(data = ckd, mapping = aes(x=dm, y=age, fill=dm)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Diabete Mellitus')

#age vs cad
ggplot(data = ckd, mapping = aes(x=cad, y=age, fill=cad)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Coronary Artery Disease')

#age vs appet
ggplot(data = ckd, mapping = aes(x=appet, y=age, fill=appet)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Appetite')

#age vs pe
ggplot(data = ckd, mapping = aes(x=pe, y=age, fill=pe)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Pedal Edema')

#age vs ane
ggplot(data = ckd, mapping = aes(x=ane, y=age, fill=ane)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by Anemia')

### Box plots based on class
#pot vs class
ggplot(data = ckd, mapping = aes(x=class, y=pot, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#age vs class
ggplot(data = ckd, mapping = aes(x=class, y=age, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#bp vs class
ggplot(data = ckd, mapping = aes(x=class, y=bp, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#bgr vs class
ggplot(data = ckd, mapping = aes(x=class, y=bgr, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#bu vs class
ggplot(data = ckd, mapping = aes(x=class, y=bu, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#sc vs class
ggplot(data = ckd, mapping = aes(x=class, y=sc, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#sod vs class
ggplot(data = ckd, mapping = aes(x=class, y=sod, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#hemo vs class
ggplot(data = ckd, mapping = aes(x=class, y=hemo, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#pcv vs class
ggplot(data = ckd, mapping = aes(x=class, y=pcv, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#wbcc vs class
ggplot(data = ckd, mapping = aes(x=class, y=wbcc, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')

#rbcc vs class
ggplot(data = ckd, mapping = aes(x=class, y=rbcc, fill=class)) + geom_boxplot() + 
  guides(fill = FALSE) + ggtitle('Boxplot Grouped by class')


#2.3 creating a scatterplots and pairplots
pairs(ckd[,10:18])
plot(ckd[,15:18],
     main="Blood Factor Relationships in CKD and nonCKD Patients",
     pch=19,
     col="blue",
     cex=0.9)

p1<-ggplot(data=ckd, mapping = aes(x=rbcc, y=hemo, colour = class)) +
  geom_point() +
  labs(title='Hemoglobin vs. Red Blood Cell Count', 
       x='Red Blood Cell Count (million/cc)', 
       y='Hemoglobin (grams)')

p2<-ggplot(data=ckd, mapping = aes(x=pcv, y=hemo, colour = class)) +
  geom_point() +
  labs(title='Hemoglobin vs. Packed Cell Volume', 
       x='Packed Cell Volume', 
       y='Hemoglobin (grams)')

#data on same plot
ggplot(data=ckd) +
  geom_point(mapping = aes(x=hemo,y=pcv,colour='pcv')) + 
  geom_point(mapping = aes(x=hemo,y=rbcc,colour='rbcc'))  

install.packages("ggpubr")
library('ggpubr')

#Data on same figure, but two plots
ggarrange(p1, p2,labels = c("A", "B"),ncol = 1, nrow = 2)

### 3.5 Bar Charts/Other
ggplot(data = ckd) + geom_bar(mapping = aes(x=su)) + 
  labs(title='Sugar Group Frequencies',x='Sugar Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=sg)) + 
  labs(title='Specific Gravity Frequencies',x='Specific Gravity Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=al)) + 
  labs(title='Albumin Group Frequencies',x='Albumin Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=rbc)) + 
  labs(title='Red Blood Cell Group Frequencies',x='Red Blood Cell Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=pc)) + 
  labs(title='Puss Cell Group Frequencies',x='Pus Cell Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=pcc)) + 
  labs(title='Pus Cell Clump Group Frequencies',x='Pus Cell Clump Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=ba)) + 
  labs(title='Bacteria Group Frequencies',x='Bacteria Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=htn)) + 
  labs(title='Hypertension Group Frequencies',x='Hypertension Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=dm)) + 
  labs(title='Diabetes Mellitus Group Frequencies',x='Diabetes Mellitus Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=cad)) + 
  labs(title='Coronary Artery Disease Group Frequencies',x='Coronary Artery Disease Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=appet)) + 
  labs(title='Appetite Group Frequencies',x='Appetite Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=pe)) + 
  labs(title='Pedal Edema Group Frequencies',x='Pedal Edema Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=ane)) + 
  labs(title='Anemia Group Frequencies',x='Anemia Groups')

ggplot(data = ckd) + geom_bar(mapping = aes(x=class)) + 
  labs(title='Class Group Frequencies',x='Class Groups')


ggplot(data = ckd) + geom_histogram(mapping = aes(x=wbcc))
  


