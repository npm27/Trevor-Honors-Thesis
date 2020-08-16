####Ex 2 Analyses####
size = read.csv("Ex 2 Scored Output/Size.csv")
highlight = read.csv("Ex 2 Scored Output/Highlight.csv")
control = read.csv("Ex 2 Scored Output/Pure_Unrelated.csv")

control = control[ , -15]

##Load libraries
library(data.table)
library(ez)

##Get sample size
length(unique(size$Username)) #40
length(unique(highlight$Username)) #40
length(unique(control$Username)) #19

colnames(size)[7] = "Direction"
colnames(highlight)[7] = "Direction"

##Remove punctuation/special characters from JOLs
table(highlight$Response.JOL)

highlight$Response.JOL =  gsub("23'", "23", highlight$Response.JOL)
highlight$Response.JOL = as.numeric(highlight$Response.JOL)

table(size$Response.JOL)

size$Response.JOL =  gsub("Y", NA, size$Response.JOL)
size$Response.JOL = as.numeric(size$Response.JOL)

##Get descriptives
summary(size)
summary(highlight)
summary(control)

#Remove out of range JOLs
size$Response.JOL[size$Response.JOL > 100] = NA
highlight$Response.JOL[highlight$Response.JOL > 100] = NA
#control$Response.JOL[control$Response.JOL > 100] = NA

##Get means
##JOLs
mean(size$Response.JOL, na.rm = T)
mean(highlight$Response.JOL, na.rm = T)
mean(control$JOL, na.rm = T)

##Recall
mean(size$Recall_Score)
mean(highlight$Recall_Score)
mean(control$Recall_Score)

##Now look at each manipulation
##Size
tapply(size$Response.JOL, size$Procedure.Trial.Type, mean, na.rm = T)
tapply(size$Recall_Score, size$Procedure.Trial.Type, mean, na.rm = T)

##Highlight
tapply(highlight$Response.JOL, highlight$Procedure.Trial.Type, mean, na.rm = T)
tapply(highlight$Recall_Score, highlight$Procedure.Trial.Type, mean, na.rm = T)

##Control
tapply(control$JOL, control$Direction, mean, na.rm = T)
tapply(control$Recall_Score, control$Direction, mean, na.rm = T)

####Set up Data for ANOVAs####
##Get only the needed columns
control2 = control[ , -c(2:8, 10:12, 14)]
control2$Group = rep("Control")
control2$Group.collapsed = rep("Control")

size2 = size[ , -c(2:7 ,9, 11:13)]
size2$Group.collapsed = rep("Size")

highlight2 = highlight[ , -c(2:7 ,9, 11:13)]
highlight2$Group.collapsed = rep("highlight")

##Reorder columns
size2 = size2[ , c(1,2,5,3,4)]
highlight2 = highlight2[ , c(1,2,5,3,4)]
control2 = control2[ , c(1,4,5,2,3)]

##Fix column names
colnames(size2)[4:5] = c("JOL", "Recall")
colnames(highlight2)[4:5] = c("JOL", "Recall")
colnames(control2)[4:5] = c("JOL", "Recall")

colnames(size2)[2] = "Group"
colnames(highlight2)[2] = "Group"

##Combine stuff
final = rbind(size2, highlight2, control2)

##Now get in long format for ANOVA
final.long = melt(final,
                  measure.vars = c("JOL", "Recall"))

#Fix column names
colnames(final.long)[4:5] = c("Task", "Score")

summary(final.long)

final.long = na.omit(final.long)

tapply(final.long$Score, list(final.long$Group, final.long$Task), mean)

####RUN THE ANOVA####
model1 = ezANOVA(final.long,
                 dv = Score,
                 wid = Username,
                 within = Task,
                 between = Group.collapsed,
                 type = 3,
                 detailed = T)
model1

##Looking at individual manipulations
model2 = ezANOVA(final.long,
                 dv = Score,
                 wid = Username,
                 within = Task,
                 between = Group,
                 type = 3,
                 detailed = T)
model2

##Removing the control
final.long2 = subset(final.long,
                     final.long$Group.collapsed != "Control")

model3 = ezANOVA(final.long2,
                 dv = Score,
                 wid = Username,
                 within = Task,
                 between = Group,
                 type = 3,
                 detailed = T)
model3
