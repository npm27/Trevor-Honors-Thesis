####Ex 2 Analyses####
size = read.csv("Ex 2 Scored Output/Size.csv")
highlight = read.csv("Ex 2 Scored Output/Highlight.csv")
control = read.csv("Ex 2 Scored Output/Pure_Unrelated.csv")

control = control[ , -15]

##Load libraries
library(data.table)
library(ez)
library(Hmisc)

##Get sample size
length(unique(size$Username)) #40
length(unique(highlight$Username)) #40
length(unique(control$Username)) #37

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

####Write the datasheets to .csv so I can build the excel sheet####
##Need subject level means
##Start with Control
library(data.table)

##Control
control3 = subset(final.long,
                  final.long$Group.collapsed == "Control")
control4 = cast(control3, Username ~ Task, mean, na.rm = T)

##Size - small
size.small = subset(final.long,
                   final.long$Group == "JOL_Small")
size.small2 = cast(size.small, Username ~ Task, mean, na.rm = T)

##Size - large
size.large = subset(final.long,
                    final.long$Group == "JOL_Large")
size.large2 = cast(size.large, Username ~ Task, mean, na.rm = T)

##Highlights
highlight.yes = subset(final.long,
                       final.long$Group == "JOL_H")
highlight.yes2 = cast(highlight.yes, Username ~ Task, mean, na.rm = T)

##No highlights
highlight.no = subset(final.long,
                      final.long$Group == "JOL")
highlight.no2 = cast(highlight.no, Username ~ Task, mean, na.rm = T)

####Write output to .csv####
#write.csv(control4, file = "Control_cleaned.csv", row.names = F)
#write.csv(size.small2, file = "Size Small.csv", row.names = F)
#write.csv(size.large2, file = "size large.csv", row.names = F)
#write.csv(highlight.yes2, file = "highlight yes.csv", row.names = F)
#write.csv(highlight.no2, file = "highlight no.csv", row.names = F)

####GAMMAS!####
##Need to cut out the outliers omitted in the SPSS analyses

#control
summary(control2)

control_final = na.omit(control2)

control_final = subset(control_final,
                       control_final$Username != "5ee64cd142c24a0008de839b")
control_final = subset(control_final,
                       control_final$Username != "5f0c10b787a3fe533f292145")
control_final = subset(control_final,
                       control_final$Username != "972730")
control_final = subset(control_final,
                       control_final$Username != "w10002554")
control_final = subset(control_final,
                       control_final$Username != "w10044868")

length(unique(control_final$Username))

##font-size
#Large
large = subset(size,
               size$Procedure.Trial.Type == "JOL_Large")
summary(large)

size.large_final = na.omit(large)

size.large_final = subset(size.large_final,
                          size.large_final$Username != "5b1b75312767e20001e506ec")
size.large_final = subset(size.large_final,
                          size.large_final$Username != "5bba1b6b8f3bd70001e6be3a")
size.large_final = subset(size.large_final,
                          size.large_final$Username != "5caa2f919f9fb8000185329b")
size.large_final = subset(size.large_final,
                          size.large_final$Username != "5ec95a3fdafc5445f27e0b61")

length(unique(size.large_final$Username))


#small
small = subset(size,
               size$Procedure.Trial.Type == "JOL_Small")
summary(small)


size.small_final = na.omit(small)

size.small_final = subset(size.small_final,
                          size.small_final$Username != "5b1b75312767e20001e506ec")
size.small_final = subset(size.small_final,
                          size.small_final$Username != "5bba1b6b8f3bd70001e6be3a")
size.small_final = subset(size.small_final,
                          size.small_final$Username != "5caa2f919f9fb8000185329b")
size.small_final = subset(size.small_final,
                          size.small_final$Username != "5ec95a3fdafc5445f27e0b61")

length(unique(size.small_final$Username))


##highlight
#Yes highlights
yes = subset(highlight,
             highlight$Procedure.Trial.Type == "JOL_H")

highlight.yes_final = na.omit(yes)

highlight.yes_final = subset(highlight.yes_final,
                             highlight.yes_final$Username != "5e8f0692facc7d1c12b4c9a1")
highlight.yes_final = subset(highlight.yes_final,
                             highlight.yes_final$Username != "5ede34f5255765051d450b9f")
highlight.yes_final = subset(highlight.yes_final,
                             highlight.yes_final$Username != "5ef516fac843781447f13719")

length(unique(highlight.yes_final$Username))

#No highlights
no = subset(highlight,
            highlight$Procedure.Trial.Type == "JOL")

highlight.no_final = na.omit(no)

highlight.no_final = subset(highlight.no_final,
                             highlight.no_final$Username != "5e8f0692facc7d1c12b4c9a1")
highlight.no_final = subset(highlight.no_final,
                             highlight.no_final$Username != "5ede34f5255765051d450b9f")
highlight.no_final = subset(highlight.no_final,
                             highlight.no_final$Username != "5ef516fac843781447f13719")

length(unique(highlight.no_final$Username))

###Now compute the gammas
##control
empty = data.frame()

for (i in unique(control_final$Username)){
  
  temp = subset(control_final, control_final$Username == i)
  
  g = rcorr.cens(temp$JOL, temp$Recall, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_control = empty

##font-size
#large
empty = data.frame()

for (i in unique(size.large_final$Username)){
  
  temp = subset(size.large_final, size.large_final$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_large = empty

#small
empty = data.frame()

for (i in unique(size.small_final$Username)){
  
  temp = subset(size.small_final, size.small_final$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_small = empty

##highlight
#yes highlights
empty = data.frame()

for (i in unique(highlight.yes_final$Username)){
  
  temp = subset(highlight.yes_final, highlight.yes_final$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight_yes = empty

#no highlights
empty = data.frame()

for (i in unique(highlight.no_final$Username)){
  
  temp = subset(highlight.no_final, highlight.no_final$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight_no = empty

##Now means and CIs for table!
#control
mean(Gammas_control$g, na.rm = T)
(sd(Gammas_control$g, na.rm = ) /sqrt(length(unique(Gammas_control$i)))) * 1.96

#Large
mean(Gammas_large$g, na.rm = T)
(sd(Gammas_large$g, na.rm = ) /sqrt(length(unique(Gammas_large$i)))) * 1.96

#small
mean(Gammas_small$g, na.rm = T)
(sd(Gammas_small$g, na.rm = ) /sqrt(length(unique(Gammas_small$i)))) * 1.96

#Yes
mean(Gammas_highlight_yes$g, na.rm = T)
(sd(Gammas_highlight_yes$g, na.rm = T) /sqrt(length(unique(Gammas_highlight_yes$i)))) * 1.96

#no
mean(Gammas_highlight_no$g, na.rm = T)
(sd(Gammas_highlight_no$g, na.rm = ) /sqrt(length(unique(Gammas_highlight_no$i)))) * 1.96
