####Ex 1 Analyses####
size = read.csv("Ex 1 Scored Output/Size.csv")
highlight = read.csv("Ex 1 Scored Output/Highlight.csv")
control = read.csv("JOL3.csv")

library(ez)
library(data.table)

##Get sample size
length(unique(size$Username))
length(unique(highlight$Username))
length(unique(control$Username))

colnames(size)[7] = "Direction"
colnames(highlight)[7] = "Direction"
colnames(control)[7] = "Direction"

##Remove punctuation/special characters from JOLs
size$Response.JOL =  gsub("[[:punct:]]", "", size$Response.JOL)
size$Response.JOL =  gsub("I", NA, size$Response.JOL)

#Make Size jols numeric
size$Response.JOL = as.numeric(size$Response.JOL)

##Get descriptives
summary(size)
summary(highlight)
summary(control)

#Remove out of range JOLs
size$Response.JOL[size$Response.JOL > 100] = NA
highlight$Response.JOL[highlight$Response.JOL > 100] = NA
control$Response.JOL[control$Response.JOL > 100] = NA

##Get means
##JOLs
tapply(size$Response.JOL, size$Direction, mean, na.rm = T)
tapply(highlight$Response.JOL, highlight$Direction, mean, na.rm = T)
tapply(control$Response.JOL, control$Direction, mean, na.rm = T)

##Recall
tapply(size$Recall_Score, size$Direction, mean, na.rm = T)
tapply(highlight$Recall_Score, highlight$Direction, mean, na.rm = T)
tapply(control$Recall_Score, control$Direction, mean, na.rm = T)

##Now look at each manipulation
##Size
tapply(size$Response.JOL, list(size$Procedure.Trial.Type, size$Direction), mean, na.rm = T)
tapply(size$Recall_Score, list(size$Procedure.Trial.Type, size$Direction), mean, na.rm = T)

##Highlight
tapply(highlight$Response.JOL, list(highlight$Procedure.Trial.Type, highlight$Direction), mean, na.rm = T)
tapply(highlight$Recall_Score, list(highlight$Procedure.Trial.Type, highlight$Direction), mean, na.rm = T)

##Control
tapply(control$Response.JOL, control$Direction, mean, na.rm = T)
tapply(control$Recall_Score, control$Direction, mean, na.rm = T)

####Write output to .csv####
##Need subject level means
##Start with Control
control2 = control[ , -c(2:6, 8, 10:12)]

control_jol = control2[ , -4]
control_recall = control2[ , -3]

##Get subject level means by direction
control_jol2 = cast(control_jol, Username ~ Direction, mean, na.rm = T)
control_recall2 = cast(control_recall, Username ~ Direction, mean, na.rm = T)

####Now do it again for size####
size2 = size[ , -c(2:6, 9, 11:13)]

size.small = subset(size2,
                    size2$Procedure.Trial.Type == "JOL_Small")
size.large = subset(size2,
                    size2$Procedure.Trial.Type == "JOL_Large")

size.small.jol = size.small[ , -c(3,5)]
size.small.recall = size.small[ , -c(3:4)]

size.large.jol = size.large[ , -c(3,5)]
size.large.recall = size.large[ , -c(3:4)]

size.small.jol2 = cast(size.small.jol, Username ~ Direction, mean, na.rm = T)
size.small.recall2 = cast(size.small.recall, Username ~ Direction, mean, na.rm = T)

size.large.jol2 = cast(size.large.jol, Username ~ Direction, mean, na.rm = T)
size.large.recall2 = cast(size.large.recall, Username ~ Direction, mean, na.rm = T)

####Now for highlights####
highlight2 = highlight[ , -c(2:6, 9, 11:13)]

highlight.yes = subset(highlight2,
                    highlight2$Procedure.Trial.Type == "JOL_H")
highlight.no = subset(highlight2,
                    highlight2$Procedure.Trial.Type == "JOL")

highlight.yes.jol = highlight.yes[ , -c(3,5)]
highlight.yes.recall = highlight.yes[ , -c(3:4)]

highlight.no.jol = highlight.no[ , -c(3,5)]
highlight.no.recall = highlight.no[ , -c(3:4)]

highlight.yes.jol2 = cast(highlight.yes.jol, Username ~ Direction, mean, na.rm = T)
highlight.yes.recall2 = cast(highlight.yes.recall, Username ~ Direction, mean, na.rm = T)

highlight.no.jol2 = cast(highlight.no.jol, Username ~ Direction, mean, na.rm = T)
highlight.no.recall2 = cast(highlight.no.recall, Username ~ Direction, mean, na.rm = T)

####Write everything to csv####
write.csv(control_jol2, file = "EX 1 Cleaned/Control_JOL.csv", row.names = F)
write.csv(control_recall2, file = "EX 1 Cleaned/Control_Recall.csv", row.names = F)
write.csv(highlight.no.jol2, file = "EX 1 Cleaned/NO Highlight JOL.csv", row.names = F)
write.csv(highlight.yes.jol2, file = "EX 1 Cleaned/Yes Highlight JOL.csv", row.names = F)
write.csv(highlight.no.recall2, file = "EX 1 Cleaned/NO HIGHLIGHT Recall.csv", row.names = F)
write.csv(highlight.yes.recall2, file = "EX 1 Cleaned/YES HIGHLIGHT RECALL.csv", row.names = F)
write.csv(size.large.jol2, file = "EX 1 Cleaned/Large JOL.csv", row.names = F)
write.csv(size.large.recall2, file = "EX 1 Cleaned/Large recall.csv", row.names = F)
write.csv(size.small.jol2, file = "EX 1 Cleaned/Small JOL.csv", row.names = F)
write.csv(size.small.recall2, file = "Ex 1 Cleaned/Small Recall.csv", row.names = F)
