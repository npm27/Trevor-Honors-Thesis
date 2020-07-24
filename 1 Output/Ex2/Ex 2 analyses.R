####Ex 2 Analyses####
size = read.csv("Ex 2 Scored Output/Size.csv")
highlight = read.csv("Ex 2 Scored Output/Highlight.csv")
#control = read.csv("JOL3.csv")

##Get sample size
length(unique(size$Username))
length(unique(highlight$Username))
#length(unique(control$Username))

colnames(size)[7] = "Direction"
colnames(highlight)[7] = "Direction"
#colnames(control)[7] = "Direction"

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
#summary(control)

#Remove out of range JOLs
size$Response.JOL[size$Response.JOL > 100] = NA
highlight$Response.JOL[highlight$Response.JOL > 100] = NA
#control$Response.JOL[control$Response.JOL > 100] = NA

##Get means
##JOLs
mean(size$Response.JOL)
mean(highlight$Response.JOL)
#cONTROL WILL GO HERE

##Recall
mean(size$Recall_Score)
mean(highlight$Recall_Score)

##Now look at each manipulation
##Size
tapply(size$Response.JOL, size$Procedure.Trial.Type, mean, na.rm = T)
tapply(size$Recall_Score, size$Procedure.Trial.Type, mean, na.rm = T)

##Highlight
tapply(highlight$Response.JOL, highlight$Procedure.Trial.Type, mean, na.rm = T)
tapply(highlight$Recall_Score, highlight$Procedure.Trial.Type, mean, na.rm = T)

##Control
#tapply(control$Response.JOL, control$Direction, mean, na.rm = T)
#tapply(control$Recall_Score, control$Direction, mean, na.rm = T)
