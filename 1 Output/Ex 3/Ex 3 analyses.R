####Ex 3 Analyses####
SF = read.csv("Ex 3 Output Scored/SF.csv")
control = read.csv("Ex 3 Output Scored/Control.csv")

#control = control[ , -15]

##Load libraries
library(data.table)
library(ez)
library(reshape)

##Get sample size
length(unique(SF$Sub.ID)) #51
length(unique(control$Sub.ID)) #43

SF$Scored = SF$Scored * 100
control$Scored = control$Scored * 100

summary(SF)
summary(control)

table(SF$Response.JOL)

SF$Response.JOL[SF$Response.JOL == "3 5"] = NA

SF$Response.JOL = as.numeric(SF$Response.JOL)

#Remove out of range JOLs
SF$Response.JOL[SF$Response.JOL > 100] = NA
control$Response.JOL[control$Response.JOL > 100] = NA

##Check for outliers and fuck ups
control2 = control[ , c(2, 1, 11, 16)]
SF2 = SF[ , c(2, 1, 11, 16)]

control.long = melt(control2,
                    id.vars = c("Sub.ID", "Trial.ID"))

colnames(control.long)[3:4] = c("TASK", "SCORE")

control3 = cast(control.long, Sub.ID ~ TASK, mean, na.rm = T)

#cut outliers

#w215057
#w10089980

control3 = subset(control3,
                  control3$Sub.ID != "w215057")
control3 = subset(control3,
                  control3$Sub.ID != "w10089980")

##Now check the SF group
SF.long = melt(SF2,
                    id.vars = c("Sub.ID", "Trial.ID"))

colnames(SF.long)[3:4] = c("TASK", "SCORE")

SF3 = cast(SF.long, Sub.ID ~ TASK, mean, na.rm = T)

#remove outliers
SF3 = subset(SF3,
              SF3$Sub.ID != "w10029010_ES")
SF3 = subset(SF3,
             SF3$Sub.ID != "5ee8e96f94cc2222aadb99af")
SF3 = subset(SF3,
             SF3$Sub.ID != "w10038764")
SF3 = subset(SF3,
             SF3$Sub.ID != "5f21cd95e4a314617a2b2067")
SF3 = subset(SF3,
             SF3$Sub.ID != "w10022439_mlr")
SF3 = subset(SF3,
             SF3$Sub.ID != "asdfadf")

##Get new n's
length(unique(SF3$Sub.ID))
length(unique(control3$Sub.ID))

##Cut these guys out of the SF and control
#SF
SF = subset(SF,
             SF$Sub.ID != "w10029010_ES")
SF = subset(SF,
             SF$Sub.ID != "5ee8e96f94cc2222aadb99af")
SF = subset(SF,
             SF$Sub.ID != "w10038764")
SF = subset(SF,
             SF$Sub.ID != "5f21cd95e4a314617a2b2067")
SF = subset(SF,
             SF$Sub.ID != "w10022439_mlr")
SF = subset(SF,
            SF$Sub.ID != "asdfadf")

#Control
control = subset(control,
                  control$Sub.ID != "w215057")
control = subset(control,
                  control$Sub.ID != "w10089980")

##Get means
##JOLs
mean(SF$Response.JOL, na.rm = T)
mean(control$Response.JOL, na.rm = T)

##Recall
mean(SF$Scored, na.rm = T)
mean(control$Scored, na.rm = T)

##Now look at each manipulation
##Size
tapply(SF$Response.JOL, SF$Procedure.Trial.Type, mean, na.rm = T) 
tapply(SF$Scored, SF$Procedure.Trial.Type, mean, na.rm = T)

x = sd(SF3$Scored, na.rm = T) / sqrt(length(unique(SF3$Sub.ID)))
x2 = x * 1.96

y = sd(SF3$Response.JOL, na.rm = T) / sqrt(length(unique(SF3$Sub.ID)))
y2 = y * 1.96

x2;y2

x = sd(control3$Scored, na.rm = T) / sqrt(length(unique(control3$Sub.ID)))
x2 = x * 1.96

y = sd(control3$Response.JOL, na.rm = T) / sqrt(length(unique(control3$Sub.ID)))
y2 = y * 1.96

x2;y2

x = tapply(SF$Response.JOL, SF$Procedure.Trial.Type, sd, na.rm = T) 
y = tapply(SF$Scored, SF$Procedure.Trial.Type, sd, na.rm = T)

x2 = x / sqrt(length(unique(SF$Sub.ID)))
y2 = y / sqrt(length(unique(SF$Sub.ID)))

x3 = x2 * 1.96
y3 = y2 * 1.96

x3;y3

##SEPARATE OUT THE SF GROUPS
SF.long2 = melt(SF,
                id.vars = c("Sub.ID", "Trial.ID", "Procedure.Trial.Type"),
                measure.vars = c("Response.JOL", "Scored"))

colnames(SF.long2)[4:5] = c("task", "score")

SF.standard = subset(SF.long2,
                     SF.long2$Procedure.Trial.Type == "JOL")

SF.SF = subset(SF.long2,
                     SF.long2$Procedure.Trial.Type == "JOL_SF")

SF.SF2 = cast(SF.SF, Sub.ID ~ task, mean, na.rm = T)
SF.standard2 = cast(SF.standard, Sub.ID ~ task, mean, na.rm = T)

##Write to file for excel
#write.csv(SF3, file = "sf group.csv", row.names = F)
#write.csv(control3, file = "control group.csv", row.names = F)
#write.csv(SF.SF2, file = "Mixed SF.csv", row.names = F)
#write.csv(SF.standard2, file = "Mixed Standard.csv", row.names = F)
