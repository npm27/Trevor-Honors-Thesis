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

####GAMMAS####
###set up the data
##CONTROL
control_final = na.omit(control2)

control_final = subset(control_final,
                       control_final$Sub.ID != "w215057")
control_final = subset(control_final,
                       control_final$Sub.ID != "w10089980")
length(unique(control_final$Sub.ID))

##SF
#Yes
yes = subset(SF,
             SF$Procedure.Trial.Type == "JOL_SF")

yes.final = na.omit(yes)

yes.final = subset(yes.final,
                   yes.final$Sub.ID != "10067786")
yes.final = subset(yes.final,
                   yes.final$Sub.ID != "5efe59519563aa3504e9966a")
yes.final = subset(yes.final,
                   yes.final$Sub.ID != "w10089265")
yes.final = subset(yes.final,
                   yes.final$Sub.ID != "W966659_MER")
yes.final = subset(yes.final,
                   yes.final$Sub.ID != "5eaa97d07eda6e01ec108107")
yes.final = subset(yes.final,
                   yes.final$Sub.ID != "5aa9528235237b0001132675")

length(unique(yes.final$Sub.ID))

#No
no = subset(SF,
             SF$Procedure.Trial.Type == "JOL")

no.final = na.omit(no)

no.final = subset(no.final,
                   no.final$Sub.ID != "10067786")
no.final = subset(no.final,
                   no.final$Sub.ID != "5efe59519563aa3504e9966a")
no.final = subset(no.final,
                   no.final$Sub.ID != "w10089265")
no.final = subset(no.final,
                   no.final$Sub.ID != "W966659_MER")
no.final = subset(no.final,
                   no.final$Sub.ID != "5eaa97d07eda6e01ec108107")
no.final = subset(no.final,
                   no.final$Sub.ID != "5aa9528235237b0001132675")

length(unique(no.final$Sub.ID))

##Compute the gammas
#Control
empty = data.frame()

for (i in unique(control_final$Sub.ID)){
  
  temp = subset(control_final, control_final$Sub.ID == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Scored, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_control = empty

##Yes
empty = data.frame()

for (i in unique(yes.final$Sub.ID)){
  
  temp = subset(yes.final, yes.final$Sub.ID == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Scored, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_yes = empty

##No
empty = data.frame()

for (i in unique(no.final$Sub.ID)){
  
  temp = subset(no.final, no.final$Sub.ID == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Scored, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_no = empty

##Now get mean gammas and CIs
#control
mean(Gammas_control$g, na.rm = T)
(sd(Gammas_control$g, na.rm = T) /sqrt(length(unique(Gammas_control$i)))) * 1.96

#YES
mean(Gammas_yes$g, na.rm = T)
(sd(Gammas_yes$g, na.rm = T) /sqrt(length(unique(Gammas_yes$i)))) * 1.96

#NO
mean(Gammas_no$g, na.rm = T)
(sd(Gammas_no$g, na.rm = T) /sqrt(length(unique(Gammas_no$i)))) * 1.96

##Analyses
Gammas_yes$encoding = rep("yes")
Gammas_no$encoding = rep("no")

Gammas_control$encoding = rep("control")

temp1 = t.test(na.omit(Gammas_control$g), na.omit(Gammas_yes$g), paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1

pbic1 = rbind(Gammas_control, Gammas_yes)
pbic1 = na.omit(pbic1)

ezANOVA(pbic1,
        wid = i,
        between = encoding,
        dv = g,
        detailed = T,
        type = 3)
