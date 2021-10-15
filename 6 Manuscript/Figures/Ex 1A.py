##set up
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

##Do Font-Size (Ex 1A)

dat = pd.read_csv("Ex 1A.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(14,10) #Might need to tweak this

fig.subplots_adjust(hspace = .45) #Controls space between sub plots

fig.suptitle('Font-Size', fontsize=30, fontweight = 'bold')


##Make the subplots
ax1 = fig.add_subplot(3, 1, 1)
ax2 = fig.add_subplot(3, 1, 2)
ax3 = fig.add_subplot(3, 1, 3)

#Subset by Encoding
small = dat[dat['Encoding'] == 'Small']
large = dat[dat['Encoding'] == 'Large']
control = dat[dat['Encoding'] == 'Control']

#subset by task
#small
j1 = small[small['Task'] == 'JOL']
r1 = small[small['Task'] == 'Recall']

j2 = large[large['Task'] == 'JOL']
r2 = large[large['Task'] == 'Recall']

j3 = control[control['Task'] == 'JOL']
r3 = control[control['Task'] == 'Recall']

#separate out averages and conf interval
j1_average = j1['Average']
r1_average = r1['Average']

j1_conf = j1['diff2']
r1_conf = r1['diff2']

j2_average = j2['Average']
r2_average = r2['Average']

j2_conf = j2['diff2']
r2_conf = r2['diff2']

j3_average = j3['Average']
r3_average = r3['Average']

j3_conf = j3['diff2']
r3_conf = r3['diff2']

ind = np.arange(len(j1_average))  # the x locations for the groups #May need to tweak this
width = 0.35 #bar width  #And tweak this

##start making the plots
rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'navy', edgecolor = 'k',
                label ='JOL')

rects2 = ax1.bar(ind + width/2, r1_average, width, yerr = r1_conf, capsize = 3, color = 'dodgerblue', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax1.set_title('Small Font', fontsize = 18, fontweight = 'bold')
ax1.set_ylabel('Mean % JOL/Recall', fontsize = 16)
ax1.set_xlabel('Direction', fontsize = 16)
ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 14)
ax1.legend(fontsize = 14)
ax1.set_ylim([0,100])

##Large Font
##start making the plots
rects3 = ax2.bar(ind - width/2, j2_average, width, yerr = j2_conf, capsize = 3, color = 'navy', edgecolor = 'k',
                label ='JOL')

rects4 = ax2.bar(ind + width/2, r2_average, width, yerr = r2_conf, capsize = 3, color = 'dodgerblue', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax2.set_title('Large Font', fontsize = 18, fontweight = 'bold')
ax2.set_ylabel('Mean % JOL/Recall', fontsize = 16)
ax2.set_xlabel('Direction', fontsize = 16)
ax2.xaxis.labelpad = 0
ax2.set_xticks(ind)
ax2.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 14)
ax2.legend(fontsize = 14)
ax2.set_ylim([0,100])

##Control
##start making the plots
rects5 = ax3.bar(ind - width/2, j3_average, width, yerr = j3_conf, capsize = 3, color = 'navy', edgecolor = 'k',
                label ='JOL')

rects6 = ax3.bar(ind + width/2, r3_average, width, yerr = r3_conf, capsize = 3, color = 'dodgerblue', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax3.set_title('Control', fontsize = 18, fontweight = 'bold')
ax3.set_ylabel('Mean % JOL/Recall', fontsize = 16)
ax3.set_xlabel('Direction', fontsize = 16)
ax3.xaxis.labelpad = 0
ax3.set_xticks(ind)
ax3.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 14)
ax3.legend(fontsize = 14)
ax3.set_ylim([0,100])

##save figure
fig.savefig('EX1A_chart_updated.png', dip = 10000)