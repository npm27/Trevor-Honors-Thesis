##set up
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

##Do Font-Size (Ex 2A)

dat = pd.read_csv("Ex 3.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(8, 5) #Might need to tweak this

fig.subplots_adjust(hspace = .30) #Controls space between sub plots

##Make the subplots
ax1 = fig.add_subplot(1, 1, 1)

#Subset by Encoding
font_group  = dat[dat['Encoding'] != 'Control']
control = dat[dat['Encoding'] == 'Control']

#subset by task
j1 = dat[dat['Task'] == 'JOL']
r1 = dat[dat['Task'] == 'Recall']

j3 = control[control['Task'] == 'JOL']
r3 = control[control['Task'] == 'Recall']

#separate out averages and conf interval
j1_average = j1['Average']
r1_average = r1['Average']

j1_conf = j1['diff2']
r1_conf = r1['diff2']

j3_average = j3['Average']
r3_average = r3['Average']

j3_conf = j3['diff2']
r3_conf = r3['diff2']

##Control will get its own plot, need to plot small and large side by side

ind = np.arange(len(j1_average))  # the x locations for the groups #May need to tweak this
width = 0.35 #bar width  #And tweak this

rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'w', edgecolor = 'k',
                label ='JOL')

rects2 = ax1.bar(ind + width/2, r1_average, width, yerr = r1_conf, capsize = 3, color = 'grey', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax1.set_title('Experiment 3: Sans Forgetica vs Arial', fontsize = 18, fontweight = 'bold')
ax1.set_ylabel('Mean % JOL/Recall', fontsize = 16)
ax1.set_xlabel('Direction', fontsize = 16)
ax1.xaxis.labelpad = 10
ax1.set_xticks(ind)
ax1.set_xticklabels(('Sans Forgetica', 'Arial', 'Control'), fontsize = 14)
ax1.legend(fontsize = 14)
ax1.set_ylim([0,100])

##save figure
fig.savefig('EX3_chart.png', dip = 10000)
