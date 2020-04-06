import os
import sys
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import leastsq
import csv
from datetime import datetime
from numpy import random,exp,loadtxt,pi,sqrt
import seaborn as sns
from scipy.stats import norm
from sklearn.preprocessing import MinMaxScaler
import pandas as pd
from scipy.signal import medfilt
from scipy import arange
from lmfit import Model


italian_data = []
for filename in os.listdir("COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"):
    if ".csv" not in filename:
        continue
    with open("COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/" + filename) as csv_file:
        reader = csv.reader(csv_file, delimiter=',')
        for row in reader:
            if datetime.strptime((filename.replace(".csv","")) , "%m-%d-%Y") <= datetime.strptime("03-21-2020", "%m-%d-%Y"):
                if row[1]== "Italy":
                    italian_data.append([row[1], row[2], int(row[3]), int(row[4]), int(row[5])])
            else:
                if row[3] == "Italy":
                    if filename == "03-22-2020.csv":
                        italian_data.append([row[3],"2020-03-22", int(row[7]), int(row[8]), int(row[9])])
                    else:
                        italian_data.append([row[3],row[4], int(row[7]), int(row[8]), int(row[9])])

italian_data.sort(key=lambda x: x[1])
italian_data = italian_data[30:-1]
for row in italian_data:
    row.append(int(row[2]-row[3]-row[4]))
    
active_cases = []
prev_case = None
max_value = 0



for row in italian_data:
    if prev_case:
        print row[2]-prev_case
        active_cases.append(row[2]-prev_case)
    else:
        active_cases.append(row[2])
    prev_case = row[2]


active_cases = np.convolve(medfilt(active_cases), np.ones((3,))/3, mode='valid')
print active_cases
#max_value = max(active_cases)
#max_curve = []
#for row in active_cases:
#    max_curve.append(float(row)/float(max_value))
#y = max_curve

y = active_cases
x= []

for i in range(0,len(active_cases)):
    x.append(i)

X = np.array(x)
Y = np.array(active_cases)
print X
print Y

def gaussian(i, amp, cen, wid):
    """1-d gaussian: gaussian(x, amp, cen, wid)"""
    return (amp / (sqrt(2*pi) * wid)) * exp(-(i-cen)**2 / (2*wid**2))


gmodel = Model(gaussian)
result = gmodel.fit(Y, i=X, amp=21, cen=29, wid=8.7)
params = result.params.valuesdict()
amp = params.items()[0][1]
cen = params.items()[1][1]
wid = params.items()[2][1]
new_y = []
new_x = []
print amp, cen, wid
for i in range(len(X)-1, len(X)+60):
    value = gaussian(i,amp,cen,wid) 
    new_y.append(value)
    new_x.append(i)

new_Y = np.array(new_y)
new_X = np.array(new_x)
plt.plot(x, y, 'bo')
plt.plot(new_X,new_Y, 'k--', label='predicted fit')
plt.plot(X, result.best_fit, 'r-', label='best fit')
plt.legend(loc='best')
plt.show()
print new(x)
for i in range (0,len(new_x)):
    print [new_x[i],new_y[i]]

