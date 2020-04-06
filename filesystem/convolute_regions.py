import os
import sys
import csv
from operator import itemgetter 
import numpy as np
from scipy.signal import medfilt
from scipy import arange
from lmfit import Model
from numpy import random,exp,loadtxt,pi,sqrt
import matplotlib.pyplot as plt
from datetime import timedelta, datetime
italy_data =    csv.reader(open(os.getcwd()+"/Global/Europe/Italy/italy_curve.tsv","rb"), delimiter = '\t')
ontario_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Ontario/accumulated.txt","rb"), delimiter = '\t'), "Ontario"]
canada_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/accumulated.txt","rb"), delimiter = '\t'), "Canada"]
us_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/United_States_of_America/accumulated.txt","rb"), delimiter = '\t'), "United State of America"]
cali_data = [csv.reader(open(os.getcwd()+"/Global/North_America/United_States_of_America/California/accumulated.txt","rb"), delimiter = '\t'), "California"]
china_data = [csv.reader
(open(os.getcwd()+"/Global/Asia/China/accumulated.txt","rb"), delimiter = '\t'), "China"]
ns_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Nova_Scotia/accumulated.txt","rb"), delimiter = '\t'), "Nova Scotia"]
italian_data = [csv.reader(open(os.getcwd()+"/Global/Europe/Italy/accumulated.txt","rb"), delimiter = '\t'), "Italy"]
mexican_data =[csv.reader(open(os.getcwd()+"/Global/North_America/Mexico/accumulated.txt","rb"), delimiter = '\t'), "Mexico"]
cases = [italian_data]

def Extract(lst, index):
    return [item[index] for item in lst[1:]] 

def get_day_count(lst,value):
    days = []
    for i in range(0,len(lst)):
        days.append(float(i+value))
    return days


def gaussian(i, amp, cen, wid):
    """1-d gaussian: gaussian(x, amp, cen, wid)"""
    return (amp / (sqrt(2*pi) * wid)) * exp(-(i-cen)**2 / (2*wid**2))


def get_slope(x1,y1,x2,y2):
    return (y2-y1)/(x2-x1)

def derivative_slope (lst):
    derive = []
    for i in range(1,len(lst)):
        derive.append((lst[i]-lst[i-1])/lst[i])
    return np.array(derive)


italy_curve = np.array([float(item) for item in  Extract(list(italy_data),5)])
italy_deriv_curve =  np.diff(italy_curve)/italy_curve[1:]
italy_deriv_curve = np.convolve(medfilt(italy_deriv_curve), np.ones((5,))/5, mode='valid')
# plt.plot(np.array(get_day_count(italy_curve,0)), np.array(italy_curve), 'r-', label='projected')
# plt.xlabel("Day",fontsize=18)
# plt.ylabel("%  of New Cases",fontsize=16)
# plt.title("Italy Max Weighted New Cases Prediction",fontsize=22)
# plt.show()
# plt.plot(np.array(get_day_count(italy_deriv_curve,0)), np.array(italy_deriv_curve), 'r-', label='projected')
# plt.xlabel("Day",fontsize=18)
# plt.ylabel("Rate of Outbreak",fontsize=16)
# plt.title("Italy Max Weighted Outbreak Rate Prediction",fontsize=22)
# plt.show()
for region, name in cases:
    data = list(region)
    region_confirmed = Extract(data,1)
    active_cases = Extract(data,4)
    for item in region_confirmed:
        if item == 'N/A':
            region_confirmed.remove(item)
    last_index = 0


    region_confirmed = [float(item) for item in region_confirmed]

    med_region_confirmed = np.convolve(medfilt(np.diff(region_confirmed)), np.ones((3,))/3, mode='valid')
    med_region_confirmed = np.convolve(medfilt(med_region_confirmed), np.ones((3,))/3, mode='valid')

    active_cases = filter(lambda a: a != 'N/A', active_cases)
    active_cases = [float(item) for item in active_cases]
    if all(v == 0.0 for v in active_cases):
        active_cases = np.convolve(medfilt(region_confirmed), np.ones((3,))/3, mode='valid')
    else:
        active_cases = np.convolve(medfilt(active_cases), np.ones((3,))/3, mode='valid')

    region_curve = derivative_slope(active_cases)
    region_curve = np.convolve(medfilt(region_curve), np.ones((3,))/3, mode='valid')
    region_curve = derivative_slope(region_curve)
    region_curve = np.convolve(medfilt(region_curve), np.ones((3,))/3, mode='valid')
    region_curve = np.convolve(medfilt(region_curve), np.ones((5,))/5, mode='valid')
    region_curve = np.convolve(medfilt(region_curve), np.ones((7,))/7, mode='valid')
    region_median = np.median(region_curve[-3:-1])

    i = 0
    day = 0
    closest_match = None
    for value in italy_deriv_curve:
        diff = abs(value-region_median)
        if not closest_match:
            closest_match = diff
        else:
            if diff < closest_match:
                closest_match = diff
                day = i + 1
            i += 1
    projected_rates = italy_deriv_curve[day:]
    region_projected = []
    print projected_rates

    for rate in projected_rates:
        if not region_projected:
            new_value = med_region_confirmed[-1]
        else:
            new_value = region_projected[-1] + region_projected[-1]*rate
        region_projected.append(new_value)

    plt.plot(np.array(get_day_count(region_curve,0)), np.array(region_curve), 'k--', label='outbreak rate')
    plt.show()
    plt.plot(np.array(get_day_count(med_region_confirmed,0)), np.array(med_region_confirmed), 'k--', label='actual')
    plt.plot(np.array(get_day_count(region_projected,len(med_region_confirmed))), np.array(region_projected), 'r-', label='projected')
    plt.xlabel("Day",fontsize=18)
    plt.ylabel("New Cases",fontsize=16)
    plt.title(name,fontsize=22)
    plt.show()
    with open(name+"_prediction_data.tsv","wb") as tsv_file:
        date = datetime.strptime("03-15-2020", "%m-%d-%Y")
        writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        for row in active_cases:
            writer.writerow([date,row])
            date = date + timedelta(1)
        for row in region_projected:
            writer.writerow([date,row])
            date = date + timedelta(1)
    exit(1)



