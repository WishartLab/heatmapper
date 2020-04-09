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
us_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/United_States_of_America/accumulated.txt","rb"), delimiter = '\t'), "United States of America"]
cali_data = [csv.reader(open(os.getcwd()+"/Global/North_America/United_States_of_America/California/accumulated.txt","rb"), delimiter = '\t'), "California"]
china_data = [csv.reader(open(os.getcwd()+"/Global/Asia/China/accumulated.txt","rb"), delimiter = '\t'), "China"]
ns_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Nova_Scotia/accumulated.txt","rb"), delimiter = '\t'), "Nova Scotia"]
italian_data = [csv.reader(open(os.getcwd()+"/Global/Europe/Italy/accumulated.txt","rb"), delimiter = '\t'), "Italy"]
mexican_data =[csv.reader(open(os.getcwd()+"/Global/North_America/Mexico/accumulated.txt","rb"), delimiter = '\t'), "Mexico"]
alberta_data = [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Alberta/accumulated.txt","rb"), delimiter = '\t'), "Alberta"]
salvador_data = [csv.reader(open(os.getcwd()+"/Global/North_America/El_Salvador/accumulated.txt","rb"), delimiter = '\t'), "El Salvador"]
quebec_data = [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Quebec/accumulated.txt","rb"), delimiter = '\t'), "Quebec"]
sask_data = [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Saskatchewan/accumulated.txt","rb"), delimiter = '\t'), "Saskatchewan"]
bc_data = [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/British_Columbia/accumulated.txt","rb"), delimiter = '\t'), "British Columbia"]
germany_data = [csv.reader(open(os.getcwd()+"/Global/Europe/Germany/accumulated.txt","rb"), delimiter = '\t'), "Germany"]
yukon_data = [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Yukon/accumulated.txt","rb"), delimiter = '\t'), "Yukon"]
cases = [yukon_data]

def Extract(lst, index,date):
    if date:
        return [(item[index],item[-1]) for item in lst[1:]]
    else:
        return  [(item[index]) for item in lst[1:]]
def get_day_count(lst,value):
    days = []
    for i in range(0,len(lst)):
        days.append(float(i+value))
    return days
def get_slope(x1,y1,x2,y2):
    return (y2-y1)/(x2-x1)
def derivative_slope (lst):
    derive = []
    for i in range(1,len(lst)):

        derive.append((lst[i]-lst[i-1])/lst[i])
    return np.array(derive)
def get_stretch_factor(italy,region):
    print("Done")
    print get_slope(0,italy[0],len(italy)-1,italy[-1])
    print get_slope(0,region[0],len(region)-1,region[-1])
    exit(1)
    return None

italy_curve = np.array([float(item) for item in  Extract(list(italy_data),5,False)])
italy_deriv_curve =  np.diff(italy_curve)/italy_curve[1:]
italy_deriv_curve = np.convolve(medfilt(italy_deriv_curve), np.ones((5,))/5, mode='valid')

print cases

for region,name in cases: 
    data = list(region)
    region_confirmed = Extract(data,1,True)
    region_confirmed = [item for item in filter(lambda item: item[0] != 'N/A' and item[0] != "NA" and item[0] != 0.0 and item[0] != '0', region_confirmed)]
    if len(region_confirmed) < 5:
        continue
    region_confirmed.sort(key=lambda L: datetime.strptime(L[1], "%Y-%m-%d"))
    #print region_confirmed
    diffdate = region_confirmed[0][1] 
    original_diff = [float(item[0]) for item in region_confirmed]
    original_diff = medfilt(original_diff,3)
    #original_diff = medfilt(original_diff,5)
    
    if len(original_diff) > 14:
        original_diff = np.convolve(original_diff, np.ones((3,))/3, mode='valid') 
        original_diff = np.convolve(original_diff, np.ones((5,))/5, mode='valid')
    else:
        original_diff = medfilt(original_diff,3)
        #original_diff = medfilt(confirmed_diff,5)
    print original_diff
    confirmed_diff = np.diff(original_diff)
    if len(confirmed_diff) > 14:
        confirmed_diff = np.convolve(confirmed_diff, np.ones((3,))/3, mode='valid') 
        confirmed_diff = medfilt(confirmed_diff,5)
    elif len(confirmed_diff) > 11:
        confirmed_diff = medfilt(confirmed_diff,3)
        confirmed_diff = medfilt(confirmed_diff,5)
    elif len(confirmed_diff) > 9:
            confirmed_diff = medfilt(confirmed_diff,3)
    print confirmed_diff
    region_curve = np.diff(confirmed_diff)/confirmed_diff[:-1]
    region_curve = np.nan_to_num(region_curve)
    if len(region_curve) > 7:
        region_curve = medfilt(region_curve,3)
        region_curve = medfilt(region_curve,5)
    if region_curve.any(): 
        region_median = np.median(region_curve[-1])
    else:
        region_median = 0.0
    print region_median
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
    #print region_curve
    #get_stretch_factor(italy_deriv_curve[(day-7):day],region_curve[-8:])
    #print projected_rates
    if confirmed_diff.any():
        for rate in projected_rates:
            if not region_projected:
                new_value = confirmed_diff[-1]
            else:
                new_value = region_projected[-1] + region_projected[-1]*rate
            region_projected.append(new_value)
    elif original_diff.any():
        for rate in projected_rates:
            if not region_projected:
                new_value = original_diff[-1]
            else:
                new_value = region_projected[-1] + region_projected[-1]*rate
            region_projected.append(new_value)
    print region_projected
    plt.plot(np.array(get_day_count(region_curve,0)), np.array(region_curve), 'k--', label='outbreak rate')
    plt.show()
    plt.plot(np.array(get_day_count(original_diff,0)), np.array(original_diff), 'r-', label='actual')
    plt.plot(np.array(get_day_count(region_projected,len(original_diff)-1)), np.array(region_projected), 'k--', label='projected')
    plt.xlabel("Day",fontsize=18)
    plt.ylabel("New Cases",fontsize=16)
    plt.title(name,fontsize=22)
    plt.show()
    # with open("predicted.tsv","wb") as tsv_file:
    #     date = datetime.strptime(diffdate,"%Y-%m-%d")
    #     makeup = datetime.strptime("2020-03-15","%Y-%m-%d")
    #     while makeup < date:
    #         writer.writerow([makeup,0.0])
    #         makeup = makeup + timedelta(1)
    #     writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    #     for row in original_diff:
    #         writer.writerow([date.date(),row])
    #         date = date + timedelta(1)
    #     for row in region_projected:
    #         writer.writerow([date.date(),round(row,3)])
    #         date = date + timedelta(1)
    #     print date
    #     august = datetime.strptime("2020-08-31","%Y-%m-%d").date()
    #     print august
    #     while date.date() < august:
    #         writer.writerow([date.date(),0.0])
    #         date = date + timedelta(1)



