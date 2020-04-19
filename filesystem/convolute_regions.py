import os
import sys
import csv
from operator import itemgetter 
import numpy as np
from scipy.signal import medfilt
from scipy import arange
from lmfit import Model
from numpy import random,exp,loadtxt,pi,sqrt
#import matplotlib.pyplot as plt
from datetime import timedelta, datetime,date
italy_data =    csv.reader(open(os.getcwd()+"/Global/Europe/Italy/italy_curve.tsv","rb"), delimiter = '\t')
# ontario_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Ontario/accumulated.txt","rb"), delimiter = '\t'), "Ontario"]
# canada_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/accumulated.txt","rb"), delimiter = '\t'), "Canada"]
# us_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/United_States_of_America/accumulated.txt","rb"), delimiter = '\t'), "United States of America"]
# cali_data = [csv.reader(open(os.getcwd()+"/Global/North_America/United_States_of_America/California/accumulated.txt","rb"), delimiter = '\t'), "California"]
# china_data = [csv.reader(open(os.getcwd()+"/Global/Asia/China/accumulated.txt","rb"), delimiter = '\t'), "China"]
# ns_data =  [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Nova_Scotia/accumulated.txt","rb"), delimiter = '\t'), "Nova Scotia"]
# italian_data = [csv.reader(open(os.getcwd()+"/Global/Europe/Italy/accumulated.txt","rb"), delimiter = '\t'), "Italy"]
# mexican_data =[csv.reader(open(os.getcwd()+"/Global/North_America/Mexico/accumulated.txt","rb"), delimiter = '\t'), "Mexico"]
# alberta_data = [csv.reader(open(os.getcwd()+"/Global/North_America/Canada/Alberta/accumulated.txt","rb"), delimiter = '\t'), "Alberta"]
# salvador_data = [csv.reader(open(os.getcwd()+"/Global/North_America/El_Salvador/accumulated.txt","rb"), delimiter = '\t'), "El Salvador"]

# cases = [china_data]
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
    print get_slope(0,italy[0],len(italy)-1,italy[-1])
    print get_slope(0,region[0],len(region)-1,region[-1])
    exit(1)
    return None

italy_curve = np.array([float(item) for item in  Extract(list(italy_data),5,False)])
italy_deriv_curve =  np.diff(italy_curve)/italy_curve[1:]
italy_deriv_curve = np.convolve(medfilt(italy_deriv_curve), np.ones((5,))/5, mode='valid')

continents = ["Other","Europe", "North_America", "South_America", "Oceania","Africa","Asia"]

for root, dirs, files in os.walk(os.getcwd()):
    os.chdir(root)
    for file in files:
        if file == "predicted.tsv":
            os.remove(file)
    os.chdir("..")

for continent in continents:
    if continent is None:
        continue
    for root, dirs, files in os.walk(os.getcwd()+"/"+continent):
        os.chdir(root)
        if files:
            files.sort()
            for file in files:
                #if "Canada" in root:
                    #print file
                if file == "accumulated.txt":
                    accumulated = csv.reader(open(root+"/"+file,"rb"), delimiter = '\t')
                    data = list(accumulated)
                    region_confirmed = (Extract(data,1,True), "Confirmed")
                    region_death = (Extract(data,2,True), "Deaths")
                    total_rows = list()
                    for values in [region_confirmed, region_death]:
                        value_name = values[1]
                        values = values[0]
                        values = [item for item in filter(lambda item: item[0] != 'N/A' and item[0] != "NA" and item[0] != 0.0 and item[0] != '0' and item[0] != '0.0', values)]
                        values.sort(key=lambda L: datetime.strptime(L[1], "%Y-%m-%d"))
                        #print values
                        if len(values) <= 1:
                            continue 
                        generate_projected_rates = True
                        todays_value = float(values[-1][0])
                        if value_name == "Confirmed":
                            confirmed_diffdate = values[-1][1]
                        if value_name == "Deaths":
                            if (float(values[-1][0]) < 50.0) and ((float(values[-1][0]) > 3.0)) :
                                generate_projected_rates = False
                                projected_rates = italy_deriv_curve[day-10:]
                        original = [float(item[0]) for item in values]
                        original_diff = np.diff(original)
                        original = medfilt(original,3)
                        if len(original_diff) > 14:
                            original = np.convolve(original, np.ones((3,))/3, mode='valid') 
                            original = np.convolve(original, np.ones((5,))/5, mode='valid')
                        else:
                            original = medfilt(original,3)
                        confirmed_diff = np.diff(original)
                        if len(confirmed_diff) > 14:
                            confirmed_diff = np.convolve(confirmed_diff, np.ones((3,))/3, mode='valid') 
                            #confirmed_diff = medfilt(confirmed_diff,5)
                            confirmed_diff = np.convolve(confirmed_diff, np.ones((5,))/5, mode='valid') 
                        elif len(confirmed_diff) > 11:
                            confirmed_diff = medfilt(confirmed_diff,3)
                            confirmed_diff = medfilt(confirmed_diff,5)
                        elif len(confirmed_diff) > 9:
                                confirmed_diff = medfilt(confirmed_diff,1)

                        if generate_projected_rates:
                            region_curve = np.diff(confirmed_diff)/confirmed_diff[:-1]
                            region_curve = np.nan_to_num(region_curve)
                            if len(region_curve) > 7:
                                region_curve = medfilt(region_curve,3)
                                region_curve = medfilt(region_curve,5)
                            if region_curve.any(): 
                                region_median = np.median(region_curve[-1])
                            else:
                                region_median = 0.0
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
                        if confirmed_diff.any():
                            for rate in projected_rates:
                                if not region_projected:
                                    new_value = abs(confirmed_diff[-1])
                                else:
                                    new_value = region_projected[-1] + region_projected[-1]*rate
                                region_projected.append(new_value)
                        elif original_diff.any():
                            for rate in projected_rates:
                                if not region_projected:
                                    new_value = abs(original_diff[-1])
                                else:
                                    new_value = region_projected[-1] + region_projected[-1]*rate
                                region_projected.append(new_value)
                        # plt.plot(np.array(get_day_count(region_curve,0)), np.array(region_curve), 'k--', label='outbreak rate')
                        # plt.show()
                        # plt.plot(np.array(get_day_count(confirmed_diff,0)), np.array(confirmed_diff), 'r-', label='actual')
                        # plt.plot(np.array(get_day_count(region_projected,len(confirmed_diff)-1)), np.array(region_projected), 'k--', label='projected')
                        # plt.xlabel("Day",fontsize=18)
                        # plt.ylabel(value_name,fontsize=16)
                        # plt.title(name,fontsize=22)
                        # plt.show()                            
                        total_rows.append([region_projected,todays_value])

                    with open(root+"/"+"predicted.tsv","wb") as tsv_file:    
                        writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                        headers = ["Date", "Predicted_Daily_Cases", "Predicted_Total_Cases", "Predicted_Daily_Deaths", "Predicted_Total_Deaths"]
                        print root
                        print total_rows
                        writer.writerow(headers)
                        if len(total_rows) < 1:
                            death_projected = None
                            death = 0.0
                            confirmed_projected = None
                            confirmed = 0.0
                        elif len(total_rows) < 2:
                            death_projected = None
                            death = 0.0
                            confirmed_projected = total_rows[0][0]
                            confirmed = total_rows[0][1]
                        else:
                            death_project = total_rows[1][0]
                            death = total_rows[1][1]
                            confirmed_projected = total_rows[0][0]
                            confirmed = total_rows[0][1]
                        death_index = 0
                        date = date.today()
                        if total_rows:
                            for row in confirmed_projected:
                                confirmed += round(row,3)
                                if death_project:
                                    if death_index < len(death_project):
                                        death += round(death_project[death_index],3)
                                        writer.writerow([date,round(row,3),round(confirmed,3),round(death_project[death_index],3),round(death,3)])
                                        date = date + timedelta(1)
                                        death_index += 1
                                    else:
                                        writer.writerow([date,round(row,3),round(confirmed,3),0.0,round(death,3)])
                                        date = date + timedelta(1)
                                else:
                                    writer.writerow([date,round(row,3),round(confirmed,3),0.0,round(death,3)])
                                    date = date + timedelta(1)
                            if death_project:
                                if death_index < len(death_project):
                                    for row in death_project[death_index:-1]:
                                            death += round(death_project[death_index],3)
                                            writer.writerow([date,0.0,round(confirmed,3),round(death_project[death_index],3),round(death,3)])
                                            date = date + timedelta(1)
                                            death_index += 1
                            august = datetime.strptime("2020-08-31","%Y-%m-%d").date()
                            while date < august:
                                writer.writerow([date,0.0,round(confirmed,3),0.0,round(death,3)])
                                date = date + timedelta(1)

                        else:
                            august = datetime.strptime("2020-08-31","%Y-%m-%d").date()
                            while date < august:
                                writer.writerow([date,0.0,0.0,0.0,0.0])
                                date = date + timedelta(1)
        os.chdir("..")
    os.chdir("..")


                    