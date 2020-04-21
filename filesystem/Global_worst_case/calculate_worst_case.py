import os
import sys
import csv
from scipy.signal import medfilt
from scipy import arange
from lmfit import Model 
from datetime import timedelta, datetime, date
import json 

continents = ["Other","Europe", "North_America", "South_America", "Oceania","Africa","Asia"]

for root, dirs, files in os.walk(os.getcwd()):
    os.chdir(root)
    for file in files:
        if (file == "accumulated.txt") or (file == "predicted.tsv"):
            os.remove(file)
    os.chdir("..")
os.chdir("..")
with open('start_dates.json') as json_file:
    country_dates = json.load(json_file)


country_population = dict()
region_file = csv.reader(open("country_data.csv","rb"), delimiter = '\t')
data = list(region_file)
for item in data:
    population = float(item[1]) 
    country_population[item[0]] = population 
        
percentage_curve = list()
region_file = csv.reader(open("canada_curve.tsv","rb"), delimiter = '\t')
next(region_file)
data = list(region_file)
for item in data:
    percentage_curve.append(item[-1])

headers = ["Date", "Cases","Cases_per_capita" "Total_Cases", "Total_Cases_per_capita",
            "Deaths", "Deaths_per_capita", "Total_Deaths", "Total_Deaths_per_capita"]
other_headers = ["Name", "Cases","Cases_per_capita" "Total_Cases", "Total_Cases_per_capita",
            "Deaths", "Deaths_per_capita", "Total_Deaths", "Total_Deaths_per_capita"]
for key in country_dates:
    if not key in country_population.keys():
        continue
    population = float(country_population[key])
    total_cases = 0.0
    total_deaths = 0.0
    date = datetime.strptime(country_dates[key],"%Y-%m-%d").date()
    accumulated_file = None
    for continent in continents:
        if os.path.exists(continent+"/"+key.replace(" ", "_")+"/"):
            accumulated_file = open(continent+"/"+key.replace(" ", "_")+"/"+"predicted.tsv","w")
            accumulated_writer = csv.writer(accumulated_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            continental_dir = continent
    if not accumulated_file:
        continue
    accumulated_writer.writerow(headers)
    death_date = 0
    print key
    for day in percentage_curve:
        day = float(day)
        cases_pc = float(day)
        cases = float(day) * population
        total_cases += cases
        total_cases_pc = total_cases / population
        str_date = date.strftime("%Y-%m-%d") 
        if death_date < 10:
            deaths = 0
            total_deaths = 0
            deaths_pc = 0
            total_deaths_pc = 0
        else:
            deaths = float(percentage_curve[death_date-10]) * population  * 0.006
            total_deaths += deaths
            deaths_pc = deaths / population
            total_deaths_pc = total_deaths / population
        accumulated_writer.writerow([str_date,round(cases,3),round(cases_pc,8),round(total_cases,3),round(total_cases_pc,8),
                                        round(deaths,3),round(deaths_pc,8),round(total_deaths,3),round(total_deaths_pc,8)])
        continental_file_name = continental_dir+"/"+continental_dir+"_"+str_date+".txt"
        if not os.path.exists(continental_file_name):
            continental_writer = csv.writer(open(continental_file_name,"wb"), delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            continental_writer.writerow(other_headers)
        with open(continental_file_name,"a") as continental_file:
            continental_writer = csv.writer(continental_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            continental_writer.writerow([key,round(cases,3),round(cases_pc,8),round(total_cases,3),round(total_cases_pc,8),
                                    round(deaths,3),round(deaths_pc,8),round(total_deaths,3),round(total_deaths_pc,8)])
        global_file_name = "Global-Country"+"_"+str_date+".txt"
        if not os.path.exists(global_file_name):
            global_writer = csv.writer(open(global_file_name,"wb"), delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            global_writer.writerow(other_headers)
        with open(global_file_name,"a") as global_file:
            global_writer = csv.writer(global_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            global_writer.writerow([key,round(cases,3),round(cases_pc,8),round(total_cases,3),round(total_cases_pc,8),
                                    round(deaths,3),round(deaths_pc,8),round(total_deaths,3),round(total_deaths_pc,8)])
        death_date += 1
        date = date + timedelta(1)
    death_date = 0
    death_remain = percentage_curve[-10:]
    for day in death_remain:
        deaths =  float(day) * population * 0.006
        total_deaths += deaths
        deaths_pc = deaths / population
        total_deaths_pc = total_deaths / population
        str_date = date.strftime("%Y-%m-%d") 
        cases = 0.0
        cases_pc = 0.0
        accumulated_writer.writerow([str_date,round(cases,3),round(cases_pc,8),round(total_cases,3),round(total_cases_pc,8),
                                        round(deaths,3),round(deaths_pc,8),round(total_deaths,3),round(total_deaths_pc,8)])
        with open(continental_file_name,"a") as continental_file:
            continental_writer = csv.writer(continental_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            continental_writer.writerow([key,round(cases,3),round(cases_pc,8),round(total_cases,3),round(total_cases_pc,8),
                                    round(deaths,3),round(deaths_pc,8),round(total_deaths,3),round(total_deaths_pc,8)])
        with open(global_file_name,"a") as global_file:
            global_writer = csv.writer(global_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            global_writer.writerow([key,round(cases,3),round(cases_pc,8),round(total_cases,3),round(total_cases_pc,8),
                                    round(deaths,3),round(deaths_pc,8),round(total_deaths,3),round(total_deaths_pc,8)])
        death_date += 1
        date = date + timedelta(1)
    #accumulated_file.close()

        



