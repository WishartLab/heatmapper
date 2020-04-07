import os
import sys
import csv 
import numpy as np
from datetime import timedelta, datetime

continents = ["Other","Europe", "North_America", "South_America", "Oceania","Africa","Asia"]

headers = ["Name","Predicted_New_Cases", "Total_Predicted_New_Cases", "Predicted_New_per_capita", "Predicted_Total_per_capita"]


for root, dirs, files in os.walk(os.getcwd()):
    os.chdir(root)
    date = datetime.date(datetime.now()) + timedelta(-1)
    for file in files:
        if "2020" in file:
            file_date = datetime.strptime(file[-14:-4], "%Y-%m-%d").date()
            if file_date > date:
                os.remove(file)
    os.chdir("..")
os.chdir("..")
global_dir = os.getcwd()

for continent in continents:
    if continent is "Other":
        continue
    print(continent)
    for root, dirs, files in os.walk(os.getcwd()+"/"+continent):
        os.chdir(root)
        #print root
        country = False
        parent = os.path.abspath(os.path.join(root, os.pardir))
        #print parent+"/region_data.csv"
        region_name = root.split("/")[-1].replace("_"," ")
        parent_name = parent.split("/")[-1].replace("_"," ")
        population = []
        if parent.split("/")[-1] in continents or (parent.split("/")[-1] in continents and region_name == parent_name):
            country = True
            if os.path.exists(global_dir+"/country_data.csv"):
                region_file = csv.reader(open(global_dir+"/country_data.csv","rb"), delimiter = '\t')
                data = list(region_file)
                population = [item[1] for item in data if item[0] == region_name]
                if population:
                    population = float(population[0])                
        else:
            if os.path.exists(parent+"/region_data.csv"):
                region_file = csv.reader(open(parent+"/region_data.csv","rb"), delimiter = ',')
                data = list(region_file)
                population = [item[1] for item in data if item[0] == region_name]
                if population:
                    population = float(population[0])
        if files:
            files.sort()           
            for file in files:
                if file == "predicted.tsv":
                    data = csv.reader(open(root+"/"+file, "rb"), delimiter = '\t')
                    startdate = datetime.date(datetime.now())
                    max_data = 90
                    #print startdate
                    total_new_cases = 0                
                    for row in data:
                        #print row
                        if datetime.strptime(row[0],"%Y-%m-%d").date() < startdate:
                            continue
                        print parent.split("/")[-1]
                        print parent+"/"+parent.split("/")[-1]+"_"+row[0]+".txt"
                        print global_dir+"/"+"Global-Country_"+row[0]+".txt"
                        with open(parent+"/"+parent.split("/")[-1]+"_"+row[0]+".txt",'a+') as tsv_file:
                            writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                            if os.path.getsize(parent+"/"+parent.split("/")[-1]+"_"+row[0]+".txt") == 0:
                                writer.writerow(headers)
                            new_case = float(row[1])
                            total_new_cases += new_case
                            new_case_pc = "N/A"
                            total_pc = "N/A"
                            if population:
                                new_case_pc = round(new_case/population,8)
                                total_pc = round(total_new_cases/population,8)
                            writer.writerow([root.split("/")[-1].replace("_", " "), round(new_case,3), round(total_new_cases,3), new_case_pc, total_pc])
                        if country:
                            with open(global_dir+"/"+"Global-Country_"+row[0]+".txt",'a+') as tsv_file:
                                writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                                if os.path.getsize(global_dir+"/"+"Global-Country_"+row[0]+".txt") == 0:
                                    writer.writerow(headers)
                                new_case = float(row[1])
                                total_new_cases += new_case
                                new_case_pc = "N/A"
                                total_pc = "N/A"
                                if population:
                                    new_case_pc = round(new_case/population,8)
                                    total_pc = round(total_new_cases/population,8)
                                writer.writerow([root.split("/")[-1].replace("_", " "), round(new_case,3), round(total_new_cases,3), new_case_pc, total_pc])
        os.chdir("..")
    os.chdir("..")