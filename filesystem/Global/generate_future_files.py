import os
import sys
import csv 
import numpy as np
from datetime import timedelta, datetime

continents = ["Other","Europe", "North_America", "South_America", "Oceania","Africa","Asia"]

headers = ["Name","Predicted_New_Cases", "Total_Predicted_New_Cases"]


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
        if files:
            files.sort()
            for file in files:
                if file == "predicted.tsv":
                    data = csv.reader(open(root+"/"+file, "rb"), delimiter = '\t')
                    startdate = datetime.date(datetime.now()) + timedelta(-1)
                    max_data = 90
                    #print startdate
                    total_new_cases = 0 
                    country = False
                    parent = os.path.abspath(os.path.join(root, os.pardir))
                    if parent.split("/")[-1] in continents:
                        country = True
                    for row in data:
                        #print row
                        if datetime.strptime(row[0],"%Y-%m-%d").date() < startdate:
                            continue
                        print parent+"/"+parent.split("/")[-1]+"_"+row[0]+".txt"
                        print global_dir+"/"+"Global-Country_"+row[0]+".txt"
                        with open(parent+"/"+parent.split("/")[-1]+"_"+row[0]+".txt",'a+') as tsv_file:
                            writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                            if os.path.getsize(parent+"/"+parent.split("/")[-1]+"_"+row[0]+".txt") == 0:
                                writer.writerow(headers)
                            new_case = float(row[1])
                            total_new_cases += new_case
                            writer.writerow([root.split("/")[-1].replace("_", " "), round(new_case,3), round(total_new_cases,3)])
                        if country:
                            with open(global_dir+"/"+"Global-Country_"+row[0]+".txt",'a+') as tsv_file:
                                writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                                if os.path.getsize(global_dir+"/"+"Global-Country_"+row[0]+".txt") == 0:
                                    writer.writerow(headers)
                                new_case = float(row[1])
                                total_new_cases += new_case
                                writer.writerow([root.split("/")[-1].replace("_", " "), round(new_case,3), round(total_new_cases,3)])
                    print root
        os.chdir("..")
    os.chdir("..")