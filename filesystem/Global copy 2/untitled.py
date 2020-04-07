import os
import sys
import csv 
import numpy as np
from datetime import timedelta, datetime

continents = ["Other","Europe", "North_America", "South_America", "Oceania","Africa","Asia"]

headers = ["Name","Predicted_New_Cases", "Total_Predicted_New_Cases"]


for root, dirs, files in os.walk(os.getcwd()):
    print root
    os.chdir(root)
    date = datetime.date(datetime.now()) + timedelta(-1)
    for file in files:
        if "2020" in file:
            file_date = datetime.strptime(file[-14:-4], "%Y-%m-%d").date()
            if file_date > date:
                print date
    os.chdir("..")
exit(1)
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
                    #print startdate
                    total_new_case = 0 
                    for row in data:
                        #print row
                        parent = os.path.abspath(os.path.join(root, os.pardir))
                        if parent.split("/")[-1] in continents:
                            country = True
                        with open(parent+parent_split.split("/")[-1]+"_"+row[0]+".txt",'a+') as tsv_file:
                            writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                            if os.path.getsize(parent+parent_split.split("/")[-1]+"_"+row[0]+".txt") == 0:
                                writer.writerow(headers)
                            new_case = float(row[1])
                            total_new_cases += new_case
                            writer.write(root.split("/")[-1].replace("_", " "), new_case, total_new_cases)
                        if country:
                            parent =  os.path.abspath(os.path.abspath(os.path.join(root, os.pardir)),os.pardir)
                            with open(parent+"Global-Country_"+row[0]+".txt",'a+') as tsv_file:
                                writer = csv.writer(tsv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                                if os.path.getsize(parent+"Global-Country_"+row[0]+".txt") == 0:
                                    writer.writerow(headers)
                                new_case = float(row[1])
                                total_new_cases += new_case
                                writer.write(root.split("/")[-1].replace("_", " "), new_case, total_new_cases)
        os.chdir("..")
    os.chdir("..")