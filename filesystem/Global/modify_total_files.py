import os
import sys
import csv
from scipy.signal import medfilt
from scipy import arange
from lmfit import Model 
from datetime import timedelta, datetime
continents = ["Other","Europe", "North_America", "South_America", "Oceania","Africa","Asia"]

for root, dirs, files in os.walk(os.getcwd()):
    os.chdir(root)
    for file in files:
        if file == "accumulated.txt":
            os.remove(file)
    os.chdir("..")

headers = ["Name", "Confirmed", "Deaths", "Recovered", "Active", "Confirmed_per_capita", "Deaths_per_capita",
         "IFR_0.30_expected", "IFR_0.65_expected", "IFR_1.0_expected", "IFR_0.30_expected_per_capita", 
         "IFR_0.65_expected_per_capita", "IFR_1.0_expected_per_capita", "Date"]
for continent in continents:
    if continent is None:
        continue
    print(continent)
    for root, dirs, files in os.walk(os.getcwd()+"/"+continent):
        os.chdir(root)
        if files:
            files.sort()
            for file in files:
                if file[-4:] == ".txt" and file != "accumulated.txt":
                    startdate = datetime.date(datetime.now())
                    filedate = datetime.strptime(file[-14:-4],"%Y-%m-%d").date()
                    if filedate >= startdate:
                        continue
                    data = csv.reader(open(root+"/"+file, "rb"), delimiter = '\t')
                    #print file
                    next(data)
                    for row in data:
                        if not os.path.exists(root+"/"+row[0].replace(" ","_")+"/"):
                            continue
                        with open(root+"/"+row[0].replace(" ","_")+"/"+"accumulated.txt",'a') as csv_file: 
                            writer = csv.writer(csv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                            if os.path.getsize(root+"/"+row[0].replace(" ","_")+"/"+"accumulated.txt") == 0:
                                writer.writerow(headers)
                            row = row[0:13]
                            row.append(file[-14:-4])
                            writer.writerow(row)

        os.chdir("..")
    os.chdir("..")