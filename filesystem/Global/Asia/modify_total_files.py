import os
import sys
import csv 

continents - ["Asia", "Europe", "North_America", "South_America", "Oceania","Africa"]

for continent in continents:
    for root, dirs, files in os.walk(os.getcwd()+"/"+continent):
        print root
        os.chdir(root)
        if files:
            for file in files:
                if file[-4:] == ".txt" and file != "accumulated.txt":
                    data = csv.reader(open(root+"/"+file, "rb"), delimiter = '\t')
                    print file
                    next(data)
                    for row in data:
                        with open(root+"/"+row[0].replace(" ","_")+"/"+"accumulated.txt",'a') as csv_file:
                            reader = csv.reader(csv_file, delimiter='\t') 
                            data = list(reader)
                            if row[1] > data[-1][1]:
                                writer = csv.writer(csv_file, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                                writer.writerow(row)
        os.chdir("..")
    os.chdir("..")