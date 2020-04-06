import csv
import os

convert_china = {"Inner Mongolia" : "Nei Mongol", "Ningxia" : "Ningxia Hui", "Xinjiang" : "Xinjiang Uygur",
"Tibet" : "Xizang"}

with open('map_dependencies/china_map_dependencies.csv') as csv_file:
    can_reader = csv.reader(csv_file, delimiter=',')
    heat_prov =[]
    for row in can_reader:
        province = (row[3])
        if province not in heat_prov:
            heat_prov.append(province)
        
        

with open('COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv') as csv_file:
    can_reader = csv.reader(csv_file, delimiter=',')
    csse_prov =[]
    for row in can_reader:
        if row[3] == "China":
            if convert_china.get(row[2], 0):
                csse_prov.append(convert_china[row[2]])
            else:
                csse_prov.append(row[2])

            
missing_in_csse =[]
for item in heat_prov:
    if item not in csse_prov:
        missing_in_csse.append(item)

missing_in_heat =[]
for item in csse_prov:
    if item not in heat_prov:
        missing_in_heat.append(item)


for prov in csse_prov:
    parent_dir = os.getcwd() + "/Global/Asia/China/"
    state_directory = prov
    os.mkdir(parent_dir+state_directory.replace(" ", "_"))

