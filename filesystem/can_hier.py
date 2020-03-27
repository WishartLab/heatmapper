import csv
import os


convert_china = {"Inner Mongolia" : "Nei Mongol", "Ningxia" : "Ningxia Hui", "Xinjiang" : "Xinjiang Uygur"}

with open('map_dependencies/canada_map_dependencies.csv') as csv_file:
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
        if row[3] == "Canada":
            csse_prov.append(row[2])



for prov in csse_prov:
    parent_dir = os.getcwd() + "/Global/North_America/Canada/"
    state_directory = prov
    os.mkdir(parent_dir+state_directory.replace(" ", "_"))
