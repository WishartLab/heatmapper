import csv
import os



convert = {"DeKalb": "De Kalb", "De Baca" : "Debaca", "De Soto": "Desoto", "DeSoto" : "Desoto", 
 "De Witt" : "Dewitt","DeWitt" : "Dewitt", "Du Page" : "Dupage", "DuPage" : "Dupage",
 "LaSalle" : "La Salle", "LaMoure" :"Lamoure", 'McKean': 'Mc Kean','New York City':'New York', 
'St. Bernard' : "Saint Bernard", 'St. Charles': "Saint Charles", 'St. Clair': "Saint Clair", "St. Croix": 'Saint Croix',
'St. Francis': "Saint Francis",'St. Francis': "Saint Francis", 'St. Helena': 'Saint Helena', 'St. James':'Saint James',
'St. John the Baptist': 'Saint John the Baptist' , 'St. Johns': 'Saint Johns', 'St. Joseph' : "Saint Joseph", "St. Landry" : 'Saint Landry',
'St. Lawrence': 'Saint Lawrence', "St. Louis": "Saint Louis",  "St. Lucie": "Saint Lucie", "St. Martin": "Saint Martin",
'St. Mary': "Saint Mary", "St. Mary's": "Saint Mary's", 'St. Tammany' : 'Saint Tammany', "Ste. Genevieve":"Sainte Genevieve" }




with open('map_dependencies/usa_map_dependencies.csv') as csv_file:
    usa_reader = csv.reader(csv_file, delimiter=',')
    heat_sc =[]
    for row in usa_reader:
       #print(row[6])
        state_county = (row[3],row[6])
        
        heat_sc.append(state_county)

with open('COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv') as csv_file:
    usa_reader = csv.reader(csv_file, delimiter=',')
    csse_sc =[]
    for row in usa_reader:
        if row[3] == "US":
            #print(row[6])
            if convert.get(row[1], 0) and (row[2],row[1]) not in heat_sc:
                
                state_county = state_county = (row[2],convert[row[1]])
            else:
                state_county = (row[2],row[1])

            csse_sc.append(state_county)


missing_in_csse =[]
for item in heat_sc:
    if item not in csse_sc:
        missing_in_csse.append(item)

missing_in_heat =[]
for item in csse_sc:
    if item not in heat_sc:
        missing_in_heat.append(item)


#print(missing_in_csse)

for state_con in csse_sc:
    parent_dir = os.getcwd() + "/Global/North_America/United_States_of_America/"
    state_directory = state_con[0]
    county_directory = state_con[1]
    if not os.path.exists(parent_dir+state_directory.replace(" ", "_")):
        os.mkdir(parent_dir+state_directory.replace(" ", "_"))
    if county_directory != '':
        os.mkdir(parent_dir+state_directory.replace(" ", "_")+"/"+county_directory.replace(" ", "_"))





