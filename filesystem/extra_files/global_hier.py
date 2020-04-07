import csv
import os

country_convert = {"US":"United States of America", "Bahamas" : "The Bahamas", "Congo (Kinshasa)": "Democratic Republic of the Congo","Congo (Brazzaville)":"Republic of the Congo",
        "Cote d'Ivoire": "Ivory Coast", "Czechia" : "Czech Republic", "Eswatini" : "Swaziland", "Korea, South" : "South Korea", 
        "North Macedonia": "Macedonia", "Serbia" : "Republic of Serbia", "Taiwan*" : "Taiwan", "Tanzania":"United Republic of Tanzania", "Timor-Leste" :"East Timor",
        "Holy See" : "Vatican", "Cabo Verde" : "Cape Verde"}

country_continent_map = {}
with open('COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv') as csv_file:
    usa_reader = csv.reader(csv_file, delimiter=',')
    csse_countries =[]
    for row in usa_reader:
       if row[3] != "Country_Region":
            if row[3] not in csse_countries and country_convert.get(row[3], 0):   
                csse_countries.append(country_convert[row[3]]) 
            else:
                csse_countries.append(row[3])  
#print(csse_countries)

with open('map_dependencies/world_data.csv') as csv_file:
    global_reader = csv.reader(csv_file, delimiter=',')
    heat_countries =[]
   
    for row in global_reader:
        if (row[7] == "Sovereign country") or (row[7] == "Country"):
            heat_countries.append((row[8],row[34]))
            country_continent_map[row[8]] = row[34]

print(country_continent_map)

#print(heat_countries)

#for con_cunt in heat_countries:
#    parent_dir = os.getcwd() + "/Global/"
#    continent_directory = con_cunt[1]
#    country_directory = con_cunt[0]
#    if continent_directory == "Australia":
#        continent_directory = "Oceania"
#    if country_directory != '' and (country_directory != "United State of America") and not os.path.exists(parent_dir+continent_directory.replace(" ", "_")+"/"+country_directory.replace(" ", "_")):
#        os.mkdir(parent_dir+continent_directory.replace(" ", "_")+"/"+country_directory.replace(" ", "_"))

            

