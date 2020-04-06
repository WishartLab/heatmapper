import csv
import os

convert_england = {"Bournemouth, Christchurch and Poole" : "Bournemouth", "Bristol, City of" : "Bristol", "Bedford": "Bedfordshire",
            "Cornwall and Isles of Scilly":"Cornwall", "County Durham" : "Durham", "Herefordshire, County of" : "Herefordshire" ,
            "Kingston upon Hull, City of" : "Kingston upon Hull", "St. Helens" : "Saint Helens", "Liverpool" : "Merseyside", "North East Lincolnshire" : "North Lincolnshire"}

convert_london = ["Camden","Lewisham", "Croydon", "Brent", "Lambeth",
                "Hackney and City of London", "Bromley", "Bexley",
                "Hammersmith and Fulham", "Hounslow", "Barking and Dagenham",
                "Greenwich", "Islington", "Kensington and Chelsea",
                "Southwark", "Tower Hamlets", "Wandsworth", "Westminster",
                "Barking", "Barnet", "Ealing", "Enfield", "Haringey",
                "Harrow", "Havering" , "Hillingdon", "Kingston upon Thames", "Merton",
                "Newham", "Redbridge", "Richmond upon Thames", "Sutton", "Waltham Forest"]

convert_scotland = { "Strathclyde": ["Inverclyde", "Glasgow", "Renfrewshire", "East Renfrewshire", "East Dunbartonshire", "West Dunbartonshire", "Argyll and Bute"],
                     "Ayrshire and Arran": ["East Ayrshire", "West Ayrshire","North Ayrshire","South Ayrshire"],
                     "Borders" : ["Scottish Borders"],
                     "Lanarkshire" : [ "North Lanarkshire", "South Lanarkshire", "East Lanarkshire", "West Lanarkshire"],
                     "Lothian" : ["West Lothian", "East Lothian", "Midlothian", "Edinburgh"],
                     "Forth Valley" : ["Clackmannanshire","Stirling", "Falkirk"],
                     "Grampian" : [ "Aberdeen", "Aberdeenshire", "Moray"],
                     "Shetland" : ["Shetland Islands"],
                     "Orkney" : ["Orkney Islands"],
                     "Tayside" : ["Perthshire and Kinross", "Dundee", "Angus"],
                     "Western Isles": ["Eilean Siar"]}
convert_scotland_data = {"Greater Glasgow and Clyde" : "Strathclyde"}

with open('map_dependencies/uk_map_dependencies.csv') as csv_file:
    can_reader = csv.reader(csv_file, delimiter=',')
    heat_stuff =[]
    for row in can_reader:
        country = row[4]
        burough = row[7]
        type = row[10]
        heat_stuff.append([country, burough])

with open('region_gits/covid-19-uk-data/data/covid-19-cases-uk.csv') as csv_file:
    uk_reader = csv.reader(csv_file, delimiter=',')
    uk_stuff =[]
    for row in uk_reader:
        if row[0] == "2020-04-02":
            if convert_england.get(row[3], 0):
                uk_stuff.append([row[1],convert_england[row[3]]])
            elif convert_scotland_data.get(row[3], 0):
                uk_stuff.append([row[1],convert_scotland_data[row[3]]])
            elif row[3] in convert_london:
                uk_stuff.append([row[1], "Greater London"])
            else:
                uk_stuff.append([row[1], row[3]])


            
for item in uk_stuff:
    if item[0] == "England":
        parent_dir = os.getcwd() + "/Global/Europe/United_Kingdom/England/"
        county = item[1]
        if not os.path.exists(parent_dir+county.replace(" ", "_")):
            os.mkdir(parent_dir+county.replace(" ", "_"))
    if item[0] == "Scotland":
        parent_dir = os.getcwd() + "/Global/Europe/United_Kingdom/Scotland/"
        county = item[1]
        if not os.path.exists(parent_dir+county.replace(" ", "_")):
            os.mkdir(parent_dir+county.replace(" ", "_"))


missing_in_data =[]
for item in heat_stuff:
    if item not in uk_stuff:
        if item[0] == "Wales":
            missing_in_data.append(item)

missing_in_heat =[]
for item in uk_stuff:
    if item[0] == "Wales":
        missing_in_heat.append(item)

print missing_in_data
print missing_in_heat

