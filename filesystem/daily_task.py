#!/usr/bin/env python 
import csv
import os
from datetime import datetime
import sys
import numpy as np
country_convert = {"US":"United States of America", "Bahamas" : "The Bahamas", "Congo (Kinshasa)": "Democratic Republic of the Congo","Congo (Brazzaville)": "Republic of the Congo" , 
        "North Macedonia": "Macedonia", "Serbia" : "Republic of Serbia", "Taiwan*" : "Taiwan", "Tanzania":"United Republic of Tanzania", "Timor-Leste" :"East Timor",
        "Holy See" : "Vatican", "Cabo Verde" : "Cape Verde", "Burma" : "Myanmar", "The Gambia": "Gambia", "Bahamas, The": "The Bahamas", "Czechia" :"Czech Republic",
        "Eswatini" :"Swaziland", "Korea, South" : "South Korea", "Korea, North" :"North Korea", "Cote d'Ivoire" : "Ivory Coast", "Russian Federation" :"Russia",
        "Viet Nam" : "Vietnam", "Mainland China" : "China", "UK":"United Kingdom"}


us_county_convert = {"DeKalb": "De Kalb", "De Baca" : "Debaca", "De Soto": "Desoto", "DeSoto" : "Desoto", 
 "De Witt" : "Dewitt","DeWitt" : "Dewitt", "Du Page" : "Dupage", "DuPage" : "Dupage",
 "LaSalle" : "La Salle", "LaMoure" :"Lamoure", 'McKean': 'Mc Kean','New York City':'New York', 
'St. Bernard' : "Saint Bernard", 'St. Charles': "Saint Charles", 'St. Clair': "Saint Clair", "St. Croix": 'Saint Croix',
'St. Francis': "Saint Francis",'St. Francis': "Saint Francis", 'St. Helena': 'Saint Helena', 'St. James':'Saint James',
'St. John the Baptist': 'Saint John the Baptist' , 'St. Johns': 'Saint Johns', 'St. Joseph' : "Saint Joseph", "St. Landry" : 'Saint Landry',
'St. Lawrence': 'Saint Lawrence', "St. Louis": "Saint Louis",  "St. Lucie": "Saint Lucie", "St. Martin": "Saint Martin",
'St. Mary': "Saint Mary", "St. Mary's": "Saint Mary's", 'St. Tammany' : 'Saint Tammany', "Ste. Genevieve":"Sainte Genevieve", "St. Francois": "Saint Francois"}


china_convert = {"Inner Mongolia" : "Nei Mongol", "Ningxia" : "Ningxia Hui", 
                "Xinjiang" : "Xinjiang Uygur",
                "Tibet" : "Xizang"}


country_continent = {'Canada': 'North America', 'East Timor': 'Asia', 'Sao Tome and Principe': 'Africa', 'Turkmenistan': 'Asia', 'United States of America': 'North America', 'Vatican': 'Europe',
'Lithuania': 'Europe', 'Cambodia': 'Asia', 'Saint Kitts and Nevis': 'North America', 'Ethiopia': 'Africa', 'Aruba': 'South America', 'Swaziland': 'Africa', 
'Argentina': 'South America', 'Bolivia': 'South America', 'Cameroon': 'Africa', 'Burkina Faso': 'Africa', 'Aland': 'Europe', 'Ghana': 'Africa', 
'Saudi Arabia': 'Asia', 'Japan': 'Asia', 'Cape Verde': 'Africa', 'Slovenia': 'Europe', 
'Guatemala': 'North America', 'Bosnia and Herzegovina': 'Europe', 'Guinea': 'Africa', 
'Germany': 'Europe', 'Spain': 'Europe', 'Liberia': 'Africa', 'Netherlands': 'Europe', 'Pakistan':'Asia', 
'Oman': 'Asia', 'Zambia': 'Africa', 'Greenland': 'Europe', 'Gabon': 'Africa', 'Monaco': 'Europe', 'Samoa': 'Oceania',
'New Zealand': 'Oceania', 'Yemen': 'Asia', 'Jersey': 'Europe', 'Jamaica': 'North America', 'Albania': 'Europe', 'West Bank': 'Europe',
'United Arab Emirates': 'Asia', 'Uruguay': 'South America', 'India': 'Asia', 'Azerbaijan': 'Europe', 'Lesotho': 'Africa', 
'Saint Vincent and the Grenadines': 'North America', 'Republic of Serbia': 'Europe', 'Kenya': 'Africa', 
'South Korea': 'Asia', 'Tajikistan': 'Asia', 'Turkey': 'Europe', 'Afghanistan': 'Asia', 'Bangladesh': 'Asia', 
'Mauritania': 'Africa', 'Solomon Islands': 'Oceania', 'Saint Lucia': 'North America', 'San Marino': 'Europe', 
'Kyrgyzstan': 'Asia', 'Mongolia': 'Asia', 'France': 'Europe', 'Rwanda': 'Africa', 'Slovakia': 'Europe', 'Somalia': 'Africa', 
'Peru': 'South America', 'Laos': 'Asia', 'Nauru': 'Oceania', 'Seychelles': 'Africa', 'Norway': 'Europe', 'Malawi': 'Africa', 
'Benin': 'Africa', 'Federated States of Micronesia': 'Oceania', 'Cuba': 'North America', 'Montenegro': 'Europe', 
'Republic of the Congo': 'Africa', 'Togo': 'Africa', 'China': 'Asia', 'Armenia': 'Europe', 'Dominican Republic': 'North America',
'Ukraine': 'Europe', 'Bahrain': 'Asia', 'Tonga': 'Oceania', 'Finland': 'Europe', 'Libya': 'Africa', 'Indonesia': 'Asia',
 'Central African Republic': 'Africa', 'Mauritius': 'Africa', 'Sweden': 'Europe', 'Belarus': 'Europe', 'Mali': 'Africa', 
 'Russia': 'Europe', 'Bulgaria': 'Europe', 'Romania': 'Europe', 'Angola': 'Africa', 'Portugal': 'Europe', 
'Trinidad and Tobago': 'North America', 'Cyprus': 'Europe', 'Liechtenstein': 'Europe', 'Qatar': 'Asia',
'Malaysia': 'Asia', 'Austria': 'Europe', 'Vietnam': 'Asia', 'Mozambique': 'Africa', 'Uganda': 'Africa', 'Hungary': 'Europe', 
'Niger': 'Africa', 'Isle of Man': 'Europe', 'Brazil': 'South America', 'The Bahamas': 'North America', 
'Panama': 'North America', 'Costa Rica': 'North America', 'Luxembourg': 'Europe', 'Andorra': 
'Europe', 'Ivory Coast': 'Africa', 'Palau': 'Oceania', 'Nigeria': 'Africa', 'Ecuador': 'South America',
'Czech Republic': 'Europe', 'Brunei': 'Asia', 'Australia': 'Oceania', 'Iran': 'Asia', 'Algeria': 'Africa',
'El Salvador': 'North America', 'Tuvalu': 'Oceania', 'Marshall Islands': 'Oceania', 'Chile': 'South America', 
'Belgium': 'Europe', 'Thailand': 'Asia', 'Haiti': 'North America', 'Belize': 'South America', 'Sierra Leone': 'Africa', 
'Georgia': 'Europe', 'Hong Kong S.A.R.': 'Asia', 'Gambia': 'Africa', 'Philippines': 'Asia', 'Guinea Bissau': 'Africa', 
'Moldova': 'Europe', 'Morocco': 'Africa', 'Croatia': 'Europe', 'Guernsey': 'Europe', 'United Republic of Tanzania': 'Africa',
'Switzerland': 'Europe', 'Grenada': 'South America', 'Iraq': 'Asia', 'Chad': 'Africa', 'Estonia': 'Europe', 'Kosovo': 'Europe',
'Mexico': 'North America', 'Lebanon': 'Asia', 'Northern Cyprus': 'Europe', 'South Africa': 'Africa', 'Uzbekistan': 'Asia', 
'Tunisia': 'Africa', 'Djibouti': 'Africa', 'Antigua and Barbuda': 'North America', 'Dominica': 'North America', 'Colombia': 'South America',
'Burundi': 'Africa', 'Taiwan': 'Asia', 'Fiji': 'Oceania', 'Barbados': 'South America', 'Madagascar': 'Africa', 'Italy': 'Europe', 'Curacao':
'South America', 'Bhutan': 'Asia', 'Sudan': 'Africa', 'Nepal': 'Asia', 'Kiribati': 'Oceania', 'Malta': 'Europe', 
'Democratic Republic of the Congo': 'Africa', 'Maldives': 'Asia', 'Suriname': 'South America', 'Gaza': 'Europe', 'Kuwait': 'Asia', 
'Israel': 'Europe', 'Iceland': 'Europe', 'Venezuela': 'South America', 'Senegal': 'Africa', 'Papua New Guinea': 'Oceania', 
'Zimbabwe': 'Africa', 'Jordan': 'Asia', 'Vanuatu': 'Oceania', 'Denmark': 'Europe', 'Kazakhstan': 'Asia', 'Poland': 'Europe', 
  'Macau S.A.R': 'Asia', 'Eritrea': 'Africa', 'Ireland': 'Europe', 'Macedonia': 'Europe', 'North Korea': 'Asia', 'Paraguay': 'North America', 
  'Latvia': 'Europe', 'South Sudan': 'Africa', 'Guyana': 'South America', 'Syria': 'Asia', 'Sint Maarten': 'Europe', 'Honduras': 'North America',
   'Myanmar': 'Asia', 'Equatorial Guinea': 'Africa', 'Egypt': 'Africa', 'Nicaragua': 'North America', 
'Singapore': 'Asia', 'Comoros': 'Africa', 'United Kingdom': 'Europe', 'Greece': 'Europe', 'Sri Lanka': 'Asia', 'Namibia': 'Africa', 
'Botswana': 'Africa', 'West Bank and Gaza': "Europe", "Guinea-Bissau" :"Africa"}

xDate = sys.argv[1]

def calculate_ifr(row):
    ifr_rate = [0.0030,0.0065, 0.01]
    asymptomatic_rate = [0.8, 0.5, 0.2]
    for x in range(len(ifr_rate)):
        if row[1] == 0.0:
            row.append(float(row[0])+(float(row[0])*(float(asymptomatic_rate[x])/(float(1)-float(asymptomatic_rate[x])))))

        else:
            row.append(float(row[1])*float(1)/float(ifr_rate[x]))
    for x in range(len(ifr_rate)):
        if row[4] == "N/A":
            row.append("N/A")
            continue
        if row[5] == 0:
            row.append(float(row[4])+(float(row[4])*(float(asymptomatic_rate[x])/(float(1)-float(asymptomatic_rate[x])))))
        else:
            row.append(float(row[5])*float(1)/float(ifr_rate[x]))
    return row


def get_region_values(list,region, data):
    if data == None:
        return list+["N/A","N/A","N/A","N/A"]
    region_population = None
    for x in data:
        if x[0] == region:
            region_population = x[1]
            break
    x =0
    while x < 2:
        if region_population == None:
            list.append("N/A")
        else:
            per_capita = round((float(list[x])/(float(region_population))),8)
            list.append(per_capita)
        x += 1
    return list

def get_data(continent,country,type):
    data = None
    if type == "Country":
        if os.path.exists('Global/country_data.csv'):
            with open('Global/country_data.csv') as csv_file:
                reader = csv.reader(csv_file, delimiter='\t') 
                data = list(reader)
    elif type == "prov_state":
        if os.path.exists('Global/{}/{}/region_data.csv'.format(continent.replace(" ","_"),country.replace(" ","_"))):
            with open('Global/{}/{}/region_data.csv'.format(continent.replace(" ","_"),country.replace(" ","_"))) as csv_file:
                reader = csv.reader(csv_file, delimiter=',') 
                data = list(reader)

    else:
        if os.path.exists('Global/{}/{}/county_data.csv'.format(continent.replace(" ","_"),country.replace(" ","_"))):
            with open('Global/{}/{}/county_data.csv'.format(continent.replace(" ","_"),country.replace(" ","_"))) as csv_file:
                reader = csv.reader(csv_file, delimiter=',')
                data = list(reader)
    return data

def get_county_values(list,county,state, data):
    county_population = None
    for x in data:
        if x[0] == state and x[1] == county:
            county_population = x[2]
            break
    x =0
    while x < 2:
        if county_population == None:
            list.append("N/A")
        else:
            per_capita = round((float(list[x])/(float(county_population))),8)
            list.append(per_capita)
        x += 1
    return list
    

def combine_values(list1,list2):
    new_list = []
    i=0
    for i in range(len(list1)):
        if list1[i] == "N/A" or list2[i] == "N/A":
            new_list.append("N/A")
        else:
            new_list.append(int(list1[i])+int(list2[i]))
    return new_list


def write_to_file(file, region, list):
    file.write(region)
    for x in list:
        file.write("\t")
        file.write(str(x))
    file.write("\n")
    #print("File: " + file.name + " written" )


def add_together(data,limiter):
    new_row = [None,limiter,"",0.0,0.0,0.0,0.0,0.0]
    for row in data:
        if row[1] == limiter:
            if row[3] == '':
                row[3] = 0
            if row[4] == '':
                row[4] = 0
            if row[5] == '':
                row[5] = 0  
            new_row[3] += float(row[3])
            new_row[4] += float(row[4])
            new_row[5] += float(row[5])
            data.remove(row)
    print new_row
    data.append(new_row)

date = datetime.strptime(xDate, "%Y%m%d")

year = str(date.year)
month = str('%02d' % date.month)
day = str('%02d' % date.day)

hierarchy = {}
all_rows = []



with open('COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/{}-{}-{}.csv'.format(month,day,year)) as csv_file:
    reader = csv.reader(csv_file, delimiter=',')
    next(csv_file)
    data = list(reader)
    if (int(month) < 3) or ((int(month) == 3) and (int(day) < 10)):
        add_together(data,'US')
        add_together(data,'Canada')
        add_together(data,'China')
        add_together(data,'Italy')
        add_together(data,'Australia')
        add_together(data,'UK')
        add_together(data,'France')
    countries = []
    w_dependencies = []
    if (int(month) < 3) or ((int(month) == 3) and (int(day) < 22)):
        data = data
        for row in data:
            data[data.index(row)] = ['',''] + row  
    for row in data:
        dependency = row[2]
        country = row[3]
        if dependency:
            w_dependencies.append(country)
    w_dependencies = np.unique(w_dependencies)
    for row in data:
        dependency = row[2]
        country = row[3]
        if (dependency == '') and (country in w_dependencies):
            countries.append(country)
    third_reader = csv.reader(csv_file, delimiter=',')
    for row in data: 
        if "Federal Bureau of Prisons" in row:
            continue
        elif "US Military" in row:
            continue
        country = row[3]
        state_province = row[2]
        if (state_province == "Diamond Princess" or state_province == "Grand Princess"):
            continue
        county_department = row[1]
        if country in country_convert:
            country = country_convert.get(country)
        if country == "United States of America":
            if county_department in us_county_convert:
                county_department = us_county_convert.get(county_department)
        if country == "China":
            if state_province in china_convert:
                state_province = china_convert.get(state_province)
        if country == "Cruise Ship":
                country = "Diamond Princess"
                state_province = None
                county_department = None
        if country in countries and state_province == '':
            state_province = country
        continent = country_continent.get(country)
        if continent == None:
            continent = "other"
        if (int(month) < 3) or ((int(month) == 3) and (int(day) < 22)):
            if row[6] == '':
                row[6] = 0.0
            if row[7] == '':
                row[7] =0.0
            if row[5] == '':
                row[5] = 0.0
            print [continent,country,state_province,county_department,row[5],row[6],row[7], "N/A"]
            all_rows.append([continent,country,state_province,county_department,float(row[5]),float(row[6]),float(row[7]), "N/A"])
        else:
            all_rows.append([continent,country,state_province,county_department,float(row[7]),float(row[8]), float(row[9]), float(row[10])])

for row in all_rows:
    region = row[2]
    for second_row in all_rows:
        dependent_country = second_row[1]
        nil_region = second_row[2]
        if region == dependent_country and nil_region == '' and region != "Diamond Princess" and second_row[0] == 'other':
            all_rows.remove(second_row)

for row in all_rows:
    continent = row[0]
    country = row[1]
    prov = row[2]
    county = row[3]
    if continent not in hierarchy:
        hierarchy[continent] = {}
    continent_hash = hierarchy.get(continent)
    if country not in continent_hash:
        continent_hash[country] = {}
    country_hash = continent_hash.get(country)
    if prov:
        if prov not in country_hash:
            country_hash[prov] = {}
        prov_hash = country_hash.get(prov)
        if county:
            if county not in prov_hash:     
                prov_hash[county] = [row[4],row[5],row[6],row[7]]
        else:
            country_hash[prov] = [row[4],row[5],row[6],row[7]]
    else:
        continent_hash[country] =  [row[4],row[5],row[6],row[7]]


headers = ["Name", "Confirmed", "Deaths", "Recovered", "Active", "Confirmed_per_capita", "Deaths_per_capita",
         "IFR_0.30_expected", "IFR_0.65_expected", "IFR_1.0_expected", "IFR_0.30_expected_per_capita", 
         "IFR_0.65_expected_per_capita", "IFR_1.0_expected_per_capita"]

time_file = year + "-" + month + "-" + day + ".txt"
country_data = get_data(None,None, "Country")
with open("Global/Global-Country_"+time_file,"w") as global_country_file:
    global_country_file.write("\t".join(headers))
    global_country_file.write("\n")
    for continent in hierarchy.keys():
        with open("Global/"+continent.replace(" ","_")+"/"+continent.replace(" ","_")+"_" + time_file,"w+") as continental_file:
            continental_file.write("\t".join(headers))
            continental_file.write("\n")
            #continental_collective_count = [0,0,0,0]
            for country in hierarchy[continent]:
                if type(hierarchy[continent][country]) is list:
                    value_list = calculate_ifr(get_region_values(hierarchy[continent][country], country, country_data))
                    write_to_file(continental_file,country,value_list)
                    write_to_file(global_country_file,country,value_list)
                    #continental_collective_count = combine_values(continental_collective_count, hierarchy[continent][country])
                else:
                    country_collective_count = [0,0,0,0]
                    with open("Global/"+continent.replace(" ","_")+ "/" + country.replace(" ","_") + "/" + country.replace(" ","_") + "_" + time_file,"w+") as country_file:
                        country_file.write("\t".join(headers))
                        country_file.write("\n")
                        prov_state_data = get_data(continent,country,"prov_state")
                        county_data = get_data(continent,country,"county")
                        for prov_state in hierarchy[continent][country]:
                            if prov_state == "Recovered":
                                continue 
                            if type(hierarchy[continent][country][prov_state]) is list:
                                write_to_file(country_file,prov_state,calculate_ifr(get_region_values(hierarchy[continent][country][prov_state],prov_state,prov_state_data)))
                                country_collective_count = combine_values(country_collective_count,hierarchy[continent][country][prov_state])
                            else:
                                prov_state_collective_count = [0,0,0,0]
                                with open("Global/"+continent.replace(" ","_")+ "/" + 
                                    country.replace(" ","_")+ "/" + prov_state.replace(" ", "_") + "/" + prov_state.replace(" ", "_")+ "_" + time_file,"w+") as prov_state_file:
                                    prov_state_file.write("\t".join(headers))
                                    prov_state_file.write("\n")
                                    for county in hierarchy[continent][country][prov_state]:
                                        write_to_file(prov_state_file, county, calculate_ifr(get_county_values(hierarchy[continent][country][prov_state][county],county,prov_state,county_data)))
                                        prov_state_collective_count = combine_values(prov_state_collective_count,hierarchy[continent][country][prov_state][county])
                                    write_to_file(country_file,prov_state,calculate_ifr(get_region_values(prov_state_collective_count,prov_state,prov_state_data)))
                                    country_collective_count = combine_values(country_collective_count, prov_state_collective_count)
                    value_list = calculate_ifr(get_region_values(country_collective_count, country, country_data))
                    write_to_file(continental_file, country,value_list)
                    write_to_file(global_country_file,country,value_list)
                    #continental_collective_count =combine_values(continental_collective_count, country_collective_count)

print xDate + " Done"

