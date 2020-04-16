import os
import sys
import csv
from datetime import timedelta, datetime, date
import json

def divide_timedelta(td, divisor):
    # timedelta.total_seconds() is new in Python version 2.7, so don't use it
    total_seconds = (td.microseconds + (td.seconds + td.days * 24 * 3600) * 1e6) / 1e6
    divided_seconds = total_seconds / float(divisor)
    return timedelta(seconds=divided_seconds)

files = [f for f in os.listdir('.') if os.path.isfile(f)]
country_date = dict()
files.sort()
china_date = date(2020,01,10)
for f in files:
    if "Global-" in f:
        data = csv.reader(open(f, "rb"), delimiter = '\t')
        next(data)
        data = list(data)
        for row in data:
            if float(row[1]) > 0:
                if row[0] not in country_date.keys():
                    date = datetime.strptime(f[-14:-4],"%Y-%m-%d").date()
                    country_date[row[0]] = date.strftime("%Y-%m-%d")
with open('start_dates.json', 'w') as outfile:
    json.dump(country_date, outfile)