import os
import sys
import csv
#import matplotlib.pyplot as plt
from datetime import timedelta, datetime,date
from distutils.dir_util import copy_tree
def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

start_date = date(2020,4,9)
end_date = date.today()
pritn start_date
print end_date

for single_date in daterange(start_date,end_date):
    date_string = single_date.strftime(single_date,"%Y-%m-%d")
    print date_string
    #copy_tree("Global/","Global"+single_date)