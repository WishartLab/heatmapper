import os
import sys
import csv
#import matplotlib.pyplot as plt
from datetime import timedelta, datetime, date
from distutils.dir_util import copy_tree
from shutil import copyfile
from subprocess import call
def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

start_date = date.today() - timedelta(4)
end_date = date.today()
for single_date in daterange(start_date,end_date):
    date_string = single_date.strftime("%Y-%m-%d")
    tomorrow = (single_date+timedelta(1)).strftime("%Y-%m-%d")
    copy_tree("Global/","Downloads/past_inferences/Global_"+ date_string)
    os.chdir("Downloads/past_inferences/Global_"+ date_string)
    for root, dirs, files in os.walk(os.getcwd()):
        for file in files:
            if file == "accumulated.txt":
                copy_name = root+"/"+file.replace(".txt","")+"_copy.txt"
                orig_name = root+"/"+file
                copyfile(orig_name,copy_name)
                with open(copy_name,'rb') as copy, open(orig_name, 'wb') as accum:
                    writer = csv.writer(accum, delimiter='\t', quotechar='"', quoting=csv.QUOTE_MINIMAL)
                    for row in csv.reader(copy, delimiter = '\t'):
                        if row[-1] == "Date":
                            writer.writerow(row)
                        elif datetime.strptime(row[-1],"%Y-%m-%d").date() <= single_date:
                            writer.writerow(row)
                os.remove(copy_name)
    call(["python","convolute_regions.py",tomorrow])
    for root, dirs, files in os.walk(os.getcwd()):
        for file in files:
            if (".txt" not in file) and (".tsv" not in file):
                os.remove(root+"/"+file)
    os.chdir("..")
    os.chdir("..")
    os.chdir("..")

