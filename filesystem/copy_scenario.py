import os
import sys
import csv
#import matplotlib.pyplot as plt
from datetime import timedelta, datetime,date
from distutils.dir_util import copy_tree
import shutil



copy_tree("Global_best_case/","Best_Case")
copy_tree("Global_worst_case/","Worst_Case")

os.chdir("Best_Case")
for root, dirs, files in os.walk(os.getcwd()):
    for file in files:
        if (".tsv" not in file) and (".txt" not in file):
            try:
                file_name = root+"/"+file
                os.remove(file_name)
            except Exception as message:
                print(message)
print os.getcwd()
os.chdir("..")
os.chdir("Worst_Case")
for root, dirs, files in os.walk(os.getcwd()):
    for file in files:
        if (".tsv" not in file) and (".txt" not in file):
            try:
                file_name = root+"/"+file
                os.remove(file_name)
            except Exception as message:
                print(message)