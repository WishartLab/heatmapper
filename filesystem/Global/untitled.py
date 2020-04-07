import os
import sys
import csv 
import numpy as np
from datetime import timedelta, datetime

import os
import sys
import csv
from scipy.signal import medfilt
from scipy import arange
from lmfit import Model 

continents = ["Other","Europe", "North_America", "South_America", "Oceania","Africa","Asia"]

for root, dirs, files in os.walk(os.getcwd()):
    print root
    os.chdir(root)
    for file in files:
        if file == "regional_predicted.txt":
            os.remove(file)
    os.chdir("..")
