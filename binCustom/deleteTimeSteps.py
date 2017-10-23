#!/usr/bin/python3

import os
import numpy as np
import re
import shutil
import sys

#find out wheter or not to keep the last timestep
keepLatestTime = False
if len(sys.argv) > 1:
    if sys.argv[1] == "--keepLatestTime":
        keepLatestTime = True

#Erstellen einer Liste, die alle Ordner enthaelt
dir_list=(next(os.walk('./'))[1])
dir_list = np.array(dir_list)
dir_list=np.sort(dir_list)

# check if we actually are in an OpenFOAM directory
if not "system" in dir_list or not "constant" in dir_list or not "system" in dir_list:
    raise Exception("This is probably not an OpenFOAM directory!")

#checks if arg is a number
def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        pass

    try:
        import unicodedata
        unicodedata.numeric(s)
        return True
    except (TypeError, ValueError):
        pass
    return False

# create a list of only the time steps
time_list = np.array([])
for i in range(0,dir_list.size):
    if is_number(dir_list[i]):
        time_list = np.append(time_list,(dir_list[i]))

latestTime = (np.max(time_list.astype(float)))

delete_list = np.array([])
if keepLatestTime:
    for j in range(0,time_list.size):
        if float(time_list[j]) != float("0") and float(time_list[j]) != latestTime:
            delete_list = np.append(delete_list,time_list[j])
    print("identified " + str(latestTime) + " as latest Time")
else:
    for j in range(0,time_list.size):
        if float(time_list[j]) != float("0"):
            delete_list = np.append(delete_list,time_list[j])
    print("deleting all nonzero timesteps including the latest time. Use option --keepLatestTime to prevent this")

if input("deleting time steps " + str(delete_list) + " , hit q to abort") == "q":
    raise Exception("Deletion aborted by user, nothing was deleted")

for k in range(0,delete_list.size):
        shutil.rmtree(delete_list[k])
