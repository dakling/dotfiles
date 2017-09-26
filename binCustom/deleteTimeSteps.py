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

latestTime = time_list[-1]

for j in range(0,time_list.size):
    if time_list[j] != "0" and time_list[j] != latestTime:
        shutil.rmtree(time_list[j])

if not keepLatestTime:
    if latestTime != "0":
        shutil.rmtree(latestTime)
