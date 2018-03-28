#!/usr/bin/python3

from openMeetingLastest import *
# import os

homeDir = os.path.expanduser("~")
baseDir = os.path.dirname(homeDir + "/Documents/turbulenceMeeting/")

latestDir = baseDir + "/" + getMostRecentFolder(baseDir)

openLatex(latestDir)
