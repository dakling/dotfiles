#!/usr/bin/python3

import os
import webbrowser

def folderToInt(folder):
    return(int(folder.replace("-", "")))

def getMostRecentFolder(baseDir):

    dateDirs = os.listdir(baseDir)

    mostRecentDirIndex = 0
    mostRecentDirInt = folderToInt(dateDirs[mostRecentDirIndex])

    for date in range(0,len(dateDirs)):
        if folderToInt(dateDirs[date]) > mostRecentDirInt:
            mostRecentDirInt = folderToInt(dateDirs[date])
            mostRecentDirIndex = date

    return dateDirs[mostRecentDirIndex]

def openFolder(folderPath):
    webbrowser.open(folderPath)

def openLatex(folderPath):
    os.system("nvim-termite " + (folderPath + "/main.tex"))

def openPdf(folderPath):
    os.system("qpdfview " + (folderPath + "/main.pdf"))

