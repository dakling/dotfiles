#! /usr/bin/env python2
from subprocess import check_output

def get_pass_gmail():
    return check_output("gpg -dq ~/.password-store/.gmail.gpg", shell=True).strip("\n")

def get_pass_web():
    return check_output("gpg -dq ~/.password-store/.web.gpg", shell=True).strip("\n")

def get_pass_fdy():
    return check_output("gpg -dq ~/.password-store/.fdy.gpg", shell=True).strip("\n")

def get_pass_gsc():
    return check_output("gpg -dq ~/.password-store/.gsc.gpg", shell=True).strip("\n")
