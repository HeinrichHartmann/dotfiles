#!/usr/bin/env python

from ftplib import FTP
import json
import sh
import sys, os
import time
from os.path import expanduser

argc = len(sys.argv)
if argc == 1:
    PREVIEW = True
    PROMPT = True
elif argc == 2:
    PREVIEW = False
    PROMPT = False
    FILENAME = argv[1]
else:
    print "Usage:", sys.argv[0], "[filename]"


ftp = 0

def connect():
    with open(expanduser("~/.scan_conf")) as fh:
        conf = json.load(fh)

    global ftp
    ftp = FTP(
        'server02.storage.hosteurope.de',
        conf['user'],
        conf['password']
    )

def ftp_get_filename():
    file_list = sorted(filter(
        lambda p: p.endswith("pdf"),
        ftp.nlst()
    ))
    
    if len(file_list) == 0:
        raise Exception("No files found.")

    return file_list[-1]

def ftp_store(file_name):
    tfn = "/tmp/" + file_name
    with open("/tmp/" + file_name, "w") as tfh:
        print "Downloading ", file_name
        ftp.retrbinary('RETR ' + file_name, tfh.write)
    return tfn

def ftp_rm(file_name):
    ftp.delete(file_name)
    print "Removed file ", file_name

def archive(src, name):
    dest = expanduser("~/Documents/") + time.strftime("%Y-%m-%d_") + name + ".pdf"
    os.rename(src, dest)
    print "Wrote file://" + dest
    

p = 0
def pdf_open(path):
    import subprocess
    global p
    p = subprocess.Popen(["evince", path])
    # returncode = p.wait()

def main():   
    connect()
    file_name = ftp_get_filename()
    temp_file = ftp_store(file_name)

    print "Opening pdf file://" + temp_file
    pdf_open(temp_file)
    
    arc_name = raw_input("Archive? ")
    if not (arc_name == ""):
        archive(temp_file, arc_name)
        ftp_rm(file_name)
    else:
        if raw_input("Delete? ") == "y":
            ftp_rm(file_name)

    p.terminate()
        
if __name__ == "__main__":
    main()
