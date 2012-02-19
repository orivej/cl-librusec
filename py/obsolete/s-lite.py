#!/usr/bin/python3

#BASE = '/home/uj/librusec'
BASE = '/tmp'
SQLITE_FILE = 'librusec-lite.sqlite'

import sys
import os
from os.path import join as J
import sqlite3
import optparse
import pprint

parser = optparse.OptionParser()

(options, args) = parser.parse_args()

if args:
    fulltitle = ' '.join(args)

os.chdir(BASE)
con = sqlite3.connect(SQLITE_FILE)
cur = con.cursor()

q_where = []
q_opts = []
if args:
    q_where.append("fulltitle_lc like ?")
    q_opts.append('%'+fulltitle.lower()+'%')

cur.execute("select id, fulltitle from document where " + " and ".join(q_where) + " order by fulltitle", q_opts)

for el in cur.fetchall():
    print("{:>6}".format(el[0]), el[1])
