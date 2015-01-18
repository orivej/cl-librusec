#!/usr/bin/python3

COPYSRC = '/home/uj/librusec/united/'
SQLITE_NONVOLATILE_FILE = '/home/uj/librusec/librusec.sqlite'
SQLITE_FILE = '/home/uj/librusec/librusec.sqlite'
#SQLITE_FILE = '/tmp/librusec.sqlite'

import sys
import sqlite3
import optparse
import shutil
import os.path

def process(q):
    for el in q:
        print("{:>6}".format(el[0]), el[1], end='')
        if el[2]:
            print(' (' + el[2] + ')', end='')
        if el[3] != 'ru':
            print(' [' + el[3] + ']', end='')
        print()
        if options.copy:
            src = COPYSRC + str(el[0]) + '.fb2'
            dst = el[1].replace('/', '\\').replace(':', '.').replace('?', '').replace('"', '')
            if el[2] and el[2].isdigit():
                dst += ' (' + el[2] + ')'
            try:
                shutil.copy(src, dst + '.fb2')
            except IOError as e:
                print('Error copying %s: %s' % (src, e))

if len(sys.argv) == 1:
    sys.argv.append('-h')

parser = optparse.OptionParser("usage: %prog [options] [search_string]")
#parser.add_option('-a', '--author', dest='author')
#parser.add_option('-t', '--title', dest='title')
#parser.add_option('-g', '--genre', dest='genre')
parser.add_option('-l', '--lang', dest='lang')
parser.add_option('-c', '--copy', action='store_true', dest='copy', help='copy matched files here')
parser.add_option('-n', '--numeric', action='store_true', dest='numeric', help='sort by id')
parser.add_option('-i', '--id', dest='id', help='select this id')

(options, args) = parser.parse_args()

if not os.path.exists(SQLITE_FILE):
    shutil.copy2(SQLITE_NONVOLATILE_FILE, SQLITE_FILE)
con = sqlite3.connect(SQLITE_FILE)
cur = con.cursor()

if options.id:
    q_where = [ "document.id = ?" ]
    q_opts = [ options.id ]
else:
    fulltitle = ' '.join(args) # even if there are no args
    q_where = [ "document.fulltitle_lc like ?" ]
    q_opts = [ '%' + fulltitle.lower() + '%' ]
if options.lang:
    q_where.append("lang.lang = ?")
    q_opts.append(options.lang)
if options.copy:
    options.numeric = True
if not options.numeric:
    order = " order by document.fulltitle"
else:
    order = " order by document.id"

cur.execute("select document.id, document.fulltitle, document.date, lang.lang"
            " from document, lang"
            " on document.lang = lang.id"
            " where " + " and ".join(q_where) +
            order,
            q_opts)

process(cur.fetchall())
