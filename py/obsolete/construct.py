#!/usr/bin/python3

import sys
import os
from os.path import join as J
import re
from xml.parsers import expat
import sqlite3

BASE = '/home/uj/librusec'
DEST = BASE
IDIR = 'united'
SQLITE_FILE = 'librusec.sqlite'

######################################
# FB2 metadata internal representation

class Author:
    def __init__(self):
        self.first = None
        self.middle = None
        self.last = None
        self.nick = None
        self.email = None

class Document:
    def __init__(self, source=None):
        self.reset(source)
    def reset(self, source=None):
        self.fb2 = source
        self.context1 = None
        self.context2 = None
        self.context3 = None
        self.level = 0
        self.genre = []
        self.authors = []
        self.title = None
        self.date = None
        self.lang = None
        self.seq_name = None
        self.seq_number = None

################
# Expat handlers

d=Document()

def start_element(name, attrs):
    d.level += 1
    if d.level == 3 and name == 'title-info':
        d.context1 = 'T'
    elif d.level == 4 and d.context1 == 'T':
        d.context2 = name
        if d.context2 == 'author':
            d.tmp_author = Author()
        elif d.context2 == 'sequence':
            d.seq_name = attrs.get('name')
            d.seq_number = attrs.get('number')
    elif d.level == 5 and d.context2 == 'author':
        d.context3 = name
        

    #print('Start element:', name, attrs)

def end_element(name):
    d.level -= 1
    if d.level == 4:
        d.context3 = None
    elif d.level == 3:
        if d.context2 == 'author':
            d.authors.append(d.tmp_author)
            d.tmp_author = None
        d.context2 = None
    elif d.level == 2:
        d.context1 = None
    elif d.level == 1: # End of 'description' section, no more interesting to parse
        raise EOFError
    

def char_data(data):
    if d.level == 4 and d.context1 == 'T':
        if d.context2 == 'genre':
            d.genre.append(data.lower())
        elif d.context2 == 'book-title':
            d.title = data
        elif d.context2 == 'date':
            d.date = data
        elif d.context2 == 'lang':
            data = data.lower()
            if data in ('ru', 'en-gb', 'en-us', 'rus', 'russian', 'ru-ru', 'русский'):
                d.lang = 'ru'
            elif data in ('en', 'eng', 'english'):
                d.lang = 'en'
            elif data in ('uk', 'ua'):
                d.lang = 'uk'
            elif data in ('be', 'bel'):
                d.lang = 'be'
            elif data in ('zh', 'chinese'):
                d.lang = 'zh'
            else:
                d.lang = data
    elif d.level == 5 and d.context2 == 'author':
        if d.context3 == 'first-name':
            d.tmp_author.first = data
        elif d.context3 == 'middle-name':
            d.tmp_author.middle = data
        elif d.context3 == 'last-name':
            d.tmp_author.last = data
        elif d.context3 == 'nickname':
            d.tmp_author.nick = data
        elif d.context3 == 'email':
            d.tmp_author.email = data

#################
# SQLite database

class Database:
    def __init__(self, db_file):
        self.lang = {}
        self.genre = {}
        self.n_of_authors = 0

        if os.path.exists(db_file):
            os.unlink(db_file)
        self.con = sqlite3.connect(db_file)
        self.cur = self.con.cursor()
        self.cur.executescript(open("create.sql").read())

    def __del__(self):
        self.con.commit()
        self.con.close()

    def get_lang(self, l):
        return self.lang.get(l)

    def set_lang(self, l):
        index = len(self.lang)+1
        self.lang[l] = index
        self.cur.execute("insert into lang values(?, ?)", (index, l))
        return index

    def getset_lang(self, l):
        lang_id = self.get_lang(l)
        if lang_id is None:
            lang_id = db.set_lang(l)
        return lang_id

    def get_genre(self, g):
        return self.genre.get(g)

    def set_genre(self, g):
        index = len(self.genre)+1
        self.genre[g] = index
        self.cur.execute("insert into genre values(?, ?)", (index, g))
        return index

    def getset_genre(self, g):
        genre_id = self.get_genre(g)
        if genre_id is None:
            genre_id = db.set_genre(g)
        return genre_id

    def get_author(self, name):
        author_id = self.cur.execute("select id from author where name=?", (name,)).fetchone()
        if author_id is not None:
            author_id = author_id[0]
        return author_id

    def set_author(self, name, author):
        self.cur.execute("insert into author values(?, ?, ?, ?, ?, ?, ?, ?)", (None, name, name.lower(), author.first, author.middle, author.last, author.nick, author.email))
        return self.cur.lastrowid

    def getset_author(self, name, author):
        author_id = self.get_author(name)
        if not author_id:
            author_id = self.set_author(name, author)
        return author_id

def author2goodname(author):
    if author.first:
        name = author.first
        if author.middle:
            name += ' ' + author.middle
        if author.last:
            name += ' ' + author.last
    elif author.middle:
        name = author.middle
        if author.last:
            name += ' ' + author.last
    elif author.last:
        name = author.last
    elif author.nick:
        name = author.nick
    else:
        name = '_Noname'
    return name

def author2badname(author):
    (author.first, author.middle, author.last) = (author.last, author.first, author.middle)
    badname = author2goodname(author)
    (author.last, author.first, author.middle) = (author.first, author.middle, author.last)

def shorten(names):
    for i in range(len(names)):
        yield names[i].split()[0]

def by_lang(d):
    "Symlink to BASE/by_lang/<lang>/<name[0]>/<name>/<title>"

    names = []
    for author in d.authors:
        name = author2badname(author)
        if not name in names:
            names.append(name)

    # fill title
    title = d.title or 'Noname'
    title = title.translate({ord('/'):ord('\\')})
    if d.seq_number:
        title = d.seq_number + '. ' + title
    title = ', '.join(shorten(names)) + ' - ' + title
    #print(d.fb2, d.lang, title)

    for name in names:
        # fill path
        if d.seq_name:
            path = J("by_lang", d.lang, name[0], name, d.seq_name)
        else:
            path = J("by_lang", d.lang, name[0], name)

        # do makedirs
        try:
            os.makedirs(path)
        except OSError: # Directory already exists
            pass

        # do symlink
        try:
            os.symlink(os.path.abspath(d.fb2), J(path, title + '.fb2'))
        except OSError: # Symlink already exists
            print(J(path, title + '.fb2'))
            os.symlink(os.path.abspath(d.fb2), J(path, title + ' (' + os.path.basename(d.fb2)[:-4] + ').fb2'))

def to_db(d):
    "Insert document into sqlite database"

    global db

    id = int(os.path.basename(d.fb2)[:-4])

    for genre in d.genre:
        # insert into genre
        genre_id = db.getset_genre(genre)
        # insert into docgenre
        db.cur.execute("insert into docgenre values (?, ?)", (id, genre_id))

    names = []
    for author in d.authors:
        name = author2goodname(author)
        author_id = db.getset_author(name, author)
        db.cur.execute("insert into docauthor values (?, ?)", (id, author_id))
        if not name in names:
            names.append(name)

    # fill fulltitle
    fulltitle = d.title or 'Noname'
    if d.seq_number:
        fulltitle = d.seq_number + '. ' + fulltitle
    if d.seq_name:
        fulltitle = d.seq_name + ' % ' + fulltitle
    fulltitle = ', '.join(names) + ' - ' + fulltitle

    # insert into lang
    lang_id = db.getset_lang(d.lang)
    # insert into document
    title_lc = None
    if d.title:
        title_lc = d.title.lower()
    db.cur.execute("insert into document values (?, ?, ?, ?, ?, ?, ?)", (id, fulltitle, fulltitle.lower(), d.title, title_lc, lang_id, d.date))
            
# XML encoding detector
RE_ENCODING = re.compile("encoding=[\"']([^\"']+)")

# Progress counters
counter = 0
error_counter = 0

def process_document(fb2):
    global counter, error_counter
    # Detect encoding
    if os.path.basename(fb2) in ('124269.fb2', '143087.fb2', '143197.fb2', '155084.fb2', '75355.fb2'):
        return # We can't handle UCS-2 (1*.fb2) or unknown (75355.fb2) encoding
    else:
        f = open(fb2, 'r', errors='ignore')
        l = f.readline()
        try:
            encoding = RE_ENCODING.search(l).group(1)
        except AttributeError as e: # Likely we encountered UCS-2 file, skipping
            print(e)
            error_counter+=1
            return
            
    f = open(fb2, 'r', encoding=encoding, errors='ignore')

    # Prepare expat
    d.reset(fb2)
    p = expat.ParserCreate('UTF-8') # Set expat internal encoding to UTF-8
    p.StartElementHandler = start_element
    p.EndElementHandler = end_element
    p.CharacterDataHandler = char_data

    # Process document with expat
    try:
        p.Parse(f.read())
    except EOFError: # We raise EOFError right after </description>
        pass
    except expat.ExpatError as e:
        print(e)
        error_counter+=1

    if not d.lang:
        d.lang = 'xx'

    # Save results
    #by_lang(d)
    #by_genre(d)
    to_db(d)

    # Report progress
    counter += 1

def process_document_tree():
    os.chdir(BASE)
    docs_list = os.listdir(IDIR)
    docs_list_length = str(len(docs_list))
    for fb2 in docs_list:
        try:
            process_document(J(IDIR, fb2))
        except:
            print('\nCaught exception while processing', J(IDIR, fb2), file=sys.stderr)
            db.con.commit()
            raise
        sys.stderr.write('\033[100D'+'\033[K') # 100 characters left to be sure + clear line
        sys.stderr.write(str(counter) + ' of ' + docs_list_length + ': ' + fb2 + ' ')
        sys.stderr.flush()

if __name__ == '__main__':
    db = Database(SQLITE_FILE)
    process_document_tree()
    print('\nProblems encountered (most are insignificant): ', error_counter)

