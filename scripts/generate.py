#!/usr/bin/env python3

import subprocess
import pathlib
import os
from glob import glob
import shutil

from bs4 import BeautifulSoup

# script_path = pathlib.Path(__file__).parent.absolute()
script_path = "/jellyfish/art/website/scripts"

def convert_org_file(fname):
    cmd = ["emacs", "-q", "--batch",
           "-l", os.path.join(script_path, "batch-org-to-html.el"),
           "--eval", '(batch-org-to-html "{}")'.format(fname)]
    subprocess.run(cmd, check=True)

def adapt_link(link):
    if 'href' not in link.attrs: return
    href = link.attrs['href']
    dirname = os.path.dirname(href)
    # don't change external links
    if dirname[:4] == 'http': return
    # don't change id links
    if href[0] == '#': return
    # turn relative into absolute paths
    if href[0] != '/':
        href = '/' + href
    # remove html link for internal posts
    base, ext = os.path.splitext(href)
    if href != '/index.html' and ext == '.html': #
        href = base
    # set the link
    link.attrs['href'] = href


def process_html(fname):
    with open(fname, 'r') as f:
        soup = BeautifulSoup(f, 'lxml')
    links = soup.find_all('a')
    for link in links:
        adapt_link(link)
    with open(fname, 'w') as f:
        f.write(str(soup))

# for each org file
fnames = glob("*.org")

for fname in fnames:
    basename, ext = os.path.splitext(fname)
    if basename != 'index':
        outname = os.path.join(basename, 'index.html')
    else:
        outname = "index.html"

    if os.path.exists(outname) and \
       os.path.getmtime(outname) > os.path.getmtime(fname):
        continue

    print('converting {}'.format(fname))
    convert_org_file(fname)
    # move to folder
    if basename != 'index':
        os.makedirs(basename, exist_ok=True)
        shutil.move(basename+'.html', outname)
    process_html(outname)
