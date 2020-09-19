#!/usr/bin/env python3

import subprocess
import pathlib
import os
from glob import glob
import shutil

from bs4 import BeautifulSoup

script_path = pathlib.Path(__file__).parent.absolute()
# script_path = "/jellyfish/art/website/scripts"

def convert_org_file(fname):
    cmd = ["emacs", "-q", "--batch",
           "-l", os.path.join(script_path, "batch-org-to-html.el"),
           "--eval", '(batch-org-to-html "{}")'.format(fname)]
    subprocess.run(cmd, check=True)

def adapt_link(href):
    dirname = os.path.dirname(href)
    # don't change external links
    if dirname[:4] == 'http':
        return href
    # don't change id links
    if href[0] == '#':
        return href
    # turn relative into absolute paths
    if href[0] != '/':
        href = '/' + href
    # remove html link for internal posts
    base, ext = os.path.splitext(href)
    if href != '/index.html' and ext == '.html': #
        href = base
    return href

def process_html(fname):
    with open(fname, 'r') as f:
        soup = BeautifulSoup(f, 'lxml')
    links = soup.find_all('a')
    for link in links:
        if 'href' in link.attrs:
            link.attrs['href'] = adapt_link(link.attrs['href'])
    images = soup.find_all('img')
    for img in images:
        if 'src' in img.attrs:
            img.attrs['src'] = adapt_link(img.attrs['src'])
    with open(fname, 'w') as f:
        f.write(str(soup))

# for each org file
fnames = glob("*.org")

for fname in fnames:
    basename, ext = os.path.splitext(fname)
    if basename != 'index':
        outname = os.path.join("docs", basename, 'index.html')
    else:
        outname = os.path.join("docs", "index.html")

    if os.path.exists(outname) and \
       os.path.getmtime(outname) > os.path.getmtime(fname):
        continue

    print('converting {}'.format(fname))
    convert_org_file(fname)
    # move to folder
    os.makedirs(os.path.dirname(outname), exist_ok=True)
    shutil.move(basename+'.html', outname)
    process_html(outname)
