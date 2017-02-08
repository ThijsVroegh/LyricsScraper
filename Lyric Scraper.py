import datetime as dt
import pandas as pd
import time
import re

from bs4 import BeautifulSoup
import urllib.request

# Open the main webpage
html = urllib.request.urlopen('THE BEATLES LYRICS - I Saw Her Standing There.html')


html = open('THE BEATLES LYRICS - I Saw Her Standing There.html')

soup = BeautifulSoup(html,"html5lib")

t = soup.prettify()
# Split accorfing the headers, keeping only the necessary bits
t = t.split("Usage of azlyrics.com content by any third-party lyrics provider is prohibited by our licensing agreement. Sorry about that. -->", 1)[-1]
t = t.split("<!-- MxM banner", 1)[0]

# Remove extraneous HTML Tags
t1 = re.sub('<.*>', '', t)
# Replace whitespace, tabs, new lines with a space
t2 = re.sub('[     \\n]+', ' ', t1)
