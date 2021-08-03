#!/usr/bin/env python3

# TODO: implement this in something lighter, eg. shell or C.
# https://stackoverflow.com/questions/10740250/c-cpp-version-of-beautifulsoup-especially-at-handling-malformed-html

import time
import re
import subprocess
import requests
from bs4 import BeautifulSoup

def get_weather():
    page = requests.get('https://www.willyweather.com.au/tas/hobart/west-moonah.html')
    soup = BeautifulSoup(page.text, "html.parser")

    temps = soup.find('div', {'class': 'observational'}).find('header')

    # Remove leading whitespace from temp
    current_temp = temps.find('h1').contents[0].lstrip()

    feels_like = temps.find('h2').contents[0]

    return f'{current_temp} / {feels_like}'


def RAM_bar():
    # Value out of 20 (arbitrary)
    proportion_used = subprocess.Popen("free | grep Mem | awk '{print $3/$2 * 20.0}'", \
        shell=True, stdout=subprocess.PIPE).communicate()[0].rstrip()

    # Remove non numeric characters, convert to int
    proportion_used = int(float(re.sub('[^0-9.]','', str(proportion_used))))

    return f'RAM usage: [{proportion_used * "#"}{(20 - proportion_used) * "_"}]'

date = '$(date "+%a %d %b %Y %R")'

while True:
    weather = get_weather()
    RAM = RAM_bar()
    subprocess.Popen(f'xsetroot -name "{RAM} | {date} | {weather}"', shell=True)
    time.sleep(59.7) # Offset by approx. time it takes to get weather & RAM usage.
