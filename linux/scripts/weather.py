#!/usr/bin/env python

import json
import urllib
import urllib.parse
import urllib.request
import os


def main():
    city = "Helsinki"
    api_key = "38ca4ad757d43fc6134dfee30c4ec018"
    icon_to_glyph = {
        "01d": "",
        "02d": "",
        "03d": "",
        "04d": "",
        "09d": "",
        "10d": "",
        "11d": "",
        "13d": "",
        "50d": "?",
    }

    try:
        data = urllib.parse.urlencode({'q': city, 'appid': api_key, 'units': 'metric'})
        weather = json.loads(urllib.request.urlopen(
            'http://api.openweathermap.org/data/2.5/weather?' + data)
            .read())
        desc = weather['weather'][0]['description'].capitalize()
        icon = weather['weather'][0]['icon']
        temp = int(float(weather['main']['temp']))
        return '{} {}°C'.format(icon_to_glyph[icon], temp)
        # return '{}°C'.format(temp)
    except:
        return ''


if __name__ == "__main__":
	print(main())
