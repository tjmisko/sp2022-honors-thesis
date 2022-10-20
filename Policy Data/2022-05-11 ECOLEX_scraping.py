import re
from lxml import html
import requests
import pandas as pd

countrydat = pd.read_csv("../aqi_countrylist.csv")
countrylist = list(countrydat["URL_Slug"])
print(f"Number of Countries: {len(countrylist)}")

dat = {"Country": [], "Type": [], "Name": [], "Date": [], "Keywords": []}
for c in countrylist:
    p = 1
    while True:
        url = "https://www.ecolex.org/result/?q=&type=legislation&xkeywords_and_=on&xkeywords=air+quality%2Fair+pollution&xkeywords=emissions&xkeywords=energy+conservation%2Fenergy+production&xcountry=" + c +"&xdate_min=1990&xdate_max=2021&page=" + str(p)
        page = requests.get(url)
        tree = html.fromstring(page.content)
        name = tree.xpath('//ul/li/h3[@class="search-result-title"]/a/@title')
        sr_type = tree.xpath('//ul[@class="search-results"]/li/div[@class="search-result-details"]/strong[@class="sr-type"]/text()')
        date = tree.xpath('//*[@class="sr-date sr-help" and not(@title="Consolidation date")]/text()[1]')
        date = [re.search("\d{4}", d).group() for d in date]
        keywords = tree.xpath('//*[@class="search-result-details"][last()]/*[@class="collapse-keywords" or @class="sr-help" or @class="sr-doctype sr-help"]/text()')
        if len(sr_type) == 0:
            break
        dat["Country"] += [c for i in range(len(sr_type))]
        dat["Type"] += sr_type
        dat["Name"] += name
        dat["Date"] += date
        dat["Keywords"] += keywords
        print(f"Data for {c}, Page {p}:")
        print(f"Type: {len(sr_type)}, Name: {len(name)}, Date: {len(date)}, Keywords: {len(keywords)}")
        if len(sr_type) == len(name) == len(date) == len(keywords):
            print("All Clear!")
        else:
            #print(date)
            count = iter(range(1, len(name) + 1))
            [print(str(next(count)) + ") " + k + "\n") for k in name]
            print("==========\n ERROR: Dimension Mismatch! \n==========")
        #print(f"Keywords: {keywords}")
        p += 1
        #source = tree.xpath('//p[@class="search-result-source"]/a/attribute()')
df = pd.DataFrame(dat)
df.to_csv("electricity_policy_data.csv")
