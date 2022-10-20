import re
from lxml import html
import requests
import pandas as pd

countrydat = pd.read_csv("../aqi_countrylist.csv")
countrylist = list(countrydat["URL_Slug"])
print(f"Number of Countries: {len(countrylist)}")

dat = {"Country": [], "Type": [], "Name": [], "Date": [], "Keywords": []}
p = 1
while True:
    print("looping")
    url = "https://www.ecolex.org/result/?q=vehicle+OR+automobile+OR+exhaust+OR+%22mobile+source%22&type=legislation&xkeywords_and_=True&xdate_min=1990&xdate_max=2019&xkeywords=air+quality%2Fair+pollution&xkeywords=emissions&page=" + str(p)
    page = requests.get(url)
    tree = html.fromstring(page.content)
    name = tree.xpath('//ul/li/h3[@class="search-result-title"]/a/@title')
    textna = tree.xpath('//ul/li/div[@class="search-result-details"]/*[1] | //ul/li/div[@class="search-result-details"]/text()')
    country = tree.xpath('//ul/li/div/span[@class="sr-jurisdiction sr-help"]/text()')
    sr_type = tree.xpath('//ul[@class="search-results"]/li/div[@class="search-result-details"]/strong[@class="sr-type"]/text()')
    date = tree.xpath('//*[@class="sr-date sr-help" and not(@title="Consolidation date")]/text()[1]')
    date = [re.search("\d{4}", d).group() for d in date]
    keywords = tree.xpath('//*[@class="search-result-details"][last()]/*[@class="collapse-keywords" or @class="sr-help" or @class="sr-doctype sr-help"]/text()')
    print(textna)
    if len(sr_type) == 0:
        print("breaking")
        break
    print(country)
    dat["Country"] += country
    dat["Type"] += sr_type
    dat["Name"] += name
    dat["Date"] += date
    dat["Keywords"] += keywords
    print(f"Data Page {p}:")
    print(f"Type: {len(sr_type)}, Name: {len(name)}, Country {len(country)}, Date: {len(date)}, Keywords: {len(keywords)}")
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
df.to_csv("policy_data.csv")
