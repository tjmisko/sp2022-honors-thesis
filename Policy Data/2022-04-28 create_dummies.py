import re
from lxml import html
import requests
import pandas as pd



def make_dummy(k, keyword_list):
    k = "(?i)" + k
    matchlist = [re.search(k, el) for el in keyword_list]
    return [1 if matchlist[i] is not None else 0 for i in range(len(matchlist))]

# pull down all possible keywords
keyword_set = []
p = 1
while True:
    page = requests.get('https://www.ecolex.org/api/v1.0/xkeywords-list/?lang=en&q=air+quality&type[]=treaty&type[]=decision&type[]=legislation&sortby=oldest&xdate_min=1980&xdate_max=2019&search=&page=' + str(p))
    if page.status_code == 404:
        break
    new_words = [page.json()["results"][i]["id"] for i in range(len(page.json()["results"]))]
    keyword_set += new_words
    p += 1

df = pd.read_csv("vehicle_policy_data.csv")
for k in keyword_set:
    print(k)
    df[k] = make_dummy(k, df["Keywords"])
df.to_csv("vehicle_policy_data_w_dummies.csv")
