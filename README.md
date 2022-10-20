# sp2022-honors-thesis
Contains the data and code used in the "Something in the Air" working paper produced as my Economics Undergraduate Honors Thesis at UC Berkeley in Spring 2022.

In the paper, I analyze the effect of country policy on air quality outcomes to determine which types of national policies are most effective in decreasing air pollution.
* I develop a theoretical framework for teasing out the different effects of policy on "source behavior" (e.g. the amount of driving in a country) and "generation intensity" (e.g. how much pollution is emitted per unit of driving).
* I proceed by matching datasets on pollution and relevant control variables from OECD to policy data scraped from ECOLEX and downloaded from UNFAO.  
* I generate regression estimates for the average effect of each policy keyword (e.g. "subsidy", "compliance mechanism", "tax credit") across a sample of the 38 OECD countries across two sectors: power generation and vehicle transport.  These sectors account for a substantial proportion of all air pollution and have high data availability.  
* I use a cumulative dummy approach for each policy keyword in each country to generate an average effect size for each keyword.  

I use python with the xpath library to scrape the html from the ECOLEX database, and do all of the data cleaning and analysis in R.
