# Replication Materials: Constitutional Regression Under Populist Government

This repository includes all replication materials for the paper [König, Jasmin Sarah; Swalve, Tilko 2023: Verhältnis demokratischer und konstitutioneller Regression unter populistischen Regierungen - Eine empirische Analyse](https://www.jasminskoenig.com/uploads/working-paper.pdf). In: [Peter Niesen (eds.): Zur Diagnose demokratischer Regression. Leviathan. Sonderband 40.](https://www.nomos-shop.de/nomos/titel/zur-diagnose-demokratischer-regression-id-105862/), as well as the dataset used for the Loop Blog Constitutional Regression - A Myth. Find the code for the blog [here](https://observablehq.com/@jasminsworkspace/constitutional-regression).

## Transparency

👩💻 In the data analyzed for the paper one case was considered twice: Moldova 2009. We corrected this mistake in the replication materials. The results are not affected by this, only the effect size changes very slightly. Further, the change in the dataset introducing a warning for one model calculated in the analysis. All of this is commented and highlighted in the code.

## Structure

💻 All code is in the folder /code. 

> 001_partydata.R combines [V-Party](https://www.v-dem.net/data/v-party-dataset/), [Ruth-Lovell & Grahn](https://ejpr.onlinelibrary.wiley.com/doi/10.1111/1475-6765.12564) and [PopuList](https://popu-list.org/) data. All data is combined based on a party - election year basis and then tranformed into a government - election year structure.

> 002_democracydata.R combines the party data with [V-Dem](https://www.v-dem.net/data/v-party-dataset/) and the [Comparative Constitutions Project](https://comparativeconstitutionsproject.org/download-data/). After joining the dataframes, we have a government - year structure. The data from the party datasets is filled until the next known observation.

> 003_analysis.R includes all regression analysis and plotting of effects.

> 004_casestudies.R includes code for the plots on Hungary and Poland.

> 005_content.R includes the plotting of rights changes.

📈 All graphs is in the folder /results/graphs. 

📄 The paper is at /results/working-paper.pdf

💾 All data is in the folder /data

> The data used for the analysis in the paper is at /data/ccpc_vdem_eu_la.rds

> The data used for the blog is at /data/blog/ccpc_vdem_eu_la_blog.csv

## Abstract

Populism and liberal democracy are – at least in parts – in conflict. Researchers have discussed a possible relationship between populist parties in government and democratic regression. In countries with sweeping populist majorities - such as Hungary - populists in power have undermined democratic institutions through constitutional changes. Our paper analyzes whether this mechanism is systematically applied by populists in government in Europe and Latin America. Using V-Dem and V-Party data, we investigate whether constitutional change leads to democratic regression more often under populist governments. The results from our multi-level model show that the relationship between populism and constitutional regression is ambiguous.
