# Do Private Utilities Outperform Local Government-Owned Utilities? Evidence from German Retail Electricity

**authors:** Caroline Stiel (DIW Berlin), Astrid Cullmann (Technische Universit&auml;t Berlin), Maria Nieswand (DIW Berlin)

Research article published in Stiel, C.; Cullmann, A.; Nieswand, M. (2018): [Do Private Utilities Outperform Local Government-Owned Utilities? Evidence from German Retail Electricity.](https://doi.org/10.1111/geer.12134) German Economic Review. 19(4). pp. 401-425.

The empirical analysis was done in `R`. In this repository you find all statistical programs to conduct the analyses in the article.

## Summary

_Against the background of remunicipalisation trends in European public service sectors, this paper estimates firm-level productivity for German electricity retailers and tests whether the ownership type has a significant impact on productivity. We specify a production function for the retail sector with labour and external services as main inputs, which is estimated using a control function approach. Employing a newly constructed dataset on German utilities by the German Federal Statistical Office for the years 2003-12, we find that firm-level productivity generally increased until 2008 but not afterwards. We do not find any evidence for ownership having an impact on productivity._

## Methods and data

### Firm-level data

 We use official microdata from the _German Statistical Office_

- consisting of balance sheet and product data [_(AFiD)_](https://gitlab.com/modern-state-owned-firms/data/afid-data) from German electricity firms covering the years 2003-12
- merging data sets from 9 different surveys.

See [gitlab.com/modern-state-owned-firms/data](https://gitlab.com/modern-state-owned-firms/data) for more information on the data sources and the linkage strategy to merge all the data sources.

### Methods

We apply different statistical methods including 

- descriptive analyses
- panel data econometrics (GMM, structural estimation)
- regression techniques
- bootstrap
- hypotheses testing.


## Further reading

We discuss the results in two policy briefs

- _(german)_ Cullmann, A.; Nieswand, M.; Seifert, S.; Stiel, C. (2016): [Keine Effizienzunterschiede zwischen &ouml;ffentlichen und privaten Energieversorgungsunternehmen.](https://hdl.handle.net/10419/141295) DIW Wochenbericht. 83(20). pp. 448-453.
- _(english)_ Cullmann, A.; Nieswand, M.; Seifert, S.; Stiel, C. (2016): [No differences in efficiency between public and private utilities.](http://hdl.handle.net/10419/141288) DIW Economic
Bulletin 20/2016, pp. 233-238.

The article was part of the research project _Kommunale Infrastrukturunternehmen zwischen Energiewende und demografischem Wandel_ financed by the Leibniz Association (2013-2016). Visit https://modern-state-owned-firms.gitlab.io/landing-page/ for more information on the project and related research in this field.

