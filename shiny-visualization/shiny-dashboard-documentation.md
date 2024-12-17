# Shiny dashboard documentation
## Major decisions
* **Date filter:** Did not use a date filter because it causes issues due to how data are pulled. For example, if there is even one DOI from 2020, even if all of the rest of the DOIs are from 2021, using the date filter makes it appear as if the data consist of records from 2020 and 2021 – which is technically true, but very easy to misinterpret. Makes more sense to have users manually add a date.
* **Field names:** Did not change any of the field names in the Shiny app so that the data can be replaced without any issues directly from the data pull. If you change any of the field names, whether in Shiny or the data source, you will have to make sure that the names are consistent between Shiny and the data source. 
* **How the dashboard is counting:** The total numbers are being calculated by the distinct count of DOI from the home/anchor institution. What this means is that a paper with two collaborating authors is counted the same number of times (once) as a paper with ten collaborating authors. That is because the DOI itself is a collaboration, not the number of authors. This logic also applies to the number of collaborating cities. 
## Maps/geocoding
* The ***latitude*** and ***longitude*** fields are generated using GeoNames based on the geographic information in the data. To enable GeoNames to geocode the data correctly, ensure that the the information is broken down into individual components: city, province, country. 
* If GeoNames cannot find a city or resolve a location, you may need to check your data for accuracy (e.g., correct spelling, proper country associations) and re-run the [script](https://github.com/crkn-rcdr/ORCID-Data-Visualization/blob/main/Geonames_Get_Lat_Long.R) with the updated csv file. 
    * If certain locations were not resolved after running the script, consider using their [search service](https://www.geonames.org/) to look up coordinates or validate ambigous entries, and manually input latitude and longitude values for these entries in your dataset.
* In the data pull, the columns for provinces (or states) are called **Region1** (home institution) and **Region2** (collaborating institution). 
## Further data cleaning
Consider using a tool such as OpenRefine to further clean the dataset. 
