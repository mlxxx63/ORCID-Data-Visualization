# Customizing your own Shiny dashboard
Take the steps below to build your own Shiny dashboard. For questions about Shiny, check out the [Posit Community](https://forum.posit.co/c/shiny/8) or the [Shiny Getting Help Page](https://shiny.posit.co/r/help.html) for other resources. If you run into issues with this Shiny dashboard, contact ORCID-CA Community support for assistance. Be sure to also review the [dashboard documentation](https://github.com/crkn-rcdr/ORCID-Data-Visualization/blob/main/shiny-visualization/shiny-dashboard-documentation.md).

1. Request a data pull from CRKN ORCID-CA Community support, or pull your own data using the R script in this repository. **Do not change any of the variable names.** Doing so will cause the Shiny app to be unable to recognize the variables. 
2. Install the packages:[^1] 
    * Un-comment or remove the hashtags from the install commands. 
3. Set the custom variables in the script to match your local setup. These variables include:  
    * Your working directory: The file path to the **folder** where the final output CSV will be stored.
    * File path: The path to the final output CSV that will be used as input for the script.  
    * Organization name: The name of your organization as it should appear in the final output.
    * Start date: The earliest date a publication can be and still be included. This should correspond to the first day of the year you selected in the `Rorcid_Crossref_Authors.R` script. For example, if you are analyzing data from 2024 to date, the start date should be `Jan 1, 2024`
    * Update date: The most recent date when the CSV File was last updated.  
    * For example: 
        * setwd("/Users/yourname/Desktop/ORCID-Data-Visualization/data")  
        * file_path <- "./orcid_data_latlng.csv"
        * organization_name <- "University of New Brunswick"  
        * start_date <- "Jan 1, 2024"        
        * update_date <- "Aug 1, 2024"      
4. To run the app:
    * In RStudio, click the **Run App** button in the top-right corner of the script editor
    * Or, in the R Console or Terminal, use the command `shiny::runApp("PATH TO YOUR APP")` to launch the app.

    Once the app is running, you can choose to view it in your browser:
   * In RStudio, you can click **Open in Browser** in the top-right corner of the Viewer panel.

If you run into any issues with these steps, refer to the Shiny resources section in the [README](https://github.com/crkn-rcdr/ORCID-Data-Visualization/blob/main/README.md#shiny-and-accessibility-resources) or reach out to ORCID-CA Community support for further assistance. 

[^1]: Once the packages are installed, you won’t need to install them again. In future sessions, you can simply leave the installation commands commented out with the hashtags.
