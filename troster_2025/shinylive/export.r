# export the Shiny app to a static site

# Install specific versions (replace with the versions shown in your warnings)
install.packages("future", version = "1.40.0")
install.packages("plotly", version = "4.10.4")
install.packages("scales", version = "1.3.0")
install.packages("globals", version = "0.17.0")
install.packages("parallelly", version = "1.43.0")
install.packages("pillar", version = "1.10.2")
install.packages("tibble", version = "3.2.1")
install.packages("textshaping", version = "0.4.0")


getwd()

# change the working directory to the location of your Shiny app shinylive
setwd("C:/Users/Lenovo/Documents/github/troster_2025/troster_2025/shinylive")

library(shinylive)



shinylive::assets_info()

# remove previously exported app export in the docs folder 


# Export the Shiny app to a static site
shinylive::export(appdir = "myapp", destdir = "docs")

# Serve the static site using httpuv
library(httpuv)
# Set the working directory to the docs folder
httpuv::runStaticServer("docs/")




# Export with verbose logging
shinylive::export("app.R", "docs", verbose = TRUE)

# To run the app, you can use the following command in your R console:
# Open a web browser and navigate to http://localhost:8008
# To stop the server, you can use Ctrl+C in the R console
# To deploy the static site, you can use GitHub Pages or any other static hosting service