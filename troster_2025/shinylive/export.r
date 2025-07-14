# export the Shiny app to a static site


library(shinylive)



shinylive::assets_info()
shinylive::assets_download()
# Export the Shiny app to a static site
shinylive::export(appdir = "myapp", destdir = "docs")

# Serve the static site using httpuv
library(httpuv)
# Set the working directory to the docs folder
httpuv::runStaticServer("docs/")


# Install the dev version of shinylive (if needed)
remotes::install_github("posit-dev/r-shinylive")

# Export with verbose logging
shinylive::export("app.R", "docs", verbose = TRUE)

# To run the app, you can use the following command in your R console:
# Open a web browser and navigate to http://localhost:8008
# To stop the server, you can use Ctrl+C in the R console
# To deploy the static site, you can use GitHub Pages or any other static hosting service