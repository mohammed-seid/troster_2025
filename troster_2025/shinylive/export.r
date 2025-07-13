# export the Shiny app to a static site
library(shinylive)
shinylive::export(appdir = "myapp", destdir = "docs")
# The app will be exported to the "docs" directory, which can be served as a static site

# options(download.file.method = "wininet")  # Windows built-in
# OR
# options(download.file.method = "curl")    # System curl (if installed)
# shinylive::export(appdir = "myapp", destdir = "docs")

# Serve the static site using httpuv
httpuv::runStaticServer("docs")
# To run the app, you can use the following command in your R console:
# Open a web browser and navigate to http://localhost:8008
# To stop the server, you can use Ctrl+C in the R console
# To deploy the static site, you can use GitHub Pages or any other static hosting service