#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(leaflet)

data <- readOGR("https://raw.githubusercontent.com/ClaraHimmelbauer/Work/main/Sprengel_json.GeoJSON")
BEZ <- readOGR("https://raw.githubusercontent.com/ClaraHimmelbauer/Work/main/BEZ")

attach(data@data)
data$ant.sp <- SPOE / (ABG. - UNG.)
data$ant.vp <- OEVP / (ABG. - UNG.)
data$ant.gr <- GRUE / (ABG. - UNG.)
data$ant.ne <- NEOS / (ABG. - UNG.)
data$ant.fp <- FPOE / (ABG. - UNG.)
data$ant.hc <- HC / (ABG. - UNG.)
data$ant.so <- SONST / (ABG. - UNG.)

sp <- colorRamp(c("#ffffff", "#ff0000"), interpolate = "spline")
vp <- colorRamp(c("#ffffff", "#63c3d0"), interpolate = "spline")
gr <- colorRamp(c("#ffffff", "#96d050"), interpolate = "spline")
ne <- colorRamp(c("#ffffff", "#e84088"), interpolate = "spline")
fp <- colorRamp(c("#ffffff", "#0066ff"), interpolate = "spline")
hc <- colorRamp(c("#ffffff", "#152358"), interpolate = "spline")
so <- colorRamp(c("#ffffff", "#d88a00"), interpolate = "spline")

label.sp <- paste0(round(data$ant.sp * 100, 1), " %")
label.vp <- paste0(round(data$ant.vp * 100, 1), " %")
label.gr <- paste0(round(data$ant.gr * 100, 1), " %")
label.ne <- paste0(round(data$ant.ne * 100, 1), " %")
label.fp <- paste0(round(data$ant.fp * 100, 1), " %")
label.hc <- paste0(round(data$ant.hc * 100, 1), " %")
label.so <- paste0(round(data$ant.so * 100, 1), " %")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gemeinderatswahlen Wien Wahlergebnisse"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Partei",
                        "Partei:",
                        c("SPÖ", "ÖVP", "Grüne", "NEOS", "FPÖ", "Team HC", "Sonstige")),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("Wien"),
           width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Wien <- renderLeaflet({
        
        if(input$Partei == "SPÖ") {
            leaflet() %>%
                addProviderTiles(providers$BasemapAT.grau) %>%
                addPolygons(data = data,
                            color = "black", weight = 1, smoothFactor = 1, opacity = 1,
                            fillOpacity = 0.8, fillColor = ~colorBin(sp, `ant.sp`)(`ant.sp`),
                            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                            label = label.sp) %>%
                addPolylines(data = BEZ,
                             fillOpacity = 0,
                             color = "black", weight = 3)
        } else
        if(input$Partei == "ÖVP") {
            leaflet() %>%
                addProviderTiles(providers$BasemapAT.grau) %>%
                addPolygons(data = data,
                            color = "black", weight = 1, smoothFactor = 1, opacity = 1,
                            fillOpacity = 0.8, fillColor = ~colorBin(vp, `ant.vp`)(`ant.vp`),
                            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                            label = label.vp) %>%
                addPolylines(data = BEZ,
                             fillOpacity = 0,
                             color = "black", weight = 3)
        } else
          if(input$Partei == "FPÖ") {
                leaflet() %>%
                    addProviderTiles(providers$BasemapAT.grau) %>%
                    addPolygons(data = data,
                                color = "black", weight = 1, smoothFactor = 1, opacity = 1,
                                fillOpacity = 0.8, fillColor = ~colorBin(fp, `ant.fp`)(`ant.fp`),
                                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                                label = label.fp) %>%
                  addPolylines(data = BEZ,
                               fillOpacity = 0,
                               color = "black", weight = 3)
         } else
           if(input$Partei == "Grüne") {
                  leaflet() %>%
                      addProviderTiles(providers$BasemapAT.grau) %>%
                      addPolygons(data = data,
                                  color = "black", weight = 1, smoothFactor = 1, opacity = 1,
                                  fillOpacity = 0.8, fillColor = ~colorBin(gr, `ant.gr`)(`ant.gr`),
                                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                                  label = label.gr) %>%
                   addPolylines(data = BEZ,
                                fillOpacity = 0,
                                color = "black", weight = 3)
          } else
            if(input$Partei == "NEOS") {
                   leaflet() %>%
                       addProviderTiles(providers$BasemapAT.grau) %>%
                       addPolygons(data = data,
                                   color = "black", weight = 1, smoothFactor = 1, opacity = 1,
                                   fillOpacity = 0.8, fillColor = ~colorBin(ne, `ant.ne`)(`ant.ne`),
                                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                                   label = label.ne) %>%
                    addPolylines(data = BEZ,
                                 fillOpacity = 0,
                                 color = "black", weight = 3)
          } else
            if(input$Partei == "Team HC") {
                leaflet() %>%
                    addProviderTiles(providers$BasemapAT.grau) %>%
                    addPolygons(data = data,
                                color = "black", weight = 1, smoothFactor = 1, opacity = 1,
                                fillOpacity = 0.8, fillColor = ~colorBin(hc, `ant.hc`)(`ant.hc`),
                                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                                label = label.hc) %>%
                    addPolylines(data = BEZ,
                                 fillOpacity = 0,
                                 color = "black", weight = 3)
          } else
            if(input$Partei == "Sonstige") {
                    leaflet() %>%
                        addProviderTiles(providers$BasemapAT.grau) %>%
                        addPolygons(data = data,
                                    color = "black", weight = 1, smoothFactor = 1, opacity = 1,
                                    fillOpacity = 0.8, fillColor = ~colorBin(so, `ant.so`)(`ant.so`),
                                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                                    label = label.so) %>%
                    addPolylines(data = BEZ,
                                 fillOpacity = 0,
                                 color = "black", weight = 3)
                }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
