geo_pointplot <- function(pointdf, region = FALSE, type = FALSE, map = "OpenStreet") {
    
    if (sum(is.na(pointdf)) != 0) {
        stop("There is NA value in the dataframe. Please clear it.")
    }
    
    m <- leaflet::leaflet(pointdf)
    if (map == "OpenStreet") {
        m <- leaflet::addTiles(m)
    } else if (map == "amap") {
        m <- leafletCN::amap(m)
    } else if (map == "landform") {
        m <- leaflet::addProviderTiles(m, "Esri.WorldImagery")
    } else if (map == "watercolor") {
        m <- leaflet::addProviderTiles(m, "Stamen.Watercolor")
    }
    if (is.character(region)) {
        reg <- leafletCN::leafletGeo(region)
        m <- leaflet::addPolygons(m, data = reg, stroke = TRUE, smoothFactor = 1, 
            fillOpacity = 0, weight = 3)
    }
    if (type == TRUE) {
        types <- unique(pointdf[, "type"])
        ntype <- length(types)
        if (ntype <= 5) {
            cols <- as.character(wesanderson::wes_palette(n = ntype, name = "BottleRocket2"))
        } else {
            cols <- as.character(wesanderson::wes_palette(n = ntype, name = "BottleRocket2", 
                type = "continuous"))
        }
        pal <- leaflet::colorFactor(cols, domain = types)
        m <- leaflet::addCircleMarkers(m, lng = ~lng, lat = ~lat, label = ~label, 
            color = ~pal(type))
        m <- leaflet::addLegend(m, "bottomright", pal = pal, values = types)
    } else {
        m <- leaflet::addCircleMarkers(m, lng = ~lng, lat = ~lat, label = ~label)
    }
    return(m)
}
