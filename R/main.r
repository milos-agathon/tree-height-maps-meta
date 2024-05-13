# 1. PACKAGES
#------------

install.packages("devtools")
install.packages("pacman")

devtools::install_github("TESS-Laboratory/chmloader")
devtools::install_github("rstudio/leaflet")

pacman::p_load(
    chmloader,
    terra,
    sf,
    maptiles,
    classInt,
    tidyverse,
    tidyterra,
    leaflet,
    htmlwidgets
)

# 2. DEFINE BUFFER
#-----------------

# 59.911491, 10.757933

lat <- 59.911491
long <- 10.757933

city_coords <- sf::st_point(
    c(long, lat)
) |>
sf::st_sfc(crs = 4326) |>
sf::st_buffer(
    dist = units::set_units(
        2, km
    )
)

# 3. GET TREE HEIGHT DATA
#------------------------

city_chm <- chmloader::download_chm(
    city_coords,
    filename = "oslo-chm.tif"
)

city_chm_new <- terra::ifel(
    city_chm == 0,
    NA,
    city_chm
)

terra::plot(
    city_chm_new,
    col = hcl.colors(
        64, "viridis"
    )
)

# 4. STREET TILES
#----------------

city_bbox <- sf::st_bbox(
    city_coords
)

streets <- maptiles::get_tiles(
    city_bbox,
    provider = "CartoDB.Positron",
    zoom = 15,
    crop = TRUE,
    project = FALSE
)

terra::plotRGB(
    streets
)

# 5. RASTER TO DATAFRAME
#-----------------------

city_chm_df <- as.data.frame(
    city_chm_new,
    xy = TRUE
)

names(city_chm_df)[3] <- "chm"

# 6. BREAKS AND PALETTE
#----------------------

breaks <- classInt::classIntervals(
    var = city_chm_df$chm,
    n = 6,
    style = "equal"
)$brks

colors <- hcl.colors(
    length(breaks),
    "ag_GrnYl",
    rev = TRUE
)

# 7. MAP
#-------

map <- ggplot(city_chm_df) +
tidyterra::geom_spatraster_rgb(
    data = streets,
    maxcell = 3e6
) +
geom_raster(
    aes(
        x = x, y = y,
        fill = chm
    )
) +
scale_fill_gradientn(
    name = "canopy height (m)",
    colors = colors,
    breaks = breaks,
    labels = round(breaks, 0)
) +
guides(
    fill = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(30, units = "mm"),
        title.position = "top",
        label.position = "bottom",
        title.hjust = .5,
        label.hjust = .5
    )
) +
theme_void() +
theme(
    legend.position = "top",
    legend.title = element_text(
        size = 11, color = "black"
    ),
    legend.text = element_text(
        size = 10, color = "black"
    ),
    legend.background = element_rect(
        fill = "white", color = NA
    ),
    plot.margin = unit(
        c(
            t = .2, r = -1,
            b = -1, l = -1
        ), "cm"
    )
)

ggsave(
    "oslo-tree-canopy-height-light.png",
    map,
    width = 7,
    height =7,
    units = "in",
    bg = "white"
)

# 8. INTERACTIVE MAP
#-------------------

map_interactive <- terra::plet(
    x = city_chm_new,
    col = colors,
    alpha = 1,
    tiles = "Streets",
    maxcell = 3e6,
    legend = "topright"
)

htmlwidgets::saveWidget(
    map_interactive,
    file = "oslo-chm.html",
    selfcontained = FALSE
)


