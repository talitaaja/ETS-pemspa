# ============================================================
# WEBGIS PROBOLINGGO | SHINY + LEAFLET
# Sentinel-2B L2A, T49MGM, 03 Juli 2026
# ============================================================

library(shiny)
library(leaflet)
library(terra)
library(raster)
library(sf)
library(DT)

akar <- 'D:/ETS Pemspa 2'
dir_hasil <- file.path(akar, 'data', 'hasil')

nama_layer <- c('NDVI', 'NDWI', 'NDCI', 'NDBI', 'Komposit')
file_raster <- file.path(dir_hasil, paste0(nama_layer, '_4326.tif'))
names(file_raster) <- nama_layer

file_kab <- file.path(dir_hasil, 'kabupaten.geojson')
file_kec <- file.path(dir_hasil, 'kecamatan.geojson')
file_stat <- file.path(dir_hasil, 'Zonal_Statistics_Probolinggo.csv')

wajib <- c(file_raster, file_kab, file_kec, file_stat)
if (!all(file.exists(wajib))) {
  stop('File preprocessing belum lengkap:\n',
       paste(wajib[!file.exists(wajib)], collapse='\n'),
       '\nJalankan preprocess.R hingga selesai.')
}

kab <- sf::st_read(file_kab, quiet=TRUE)
kec <- sf::st_read(file_kec, quiet=TRUE)
if (is.na(sf::st_crs(kab)) || is.na(sf::st_crs(kec))) {
  stop('GeoJSON tidak mempunyai CRS yang valid.')
}
kab <- sf::st_transform(kab, 4326)
kec <- sf::st_transform(kec, 4326)

if (!('Nama_Kecamatan' %in% names(kec))) {
  stop('Kolom Nama_Kecamatan tidak ditemukan di kecamatan.geojson.')
}
kec$Nama_Kecamatan <- as.character(kec$Nama_Kecamatan)

stat_data <- read.csv(file_stat, check.names=FALSE, stringsAsFactors=FALSE)
if (!('Kecamatan' %in% names(stat_data))) {
  stop('Kolom Kecamatan tidak ditemukan di Zonal_Statistics_Probolinggo.csv.')
}
stat_data$Kecamatan <- as.character(stat_data$Kecamatan)

# Siapkan raster berukuran wajar khusus untuk ditampilkan di Leaflet.
# GeoTIFF asli tetap utuh dan tidak diubah.
# raster::raster digunakan karena leaflet::addRasterImage menerima RasterLayer.
max_sel <- 400000
siapkan_raster <- function(nama) {
  r <- terra::rast(file_raster[[nama]])
  if (!terra::same.crs(r, 'EPSG:4326')) {
    stop('Raster ', nama, ' belum menggunakan EPSG:4326.')
  }
  faktor <- max(1L, as.integer(ceiling(sqrt(terra::ncell(r) / max_sel))))
  if (faktor > 1L) {
    r <- terra::aggregate(r, fact=faktor,
                          fun=if (nama == 'Komposit') 'modal' else 'mean',
                          na.rm=TRUE)
  }
  raster::raster(r)
}

rasters <- setNames(lapply(nama_layer, siapkan_raster), nama_layer)

# Palet untuk indeks spektral
ind_pal <- leaflet::colorNumeric(
  palette = c(
    "#9e0142",
    "#fdae61",
    "#ffffbf",
    "#a6d96a",
    "#1a9850"
  ),
  domain = c(-1, 1),
  na.color = "transparent"
)

water_pal <- leaflet::colorNumeric(
  palette = c(
    "#9e0142",
    "#fdae61",
    "#ffffbf",
    "#74add1",
    "#2b83ba"
  ),
  domain = c(-1, 1),
  na.color = "transparent"
)

# Palet komposit: hanya 4 kelas (1 sampai 4)
class_pal <- leaflet::colorFactor(
  palette = c(
    "#228B22",  # 1 Vegetasi
    "#E74C3C",  # 2 Terbangun
    "#2196F3",  # 3 Perairan
    "#FFFFFF"   # 4 Awan
  ),
  domain = 1:4,
  na.color = "transparent"
)
ui <- fluidPage(
  tags$head(tags$style(HTML('
    body { background:#f5f7f9; }
    .well { background:white; border-radius:12px; }
    .judul { font-weight:700; color:#193a45; margin:14px 0; }
    .leaflet-container { border-radius:12px; }
  '))),
  titlePanel(div(class='judul', 'WebGIS Indeks Spektral | Kabupaten Probolinggo')),
  sidebarLayout(
    sidebarPanel(width=3,
                 selectInput('layer', 'Tampilan peta', choices=nama_layer),
                 checkboxInput('tampil_kec', 'Tampilkan batas kecamatan', TRUE),
                 checkboxInput('tampil_kab', 'Tampilkan batas kabupaten', TRUE),
                 selectInput('filter_kec', 'Filter statistik kecamatan',
                             choices=c('Semua', sort(unique(stat_data$Kecamatan)))),
                 tags$hr(),
                 strong('Panduan indeks'),
                 p('NDVI: vegetasi | NDWI: perairan | NDBI: kawasan terbangun.'),
                 p('NDCI: eksperimen indeks awan dengan B07 dan B03; belum tervalidasi sebagai metode deteksi awan baku.'),
                 p('Kelas awan pada peta komposit berasal dari Sentinel-2 SCL (kelas 8, 9, dan 10).'),
                 p('Threshold komposit adalah nilai awal, bukan hasil klasifikasi teruji.'),
                 p('Sumber citra: Sentinel-2B Level-2A, 3 Juli 2026, tile T49MGM.')
    ),
    mainPanel(width=9,
              leafletOutput('peta', height='600px'),
              tags$h4('Statistik zonal per kecamatan'),
              DT::DTOutput('tabel'),
              downloadButton('unduh', 'Unduh statistik CSV')
    )
  )
)

server <- function(input, output, session) {
  output$peta <- renderLeaflet({
    m <- leaflet(options=leafletOptions(preferCanvas=TRUE)) |>
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; OpenStreetMap contributors'
      )
    bb <- sf::st_bbox(kab)
    leaflet::fitBounds(m, unname(bb['xmin']), unname(bb['ymin']),
                       unname(bb['xmax']), unname(bb['ymax']))
  })
  
  observe({
    req(input$layer)
    x <- rasters[[input$layer]]
    m <- leafletProxy('peta') |>
      clearImages() |>
      clearShapes() |>
      clearControls()
    
    if (identical(input$layer, 'Komposit')) {
      m <- leaflet::addRasterImage(
        m, x, colors=pal_kelas, opacity=0.8,
        project=FALSE, maxBytes=20*1024*1024, layerId='raster'
      )
      m <- leaflet::addLegend(
        m, position='bottomright',
        colors=c('#228B22','#E74C3C','#2196F3','#FFFFFF'),
        labels=c('Vegetasi','Terbangun','Perairan','Awan (SCL)'),
        title='Kelas komposit', opacity=1
      )
    } else {
      pal <- if (identical(input$layer, 'NDWI')) pal_air else pal_indeks
      m <- leaflet::addRasterImage(
        m, x, colors=pal, opacity=0.85,
        project=FALSE, maxBytes=20*1024*1024, layerId='raster'
      )
      m <- leaflet::addLegend(
        m, position='bottomright', pal=pal,
        values=seq(-1, 1, length.out=100),
        title=input$layer,
        labFormat=leaflet::labelFormat(digits=2)
      )
    }
    
    if (isTRUE(input$tampil_kab)) {
      m <- leaflet::addPolygons(
        m, data=kab, color='#171717', weight=2,
        fill=FALSE, group='Kabupaten'
      )
    }
    if (isTRUE(input$tampil_kec)) {
      m <- leaflet::addPolygons(
        m, data=kec, color='#3d4951', weight=1,
        fillOpacity=0, group='Kecamatan',
        label=~Nama_Kecamatan,
        layerId=~Nama_Kecamatan,
        highlightOptions=leaflet::highlightOptions(
          weight=3, color='#f6bb42', bringToFront=TRUE
        )
      )
    }
  })
  
  observeEvent(input$peta_shape_click, {
    klik <- input$peta_shape_click
    if (!is.null(klik$id) && nzchar(as.character(klik$id))) {
      if (klik$id %in% stat_data$Kecamatan) {
        updateSelectInput(session, 'filter_kec', selected=klik$id)
      }
    }
  })
  
  tabel_filter <- reactive({
    req(input$filter_kec)
    if (identical(input$filter_kec, 'Semua')) return(stat_data)
    stat_data[stat_data$Kecamatan == input$filter_kec, , drop=FALSE]
  })
  
  output$tabel <- DT::renderDT({
    dat <- tabel_filter()
    cols_angka <- names(dat)[vapply(dat, is.numeric, logical(1))]
    tampilan <- DT::datatable(
      dat, options=list(scrollX=TRUE, pageLength=10),
      rownames=FALSE, filter='top'
    )
    if (length(cols_angka)) {
      tampilan <- DT::formatRound(tampilan, columns=cols_angka, digits=3)
    }
    tampilan
  })
  
  output$unduh <- downloadHandler(
    filename=function() 'Zonal_Statistics_Probolinggo.csv',
    content=function(file) write.csv(tabel_filter(), file, row.names=FALSE)
  )
}

shinyApp(ui, server)
