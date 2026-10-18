# ================================================================
# WEBGIS INDEKS SPEKTRAL | KABUPATEN PROBOLINGGO
# Shiny + Leaflet | Sentinel-2B L2A, 3 Juli 2026, T49MGM
# ================================================================

library(shiny)
library(leaflet)
library(terra)
library(raster)
library(sf)
library(DT)

# Jalankan menggunakan shiny::runApp('D:/ETS Pemspa 2/deploy_webgis')
# Path relatif kompatibel dengan localhost dan shinyapps.io.
dir_hasil <- file.path('.', 'data', 'hasil')
nama_layer <- c('NDVI', 'NDWI', 'NDCI', 'NDBI', 'Komposit')
file_raster <- setNames(
  file.path(dir_hasil, paste0(nama_layer, '_4326.tif')),
  nama_layer
)
file_kab <- file.path(dir_hasil, 'kabupaten.geojson')
file_kec <- file.path(dir_hasil, 'kecamatan.geojson')
file_stat <- file.path(dir_hasil, 'Zonal_Statistics_Probolinggo.csv')
wajib <- c(file_raster, file_kab, file_kec, file_stat)
if (!all(file.exists(wajib))) {
  stop('File aplikasi belum lengkap:\n',
       paste(wajib[!file.exists(wajib)], collapse = '\n'),
       '\nJalankan aplikasi dari folder deploy_webgis dan periksa data/hasil.')
}

kab <- sf::st_transform(sf::st_read(file_kab, quiet = TRUE), 4326)
kec <- sf::st_transform(sf::st_read(file_kec, quiet = TRUE), 4326)
if (!'Nama_Kecamatan' %in% names(kec)) {
  stop('Kolom Nama_Kecamatan tidak ada di kecamatan.geojson.')
}
kec$Nama_Kecamatan <- trimws(as.character(kec$Nama_Kecamatan))
stat_data <- read.csv(file_stat, check.names = FALSE, stringsAsFactors = FALSE)
if (!'Kecamatan' %in% names(stat_data)) {
  stop('Kolom Kecamatan tidak ada di CSV statistik.')
}
stat_data$Kecamatan <- trimws(as.character(stat_data$Kecamatan))

# Lima raster tidak dibaca sekaligus. Cache per sesi pengguna.
siapkan_raster <- function(nama, maks_sel = 120000L) {
  r <- terra::rast(file_raster[[nama]])
  if (!terra::same.crs(r, 'EPSG:4326')) {
    stop('Raster ', nama, ' belum EPSG:4326.')
  }
  faktor <- max(1L, ceiling(sqrt(terra::ncell(r) / maks_sel)))
  if (faktor > 1L) {
    r <- terra::aggregate(
      r, fact = faktor,
      fun = if (nama == 'Komposit') 'modal' else 'mean',
      na.rm = TRUE
    )
  }
  raster::raster(r)
}

pal_indeks <- leaflet::colorNumeric(
  palette = c('#9e0142', '#fdae61', '#ffffbf', '#a6d96a', '#1a9850'),
  domain = c(-1, 1), na.color = 'transparent'
)
pal_air <- leaflet::colorNumeric(
  palette = c('#9e0142', '#fdae61', '#ffffbf', '#74add1', '#2b83ba'),
  domain = c(-1, 1), na.color = 'transparent'
)
pal_komposit <- leaflet::colorFactor(
  palette = c('#228B22', '#E74C3C', '#2196F3', '#FFFFFF'),
  domain = 1:4, na.color = 'transparent'
)

ui <- fluidPage(
  tags$head(tags$style(HTML('
    body { background:#f4f7fb; font-family:"Segoe UI",Arial,sans-serif; color:#1c3342; }
    .container-fluid { padding:0 22px; }
    .judul { text-align:center; font-size:clamp(27px,3vw,39px); font-weight:800;
      margin:24px auto 10px; color:#17384a; }
    .ringkasan { text-align:center; max-width:1000px; margin:0 auto 22px;
      font-size:15px; color:#586c79; line-height:1.65; }
    .infocard { background:#fff; border:1px solid #e7edf3; border-radius:15px;
      padding:17px 21px; margin-bottom:17px; min-height:155px;
      box-shadow:0 5px 15px rgba(26,55,77,.06); }
    .infocard h4 { font-weight:700; margin-top:0; color:#17384a; }
    .infocard p { margin-bottom:7px; font-size:13px; line-height:1.55; color:#526471; }
    .map-wrap { position:relative; width:100%; }
    #peta { border-radius:16px; overflow:hidden; box-shadow:0 8px 24px rgba(12,39,56,.12); }
    .map-wrap .floating-panel { position:absolute; top:100px; left:16px; z-index:1000;
      width:315px; max-width:calc(100% - 32px); background:rgba(255,255,255,.97);
      border:1px solid #e2e8f0; border-radius:16px; padding:18px;
      box-shadow:0 10px 30px rgba(0,0,0,.20); box-sizing:border-box;
      max-height:calc(100% - 116px); overflow-y:auto; }
    .floating-panel h4 { margin:0 0 14px; font-weight:700; color:#17384a; }
    .floating-panel .form-group { margin-bottom:12px; }
    .floating-mini { font-size:12px; color:#627784; line-height:1.5; }
    .leaflet-top.leaflet-right { top:10px; right:10px; }
    .leaflet-control-attribution { font-size:10px!important; }
    .table-card { background:#fff; border:1px solid #e7edf3; border-radius:15px;
      padding:19px; margin:20px 0 26px; box-shadow:0 5px 15px rgba(26,55,77,.06); }
    .section-title { font-weight:800; color:#17384a; margin-top:0; }
    @media (max-width:767px) {
      .container-fluid { padding:0 10px; }
      .map-wrap .floating-panel { top:95px; left:10px; width:260px;
        max-width:calc(100% - 20px); padding:12px; }
    }
  '))),
  div(class = 'judul', 'WebGIS Indeks Spektral | Kabupaten Probolinggo'),
  div(class = 'ringkasan',
      'Eksplorasi kondisi vegetasi, perairan, kawasan terbangun, dan komposit ',
      'tutupan lahan Kabupaten Probolinggo berdasarkan citra Sentinel-2B. ',
      'Pilih indeks, bandingkan dengan basemap satelit, dan pelajari statistik per kecamatan.'),
  fluidRow(
    column(4, div(class='infocard', h4('Tentang WebGIS'),
                  p('Empat indeks spektral (NDVI, NDWI, NDCI, NDBI), peta komposit, dan statistik zonal 24 kecamatan.'),
                  p('Peta dapat diganti ke basemap satelit. Matikan raster untuk melihat citra dasar saja.'))),
    column(4, div(class='infocard', h4('Panduan indeks'),
                  p('NDVI: vegetasi · NDWI: perairan · NDBI: kawasan terbangun.'),
                  p('NDCI: adaptasi Red–NIR B08/B04 yang bersifat eksploratif; pada reflektansi Sentinel-2 rumus ini identik dengan NDVI.'))),
    column(4, div(class='infocard', h4('Sumber & batasan'),
                  p('Sentinel-2B Level-2A · 3 Juli 2026 · Tile T49MGM.'),
                  p('Awan pada komposit berasal dari SCL kelas 8, 9, dan 10; ambang komposit merupakan klasifikasi awal.')))
  ),
  div(class='map-wrap',
      div(class='floating-panel',
          h4('Kontrol Peta'),
          selectInput('layer', 'Indeks spektral', choices=nama_layer, selected='NDVI'),
          checkboxInput('tampil_raster', 'Tampilkan Raster Indeks', value=TRUE),
          selectInput('filter_kec', 'Statistik kecamatan',
                      choices=c('Semua', sort(unique(stat_data$Kecamatan))),
                      selected='Semua'),
          div(class='floating-mini',
              'Ubah basemap melalui kontrol lapisan di kanan atas peta. ',
              'Raster tetap aktif ketika basemap diganti.')
      ),
      leafletOutput('peta', height='690px')
  ),
  div(class='table-card',
      h3(class='section-title', 'Statistik Zonal per Kecamatan'),
      p('Tabel otomatis mengikuti indeks dan pilihan kecamatan pada kontrol peta.'),
      DT::DTOutput('tabel'),
      tags$br(),
      downloadButton('unduh', 'Unduh statistik CSV')
  )
)

server <- function(input, output, session) {
  # Simpan hanya raster yang pernah dipilih dalam satu sesi.
  cache <- new.env(parent = emptyenv())
  
  output$peta <- leaflet::renderLeaflet({
    bb <- sf::st_bbox(kab)
    m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
    
    # Pane dibuat pada OBJEK PETA, bukan dipanggil sendirian.
    # Basemap berada di tile pane (zIndex ~200), raster di atasnya (450).
    m <- leaflet::addMapPane(m, 'rasterPane', zIndex = 450)
    m <- leaflet::addProviderTiles(m, leaflet::providers$OpenStreetMap,
                                   group = 'OpenStreetMap')
    m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery,
                                   group = 'Satelit')
    m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldTopoMap,
                                   group = 'Topografi')
    m <- leaflet::addLayersControl(m,
                                   baseGroups = c('OpenStreetMap','Satelit','Topografi'),
                                   options = leaflet::layersControlOptions(collapsed = FALSE))
    leaflet::fitBounds(m, unname(bb['xmin']), unname(bb['ymin']),
                       unname(bb['xmax']), unname(bb['ymax']))
  })
  
  # Satu observer untuk pilihan indeks dan checkbox. Tidak ada clearControls(),
  # clearShapes(), atau observer kedua yang menghapus overlay raster.
  observeEvent(list(input$layer, input$tampil_raster), {
    req(input$layer)
    m <- leaflet::leafletProxy('peta', session = session)
    m <- leaflet::clearImages(m)
    m <- leaflet::removeControl(m, layerId = 'legenda_raster')
    if (!isTRUE(input$tampil_raster)) return(invisible(NULL))
    
    nama <- input$layer
    if (!exists(nama, envir = cache, inherits = FALSE)) {
      assign(nama, siapkan_raster(nama), envir = cache)
    }
    x <- get(nama, envir = cache, inherits = FALSE)
    
    # gridOptions membuat image-overlay menggunakan pane di atas tile basemap.
    opts <- leaflet::gridOptions(pane = 'rasterPane')
    if (identical(nama, 'Komposit')) {
      m <- leaflet::addRasterImage(m, x, colors = pal_komposit,
                                   opacity = 0.85, project = FALSE, maxBytes = 20*1024*1024,
                                   options = opts, layerId = 'raster_indeks', group = 'Raster Indeks')
      m <- leaflet::addLegend(m, position = 'bottomright',
                              colors = c('#228B22','#E74C3C','#2196F3','#FFFFFF'),
                              labels = c('Vegetasi','Terbangun','Perairan','Awan (SCL)'),
                              title = 'Komposit', opacity = 1, layerId = 'legenda_raster')
    } else {
      pal <- if (identical(nama, 'NDWI')) pal_air else pal_indeks
      m <- leaflet::addRasterImage(m, x, colors = pal,
                                   opacity = 0.85, project = FALSE, maxBytes = 20*1024*1024,
                                   options = opts, layerId = 'raster_indeks', group = 'Raster Indeks')
      m <- leaflet::addLegend(m, position = 'bottomright', pal = pal,
                              values = seq(-1,1,length.out = 100), title = nama,
                              labFormat = leaflet::labelFormat(digits = 2),
                              layerId = 'legenda_raster')
    }
  }, ignoreInit = FALSE)
  
  data_tabel <- shiny::reactive({
    shiny::req(input$layer, input$filter_kec)
    dat <- stat_data
    if (!identical(input$filter_kec, 'Semua')) {
      dat <- dat[dat$Kecamatan == input$filter_kec, , drop = FALSE]
    }
    if (identical(input$layer, 'Komposit')) {
      cols <- c('Kecamatan', paste0(c('NDVI','NDWI','NDCI','NDBI'), '_mean'))
    } else {
      cols <- c('Kecamatan', paste0(input$layer, '_', c('mean','min','max','sd')))
    }
    cols <- intersect(cols, names(dat))
    dat[, cols, drop = FALSE]
  })
  
  output$tabel <- DT::renderDT({
    dat <- data_tabel()
    tbl <- DT::datatable(dat, rownames = FALSE,
                         options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
    numcols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    if (length(numcols)) tbl <- DT::formatRound(tbl, numcols, digits = 3)
    tbl
  })
  output$unduh <- shiny::downloadHandler(
    filename = function() paste0('Statistik_', input$layer, '_Probolinggo.csv'),
    content = function(file) write.csv(data_tabel(), file, row.names = FALSE)
  )
}

shiny::shinyApp(ui, server)
