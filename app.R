# ================================================================
# WEBGIS INDEKS SPEKTRAL KABUPATEN PROBOLINGGO - REVISI
# Shiny, Leaflet | Sentinel-2B L2A, 3 Juli 2026
# ================================================================
library(shiny)
library(leaflet)
library(terra)
library(raster)
library(sf)
library(DT)

# Jalankan dari folder yang berisi app.R dan subfolder data/hasil.
dir_hasil <- file.path('data', 'hasil')
indeks <- c('NDVI', 'NDWI', 'NDCI', 'NDBI')
nama_layer <- c(indeks, 'Komposit')
berkas <- setNames(file.path(dir_hasil, paste0(nama_layer, '_4326.tif')), nama_layer)
file_kab <- file.path(dir_hasil, 'kabupaten.geojson')
file_kec <- file.path(dir_hasil, 'kecamatan.geojson')
file_stat <- file.path(dir_hasil, 'Zonal_Statistics_Probolinggo.csv')
wajib <- c(berkas, file_kab, file_kec, file_stat)
if (!all(file.exists(wajib))) {
  stop('File hasil preprocessing belum lengkap:\n',
       paste(wajib[!file.exists(wajib)], collapse = '\n'),
       '\nJalankan preprocess.R terlebih dahulu, lalu pindahkan folder data/hasil ke folder aplikasi.')
}

kab <- sf::st_transform(sf::st_read(file_kab, quiet=TRUE), 4326)
kec <- sf::st_transform(sf::st_read(file_kec, quiet=TRUE), 4326)
stopifnot('Nama_Kecamatan' %in% names(kec))
kec$Nama_Kecamatan <- trimws(as.character(kec$Nama_Kecamatan))
stat_data <- read.csv(file_stat, check.names=FALSE, stringsAsFactors=FALSE)
stopifnot('Kecamatan' %in% names(stat_data))
stat_data$Kecamatan <- trimws(as.character(stat_data$Kecamatan))

# Join berdasarkan nama (bukan posisi urutan polygon).
match_id <- match(kec$Nama_Kecamatan, stat_data$Kecamatan)
if (anyNA(match_id)) stop('Nama kecamatan pada GeoJSON tidak seluruhnya cocok dengan CSV.')
for (nm in setdiff(names(stat_data), 'Kecamatan')) {
  kec[[nm]] <- stat_data[[nm]][match_id]
}

# Warna yang secara visual berbeda untuk setiap indeks.
warna <- list(
  NDVI=c('#f4f9ed','#ccebb5','#80c978','#269c55','#075f34'),
  NDWI=c('#ecf5ff','#bbddfc','#73b8ed','#277fc0','#073b80'),
  NDCI=c('#fff3e8','#ffd7a7','#fda66a','#eb713a','#a83c21'),
  NDBI=c('#fff1f2','#ffc6c9','#f58b91','#d9394a','#881326')
)
warna_komposit <- c('#269c55','#d9394a','#277fc0','#9bdfff')
nama_komposit <- c('Vegetasi','Kawasan terbangun','Perairan','Awan (SCL)')
label_indeks <- c(NDVI='NDVI · Vegetasi', NDWI='NDWI · Perairan',
                  NDCI='NDCI · Red–NIR (eksploratif)', NDBI='NDBI · Bangunan')

# Mengubah rentang VISUALISASI saja; nilai asli raster dan statistik tidak dimodifikasi.
# Gunakan persentil 2%-98% agar kontras lebih terlihat dan outlier tak mendominasi.
siapkan_layer <- function(nama, maks_sel=120000L) {
  x <- terra::rast(berkas[[nama]])
  if (!terra::same.crs(x, 'EPSG:4326')) stop('Raster ', nama, ' bukan EPSG:4326.')
  faktor <- max(1L, ceiling(sqrt(terra::ncell(x)/maks_sel)))
  if (faktor > 1L) {
    x <- terra::aggregate(x, fact=faktor,
                          fun=if (nama=='Komposit') 'modal' else 'mean', na.rm=TRUE)
  }
  if (nama=='Komposit') return(list(r=raster::raster(x), batas=NULL, pal=NULL))
  vals <- terra::values(x, mat=FALSE)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) stop('Raster ', nama, ' tidak mempunyai nilai valid.')
  batas <- as.numeric(stats::quantile(vals, c(.02,.98), na.rm=TRUE, names=FALSE))
  if (diff(batas) < 1e-8) batas <- range(vals)
  if (diff(batas) < 1e-8) batas <- batas + c(-.001,.001)
  pal <- leaflet::colorNumeric(palette=warna[[nama]], domain=batas, na.color='transparent')
  # Tampilan di luar persentil dibuat warna ujung palet, BUKAN NA.
  x_tampil <- terra::clamp(x, lower=batas[1], upper=batas[2], values=TRUE)
  list(r=raster::raster(x_tampil), batas=batas, pal=pal)
}

fmt <- function(x, digits=3) {
  ifelse(is.na(x) | !is.finite(x), 'Tidak tersedia',
         formatC(x, digits=digits, format='f'))
}

# HTML hover/popup menggunakan data statistik yang dihitung dari raster asli.
buat_info <- function(nama_indeks) {
  tag <- function(x) htmltools::htmlEscape(as.character(x))
  sapply(seq_len(nrow(kec)), function(i) {
    a <- function(akhir) fmt(kec[[paste0(nama_indeks,'_',akhir)]][i])
    paste0('<div style="min-width:190px;color:#101c2b;line-height:1.65">',
           '<strong>',tag(kec$Nama_Kecamatan[i]),'</strong><br>',
           '<span style="font-weight:700">',tag(nama_indeks),'</span><hr style="margin:5px 0">',
           'Mean: <b>',a('mean'),'</b><br>',
           'Median: <b>',a('median'),'</b><br>',
           'Minimum: ',a('min'),'<br>',
           'Maksimum: ',a('max'),'<br>',
           'Standar deviasi: ',a('sd'),'</div>')
  }, USE.NAMES=FALSE)
}

anggota <- div(class='footer-anggota',
  strong('Tim Pengembang'),
  span('Talita Adzra Junita Arief (5016241044)'),
  span('Linda Ambala Tifa Ramadhani (5016241045)'))

ui <- fluidPage(
  tags$head(tags$meta(name='viewport', content='width=device-width, initial-scale=1')),
  tags$head(tags$style(HTML('
    body{background:#f4f7fb;color:#172333;font-family:"Segoe UI",Arial,sans-serif}
    .container-fluid{padding:0 28px}.hero{text-align:center;padding:27px 8px 15px}
    .hero h1{font-size:clamp(27px,3vw,39px);font-weight:800;color:#12325f;margin:0 0 10px}
    .hero p{font-size:15px;color:#202c3b;line-height:1.6}
    .info-card{background:#fff;border:1px solid #dce5ef;border-top:3px solid #f28b2e;border-radius:12px;
      padding:18px 19px;min-height:176px;margin-bottom:15px;box-shadow:0 3px 13px #16324f0d}
    .info-card h4{font-weight:800;color:#153765;margin:0 0 12px}
    .info-card p{font-size:13px;line-height:1.7;color:#151e2b;margin:0 0 9px}
    .swatch{display:inline-block;width:12px;height:12px;border-radius:3px;margin:0 7px 0 4px;vertical-align:middle}
    .switch-nav{width:100%;margin:12px auto 23px}
    .switch-nav>.tabbable>.nav-tabs{max-width:700px;margin:0 auto 23px}
    .switch-nav .nav-tabs{display:flex;border:1px solid #d4deea;border-radius:15px;overflow:hidden;background:#ecf2f8}
    .switch-nav .nav-tabs>li{float:none;flex:1;text-align:center}
    .switch-nav .nav-tabs>li>a{border:0!important;border-radius:0;margin:0;padding:15px 8px;font-weight:800;color:#43566d;background:transparent}
    .switch-nav .nav-tabs>li.active>a,.switch-nav .nav-tabs>li.active>a:hover{background:#fff;color:#0e346e;box-shadow:inset 0 -4px #f28b2e}
    .panel-white{background:#fff;border:1px solid #dce5ef;border-radius:15px;padding:19px;margin-bottom:22px;box-shadow:0 7px 25px #16324f12}
    .panel-white h3{margin-top:0;color:#12325f;font-weight:800}
    .panel-white p{color:#182333}.map-controls{margin-bottom:16px}
    .map-controls .form-group{margin-bottom:6px}#peta{border-radius:10px;overflow:hidden}
    .leaflet-popup-content{margin:11px 15px}.leaflet-tooltip{color:#101c2b}
    .stats-switch .shiny-options-group{display:flex;gap:8px;flex-wrap:wrap}
    .stats-switch .radio-inline{background:#eaf0f7;border:1px solid #d6e0ee;border-radius:10px;padding:10px 15px;margin:0;color:#122b50;font-weight:700}
    .stats-switch .radio-inline:has(input:checked){background:#164480;color:white;border-color:#164480}
    .top-kecamatan{font-size:16px;font-weight:750;color:#133764;background:#eef5fc;padding:12px 15px;border-radius:9px}
    .footer-anggota{display:flex;flex-wrap:wrap;gap:12px 23px;justify-content:center;align-items:center;
      padding:20px 12px 25px;color:#101e33;border-top:1px solid #d8e2ed;font-size:13px}
    .footer-anggota strong{color:#123d78}.note{font-size:12px;color:#24354a}
    .btn-default{border-radius:8px;font-weight:700;color:#123765}
    @media(max-width:767px){.container-fluid{padding:0 10px}.info-card{min-height:0}
      .panel-white{padding:12px}.hero{padding-top:16px}.switch-nav .nav-tabs>li>a{padding:13px 3px;font-size:13px}}
  '))),
  div(class='hero',
      h1('WebGIS Indeks Spektral Kabupaten Probolinggo'),
      p('Eksplorasi vegetasi, perairan, kawasan terbangun, dan kondisi wilayah dari citra Sentinel-2B Level-2A · 3 Juli 2026')),
  fluidRow(
    column(4, div(class='info-card', h4('Tentang WebGIS'),
      p('WebGIS ini membantu melihat kondisi permukaan wilayah Kabupaten Probolinggo melalui peta dan angka pada 24 kecamatan.'),
      p('Pilih halaman Peta Interaktif untuk melihat sebaran warna, atau Statistik Zonal untuk membandingkan kecamatan.'))),
    column(4, div(class='info-card', h4('Panduan Indeks'),
      p(HTML(paste0('<span class="swatch" style="background:#269c55"></span><b>NDVI</b> Vegetasi<br>',
                    '<span class="swatch" style="background:#277fc0"></span><b>NDWI</b> Perairan<br>',
                    '<span class="swatch" style="background:#eb713a"></span><b>NDCI</b> Red–NIR (eksploratif)<br>',
                    '<span class="swatch" style="background:#d9394a"></span><b>NDBI</b> Kawasan terbangun<br>',
                    '<span class="swatch" style="background:#9bdfff"></span><b>Awan</b> pada peta komposit'))),
      p('Catatan: NDCI pada data ini memakai rumus yang sama dengan NDVI, sehingga nilainya sama meskipun warnanya dibedakan.'))),
    column(4, div(class='info-card', h4('Sumber dan Batasan'),
      p(HTML('Citra satelit: <a href="https://dataspace.copernicus.eu/" target="_blank">Copernicus Data Space</a> (Sentinel-2B L2A, 3 Juli 2026).')),
      p(HTML('Batas Kabupaten dan Kecamatan Probolinggo: <a href="https://tanahair.indonesia.go.id/" target="_blank">Badan Informasi Geospasial (BIG)</a> sesuai sumber data yang digunakan.')),
      p('Peta komposit merupakan gambaran awal, bukan penentuan tutupan lahan yang sudah dicek langsung di lapangan. Awan diambil dari pengelompokan citra satelit (SCL).')))
  ),
  div(class='switch-nav',
    tabsetPanel(id='halaman', type='tabs',
      tabPanel('Peta Interaktif', value='peta',
        div(class='panel-white',
          fluidRow(class='map-controls',
            column(4, selectInput('layer', 'Pilih indeks/lapisan',
                   choices=c(label_indeks, 'Komposit Tutupan Lahan'='Komposit'), selected='NDVI')),
            column(4, selectInput('filter_kec', 'Sorot kecamatan',
                   choices=c('Semua Kecamatan'='Semua', sort(unique(stat_data$Kecamatan))), selected='Semua')),
            column(4, checkboxInput('tampil_raster', 'Tampilkan warna indeks pada peta', TRUE))),
          p(class='note', 'Arahkan kursor ke suatu kecamatan untuk melihat statistik indeks terpilih; klik wilayah untuk membuka popup yang dapat dibaca lebih lama.'),
          leafletOutput('peta', height='640px'),
          tags$br(), p(class='note', 'Skala warna menggunakan persentil 2–98 dari raster yang ditampilkan agar perbedaan warna lebih terlihat. Nilai statistik tidak diubah.')),
        anggota),
      tabPanel('Statistik Zonal', value='statistik',
        div(class='panel-white',
          h3('Perbandingan Statistik Zonal per Kecamatan'),
          p('Pilih indeks untuk melihat peringkat kecamatan, grafik rata-rata, serta tabel statistik terperinci.'),
          div(class='stats-switch', radioButtons('indeks_stat', NULL,
            choices=c('NDVI · Vegetasi'='NDVI','NDWI · Perairan'='NDWI',
                      'NDCI · Red–NIR'='NDCI','NDBI · Bangunan'='NDBI'),
            selected='NDVI',inline=TRUE)),
          div(class='top-kecamatan', textOutput('pemenang', inline=TRUE)),
          tags$br(),plotOutput('grafik', height='550px'),
          p(class='note','Grafik memperlihatkan 10 kecamatan dengan rata-rata indeks tertinggi. Nilai yang mendekati nol atau negatif tetap ditampilkan sebagaimana hasil pengolahan citra.'),
          h3('Tabel Statistik Kecamatan'),
          DTOutput('tabel'),tags$br(),
          downloadButton('unduh','Unduh Statistik CSV'),
          tags$br(),tags$br(),
          p(class='note','Mean = rata-rata; median = nilai tengah; minimum/maksimum = nilai terendah/tertinggi; standar deviasi = ukuran variasi nilai.')),
        anggota)
    )
  )
)

server <- function(input, output, session) {
  cache <- new.env(parent=emptyenv())
  ambil_layer <- function(nama) {
    if (!exists(nama, envir=cache, inherits=FALSE)) {
      assign(nama, siapkan_layer(nama), envir=cache)
    }
    get(nama, envir=cache, inherits=FALSE)
  }
  output$peta <- renderLeaflet({
    bb <- sf::st_bbox(kab)
    m <- leaflet(options=leafletOptions(preferCanvas=TRUE))
    m <- addMapPane(m, 'rasterPane', zIndex=450)
    m <- addMapPane(m, 'batasPane', zIndex=500)
    m <- addProviderTiles(m, providers$OpenStreetMap, group='OpenStreetMap')
    m <- addProviderTiles(m, providers$Esri.WorldImagery, group='Satelit')
    m <- addProviderTiles(m, providers$Esri.WorldTopoMap, group='Topografi')
    m <- addLayersControl(m, baseGroups=c('OpenStreetMap','Satelit','Topografi'),
                          options=layersControlOptions(collapsed=TRUE))
    fitBounds(m, bb[['xmin']],bb[['ymin']],bb[['xmax']],bb[['ymax']])
  })
  observeEvent(list(input$layer,input$tampil_raster), {
    req(input$layer)
    m <- leafletProxy('peta',session=session)
    m <- clearImages(m)
    m <- removeControl(m, layerId='legenda_raster')
    if (!isTRUE(input$tampil_raster)) return(invisible(NULL))
    nama <- input$layer
    dati <- ambil_layer(nama)
    if (nama=='Komposit') {
      pal <- colorFactor(palette=warna_komposit, domain=1:4, na.color='transparent')
      m <- addRasterImage(m, dati$r, colors=pal, opacity=.87, project=FALSE,
                          maxBytes=20*1024*1024, options=gridOptions(pane='rasterPane'),
                          layerId='raster_indeks')
      m <- addLegend(m,position='bottomright', colors=warna_komposit,
                     labels=nama_komposit,title='Komposit',opacity=1,layerId='legenda_raster')
    } else {
      m <- addRasterImage(m, dati$r, colors=dati$pal,opacity=.86,project=FALSE,
                          maxBytes=20*1024*1024,options=gridOptions(pane='rasterPane'),
                          layerId='raster_indeks')
      m <- addLegend(m,position='bottomright',pal=dati$pal,
                     values=seq(dati$batas[1],dati$batas[2],length.out=100),
                     title=paste(nama,'(skala data)'),
                     labFormat=labelFormat(digits=3),layerId='legenda_raster')
    }
  },ignoreInit=FALSE)

  # Batas kecamatan dan informasinya digambar ulang ketika layer/filter berubah.
  # Pane batas lebih tinggi daripada raster, sehingga hover dan klik tetap bekerja.
  observeEvent(list(input$layer,input$filter_kec), {
    req(input$layer,input$filter_kec)
    nama <- if (input$layer=='Komposit') 'NDVI' else input$layer
    informasi <- buat_info(nama)
    selected <- input$filter_kec
    berat <- if (selected=='Semua') rep(1.2,nrow(kec)) else
      ifelse(kec$Nama_Kecamatan==selected, 3.5, 1)
    garis <- if (selected=='Semua') rep('#203f60',nrow(kec)) else
      ifelse(kec$Nama_Kecamatan==selected, '#ffae25','#203f60')
    m <- leafletProxy('peta',session=session) |> clearShapes()
    m <- addPolygons(m,data=kec,group='Batas Kecamatan',
      color=garis,weight=berat,opacity=.95,fill=TRUE,
      fillColor='#ffffff',fillOpacity=.015,
      options=pathOptions(pane='batasPane'),
      layerId=~Nama_Kecamatan,
      label=lapply(informasi,HTML),popup=lapply(informasi,HTML),
      labelOptions=labelOptions(direction='auto',textsize='12px',opacity=.97),
      highlightOptions=highlightOptions(weight=3,color='#ffb327',fillOpacity=.08,
                                       bringToFront=TRUE))
    if (selected!='Semua') {
      kk <- kec[kec$Nama_Kecamatan==selected,,drop=FALSE]
      if(nrow(kk)) {
        b <- st_bbox(kk)
        fitBounds(m,b[['xmin']],b[['ymin']],b[['xmax']],b[['ymax']])
      }
    }
  }, ignoreInit=FALSE)

  tabel_terpilih <- reactive({
    req(input$indeks_stat)
    nama <- input$indeks_stat
    cols <- paste0(nama,'_',c('mean','median','min','max','sd'))
    if (!all(cols %in% names(stat_data))) stop('Kolom statistik hilang: ',paste(setdiff(cols,names(stat_data)),collapse=', '))
    x <- stat_data[,c('Kecamatan',cols),drop=FALSE]
    names(x) <- c('Kecamatan','Mean','Median','Minimum','Maksimum','Standar Deviasi')
    x[order(x$Mean,decreasing=TRUE,na.last=TRUE),,drop=FALSE]
  })
  output$pemenang <- renderText({
    d <- tabel_terpilih()
    d <- d[is.finite(d$Mean),,drop=FALSE]
    if(!nrow(d)) return('Data nilai rata-rata belum tersedia.')
    paste0('Rata-rata ',input$indeks_stat,' tertinggi: ',d$Kecamatan[1],
           ' (',fmt(d$Mean[1]),')')
  })
  output$grafik <- renderPlot({
    d <- tabel_terpilih()
    d <- head(d[is.finite(d$Mean),,drop=FALSE],10)
    validate(need(nrow(d)>0,'Tidak ada nilai valid untuk grafik'))
    # Pengurutan untuk grafik batang horizontal: tertinggi berada di atas.
    d <- d[nrow(d):1,,drop=FALSE]
    old <- par(mar=c(5,11,3,3)); on.exit(par(old))
    warna_bar <- warna[[input$indeks_stat]][4]
    lim <- range(c(0,d$Mean),finite=TRUE)
    padding <- max(diff(lim)*.16,.01)
    lim <- c(lim[1]-padding,lim[2]+padding)
    mids <- barplot(d$Mean, names.arg=d$Kecamatan, horiz=TRUE,las=1,
                    col=warna_bar,border=NA,xlim=lim,
                    xlab=paste('Nilai rata-rata',input$indeks_stat),
                    main=paste('10 Kecamatan dengan Mean',input$indeks_stat,'Tertinggi'))
    abline(v=0,col='#41536a',lty=2)
    offset <- padding*.12
    text(d$Mean+ifelse(d$Mean>=0,offset,-offset),mids,labels=fmt(d$Mean),
         pos=ifelse(d$Mean>=0,4,2),cex=.8,xpd=NA)
  })
  output$tabel <- renderDT({
    d <- tabel_terpilih()
    tb <- datatable(d,rownames=FALSE,filter='top',
                    options=list(pageLength=10,scrollX=TRUE,autoWidth=TRUE,
                                 order=list(list(1,'desc'))))
    formatRound(tb,c('Mean','Median','Minimum','Maksimum','Standar Deviasi'),digits=4)
  })
  output$unduh <- downloadHandler(
    filename=function() paste0('Statistik_',input$indeks_stat,'_Probolinggo.csv'),
    content=function(file) write.csv(tabel_terpilih(),file,row.names=FALSE,na='')
  )
}

shinyApp(ui,server)
