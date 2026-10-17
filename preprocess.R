# WEBGIS INDEKS SPEKTRAL KABUPATEN PROBOLINGGO
# Sentinel-2B L2A | 03 Juli 2026 | tile T49MGM
# Simpan sebagai D:/ETS Pemspa 2/preprocess.R

library(sf)
library(terra)
library(dplyr)

akar <- 'D:/ETS Pemspa 2'
dir_batas <- file.path(akar, 'data', 'batas')
dir_s2 <- file.path(akar, 'data', 'sentinel')
dir_hasil <- file.path(akar, 'data', 'hasil')
dir_temp <- file.path(akar, 'temp_terra')
dir.create(dir_hasil, recursive=TRUE, showWarnings=FALSE)
dir.create(dir_temp, recursive=TRUE, showWarnings=FALSE)
terra::terraOptions(tempdir=dir_temp, memfrac=0.2, progress=1)

# Metadata produk: quantification=10000, BOA_ADD_OFFSET=-1000.
safe_nama <- 'S2B_MSIL2A_20260703T022529_N0512_R046_T49MGM_20260703T052526.SAFE'
safe <- file.path(dir_s2, safe_nama)
if (!dir.exists(safe)) stop('Folder SAFE tidak ditemukan: ',safe,'\nEkstrak produk lengkap (termasuk IMG_DATA/*.jp2).')

# Baca SHP bersama berkas .shx, .dbf, .prj -- file .shp saja tidak cukup.
baca_batas <- function(nama) {
  f <- file.path(dir_batas, paste0(nama, '.shp'))
  for (ext in c('.shp','.shx','.dbf','.prj')) {
    f2 <- file.path(dir_batas,paste0(nama,ext))
    if (!file.exists(f2)) stop('Berkas pendamping shapefile hilang: ',f2)
  }
  x <- sf::st_read(f, quiet=TRUE)
  if (is.na(sf::st_crs(x))) stop('CRS tidak terbaca untuk ',nama)
  cat('\n',nama,': ',nrow(x),' fitur. Kolom: ',paste(names(x),collapse=', '),'\n',sep='')
  print(table(sf::st_geometry_type(x)))
  x
}

kab_asli <- baca_batas('Batas Kabupaten')
kec_asli <- baca_batas('Batas Kecamatan')

# Data batas kabupaten unggahan berisi lebih dari satu fitur.
# Jangan menyatukan semuanya menjadi wilayah Probolinggo.
# Seleksi otomatis hanya bila ada nama administrasi yang jelas.
pilih_wilayah <- function(x, nama_unit, target) {
  att <- sf::st_drop_geometry(x)
  kandidat <- names(att)[vapply(att, function(z) is.character(z) || is.factor(z), logical(1))]
  hit <- rep(FALSE,nrow(x))
  for (nm in kandidat) {
    v <- trimws(tolower(as.character(att[[nm]])))
    hit <- hit | (grepl(target, v, fixed=TRUE) & !is.na(v))
  }
  if (!any(hit)) stop('Tidak ditemukan atribut ',nama_unit,' dengan teks "',target,'".\n',
                      'Periksa atribut data (print(st_drop_geometry(kab_asli))) dan pilih secara manual.')
  x[hit, , drop=FALSE]
}

# Pilih Probolinggo dari dataset batas beberapa kabupaten.
kabupaten <- pilih_wilayah(kab_asli, 'kabupaten','probolinggo')
# Bila atribut membedakan kabupaten/kota, singkirkan baris bernama Kota Probolinggo.
att_kab <- sf::st_drop_geometry(kabupaten)
kota <- apply(att_kab,1,function(v) any(grepl('kota probolinggo',tolower(as.character(v)),fixed=TRUE)))
if (any(!kota)) kabupaten <- kabupaten[!kota,,drop=FALSE]

perbaiki_area <- function(x, label) {
  x <- sf::st_make_valid(x)
  # st_collection_extract untuk GEOMETRYCOLLECTION saja; MULTIPOLYGON dibiarkan utuh.
  tipe <- as.character(sf::st_geometry_type(x))
  if (any(tipe=='GEOMETRYCOLLECTION')) {
    x <- sf::st_collection_extract(x,'POLYGON',warn=FALSE)
  }
  x <- x[!sf::st_is_empty(x),,drop=FALSE]
  tipe <- as.character(sf::st_geometry_type(x))
  if (!nrow(x) || !all(tipe %in% c('POLYGON','MULTIPOLYGON'))) {
    stop(label,' tidak menjadi area polygon. Tipe: ',paste(unique(tipe),collapse=', '))
  }
  x
}
kabupaten <- perbaiki_area(kabupaten,'Kabupaten')
kabupaten <- sf::st_transform(kabupaten,32749)
g <- sf::st_union(sf::st_geometry(kabupaten))
if (length(g)!=1 || any(sf::st_is_empty(g))) stop('Geometri Kabupaten Probolinggo kosong.')
kabupaten <- sf::st_sf(Nama_Kabupaten='Probolinggo',geometry=g)

# Untuk shapefile kecamatan yang terdiri dari banyak kabupaten,
# gunakan irisan spasial dengan batas kabupaten agar tidak tercampur.
perbaiki_area <- function(x, label) {
  
  # Abaikan fitur kosong yang sengaja dihapus di QGIS
  x <- x[
    !sf::st_is_empty(x),
    ,
    drop = FALSE
  ]
  
  if (nrow(x) == 0) {
    stop(label, ": tidak ada geometri yang dapat digunakan.")
  }
  
  # Perbaiki geometri fitur yang tersisa
  x <- sf::st_make_valid(x)
  
  # Hilangkan geometri kosong hasil validasi, jika ada
  x <- x[
    !sf::st_is_empty(x),
    ,
    drop = FALSE
  ]
  
  # Periksa tipe geometri
  tipe <- as.character(sf::st_geometry_type(x))
  
  if (!all(tipe %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop(
      label,
      ": ditemukan geometri bukan polygon: ",
      paste(unique(tipe), collapse = ", ")
    )
  }
  
  cat(label, ":", nrow(x), "fitur digunakan.\n")
  
  return(x)
}

# Sentinel-2 L2A T49MGM: ambil band pada grid 20m.
ambil_band <- function(band,res_m=20) {
  jp2 <- list.files(safe,pattern='\\.jp2$',recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
  file <- grep(paste0('_',band,'_',res_m,'m\\.jp2$'),jp2,value=TRUE,ignore.case=TRUE)
  if (length(file)!=1) stop('Band ',band,' ',res_m,'m tidak unik/absen: ',length(file),' file. Periksa isi SAFE.')
  r <- terra::rast(file)
  if (!nzchar(terra::crs(r))) stop('CRS JP2 kosong. Perbaiki konfigurasi GDAL/PROJ, jangan paksa CRS secara sembarangan: ',file)
  if (!terra::same.crs(r,'EPSG:32749')) stop('Band berbeda CRS dengan UTM 49S: ',file)
  r
}

# Cegah proses lama jika ROI tidak tercakup tile.
tes <- ambil_band('B03',20)
if (is.null(terra::intersect(terra::ext(tes),terra::ext(roi)))) stop('Tile T49MGM tidak mencakup ROI; perlu tile tambahan.')

crop_band <- function(band,res_m=20) {
  cat('Membaca ',band,'...\n',sep='')
  r <- terra::crop(ambil_band(band,res_m),roi,snap='out')
  if (band=='SCL') r <- terra::ifel(r==0,NA,r)
  else r <- terra::ifel(r==0 | r==65535,NA,r)
  r
}
B03 <- crop_band('B03')
B04 <- crop_band('B04')
B07 <- crop_band('B07')
B11 <- crop_band('B11')
SCL <- crop_band('SCL')
B08_10 <- crop_band('B08',10)
B08 <- terra::resample(B08_10,B03,method='bilinear')

align <- function(r,method='bilinear') {
  if (terra::compareGeom(r,B03,stopOnError=FALSE)) return(r)
  terra::resample(r,B03,method=method)
}
B04 <- align(B04)
B07 <- align(B07)
B11 <- align(B11)
SCL <- align(SCL,'near')

mask_save <- function(r,nama) terra::mask(r,roi,filename=file.path(dir_hasil,paste0('CROP_',nama,'.tif')),overwrite=TRUE)
B03 <- mask_save(B03,'B03')
B04 <- mask_save(B04,'B04')
B07 <- mask_save(B07,'B07')
B08 <- mask_save(B08,'B08')
B11 <- mask_save(B11,'B11')
SCL <- mask_save(SCL,'SCL')
if (!terra::compareGeom(B03,B04,B07,B08,B11,SCL,stopOnError=FALSE)) stop('Band tidak satu grid.')

# Metadata level-2A: reflectance = (DN - 1000) / 10000.
ref <- function(r) (r-1000)/10000
green <- ref(B03); red <- ref(B04); rededge <- ref(B07)
nir <- ref(B08); swir <- ref(B11)

# NDCI berikut hanyalah indeks beda ternormalisasi B07-B03 yang EKSPERIMENTAL,
# belum indeks deteksi awan Sentinel-2 yang tervalidasi. Kelas awan dari SCL.
indeks <- function(a,b,nama,mask_cloud=TRUE) {
  den <- a+b
  x <- terra::ifel(abs(den)<1e-6,NA,(a-b)/den)
  bad <- is.na(SCL) | SCL %in% c(0,1)
  if (mask_cloud) bad <- bad | SCL %in% c(3,8,9,10,11)
  x <- terra::ifel(bad,NA,x)
  x <- terra::writeRaster(x,file.path(dir_hasil,paste0(nama,'.tif')),overwrite=TRUE,datatype='FLT4S')
  names(x) <- nama
  x
}
NDVI <- indeks(nir,red,'NDVI')
NDWI <- indeks(green,nir,'NDWI')
NDCI <- indeks(rededge,green,'NDCI',mask_cloud=FALSE)
NDBI <- indeks(swir,nir,'NDBI')

# Klasifikasi 4 kelas; SCL menentukan awan.
komposit <- terra::lapp(c(NDVI,NDWI,NDBI,SCL),fun=function(v,w,b,s) {
  out <- rep(NA_integer_,length(s))
  ok <- !is.na(s) & !(s %in% c(0,1,3,8,9,10,11))
  out[which(ok & !is.na(v) & v>0.3)] <- 1L
  out[which(ok & !is.na(v) & !is.na(b) & b>0.1 & b>v)] <- 2L
  out[which(ok & !is.na(w) & w>0.15)] <- 3L
  out[which(!is.na(s) & s %in% c(8,9,10))] <- 4L
  out
},filename=file.path(dir_hasil,'Komposit.tif'),overwrite=TRUE,wopt=list(datatype='INT1U'))
names(komposit) <- 'Komposit'

# Ringkasan zonal per kecamatan.
r_stack <- c(NDVI,NDWI,NDCI,NDBI)
tabel <- data.frame(ID=seq_len(nrow(kecamatan)),Kecamatan=kecamatan$Nama_Kecamatan)
stat_fun <- list(mean=mean,min=min,max=max,median=median,sd=sd)
for (metode in names(stat_fun)) {
  cat('Zonal statistik: ',metode,'\n',sep='')
  z <- terra::extract(r_stack,kec_vect,fun=stat_fun[[metode]],na.rm=TRUE)
  z <- z[match(tabel$ID,z$ID),,drop=FALSE]
  if (nrow(z)!=nrow(tabel) || anyNA(z$ID)) stop('Hasil zonal statistic tidak cocok dengan jumlah kecamatan.')
  names(z)[-1] <- paste0(names(z)[-1],'_',metode)
  tabel <- cbind(tabel,z[,-1,drop=FALSE])
}
tabel$ID <- NULL
write.csv(tabel,file.path(dir_hasil,'Zonal_Statistics_Probolinggo.csv'),row.names=FALSE,na='')

sf::st_write(sf::st_transform(kabupaten,4326),file.path(dir_hasil,'kabupaten.geojson'),delete_dsn=TRUE,quiet=TRUE)
sf::st_write(sf::st_transform(kecamatan,4326),file.path(dir_hasil,'kecamatan.geojson'),delete_dsn=TRUE,quiet=TRUE)

# Proyeksikan ke EPSG:4326 untuk aplikasi Leaflet.
template <- terra::project(NDVI,'EPSG:4326',method='bilinear')
for (nama in c('NDVI','NDWI','NDCI','NDBI')) {
  cat('WebGIS ',nama,'\n',sep='')
  terra::project(terra::rast(file.path(dir_hasil,paste0(nama,'.tif'))),template,
                 method='bilinear',filename=file.path(dir_hasil,paste0(nama,'_4326.tif')),
                 overwrite=TRUE,wopt=list(datatype='FLT4S'))
}
terra::project(komposit,template,method='near',filename=file.path(dir_hasil,'Komposit_4326.tif'),
               overwrite=TRUE,wopt=list(datatype='INT1U'))

png(file.path(dir_hasil,'Preview_Indeks.png'),width=1400,height=1100)
par(mfrow=c(2,2))
plot(NDVI,main='NDVI - Vegetasi'); plot(NDWI,main='NDWI - Perairan')
plot(NDCI,main='NDCI - Awan (eksperimen)'); plot(NDBI,main='NDBI - Terbangun')
dev.off()
png(file.path(dir_hasil,'Preview_Komposit.png'),width=1200,height=900)
plot(komposit,col=c('#228B22','#E74C3C','#2196F3','#FFFFFF'),type='classes',main='Komposit Probolinggo')
dev.off()
cat('\nPREPROCESSING SELESAI: ',dir_hasil,'\n',sep='')
