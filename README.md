# Analisis Indeks Spektral dan Klasifikasi Tutupan Lahan Kabupaten Probolinggo
Evaluasi Tengah Semester (ETS) Mata Kuliah Pemrograman Spasial - Departemen Teknik Geomatika ITS

## Identitas Kelompok (Kelompok 14)
- Talita Adzra Junita Arief (5016241044)
- Linda Ambala Tifa Ramadhani (5016241045)

Dosen Pengampu: Mohammad Rohmaneo Darminto, S.T., M.Sc.

---

## Deskripsi Singkat Proyek
Repositori ini berisi kode sumber dan dokumentasi pengolahan data penginderaan jauh menggunakan bahasa pemrograman R. Proyek ini berfokus pada pengolahan citra multispektral **Sentinel-2B Level-2A** (tanggal 3 Juli 2026, Tile T49MGM) untuk wilayah kajian **Kabupaten Probolinggo**. 

Aktivitas utama dalam proyek ini meliputi:
1. **Pra-pemrosesan Citra & Batas Administrasi**: Validasi geometri shapefile, *crop* citra berdasarkan batas wilayah, konversi *Digital Number* (DN) ke nilai reflektansi permukaan, serta penyelarasan grid dan resolusi spasial band (B03, B04, B08, B11, dan SCL).
2. **Perhitungan Indeks Spektral**:
   - **NDVI** (Normalized Difference Vegetation Index): Identifikasi dan analisis kerapatan vegetasi.
   - **NDWI** (Normalized Difference Water Index): Ekstraksi fitur perairan.
   - **NDBI** (Normalized Difference Built-up Index): Identifikasi kawasan terbangun.
   - **NDCI** (Normalized Difference Cloud Index / adaptasi Red-NIR): Analisis spektral eksploratif.
3. **Klasifikasi Tutupan Lahan Komposit**: Menggabungkan hasil indeks spektral dan *Scene Classification Layer* (SCL) untuk mengklasifikasikan wilayah menjadi 4 kelas: Vegetasi, Kawasan Terbangun, Perairan, dan Awan.
4. **Statistik Zonal (Zonal Statistics)**: Menghitung nilai rata-rata, minimum, maksimum, median, dan standar deviasi dari setiap indeks spektral untuk masing-masing dari **24 kecamatan** di Kabupaten Probolinggo.
5. **WebGIS Interaktif**: Membangun aplikasi berbasis web menggunakan **R Shiny** dan **Leaflet** untuk menyajikan peta interaktif, grafik perbandingan kecamatan, tabel data statistik, serta fitur unduh data CSV.

---

## Struktur Direktori Repositori
```text
ETS-Pemspa/
│
├── data/
│   ├── batas/                       # Shapefile (.shp) batas kabupaten & kecamatan
│   ├── sentinel/                    # Data citra Sentinel-2B (.SAFE)
│   └── hasil/                       # Output GeoJSON, Raster (.tif), & CSV Zonal Stats
│
├── app.R                            # Script utama aplikasi R Shiny WebGIS
├── preprocess.R                     # Script R untuk pra-pemrosesan data raster & vektor
└── README.md                        # Dokumentasi proyek
```

---

## Alat dan Library yang Digunakan

### Bahasa Pemrograman & Perangkat Lunak
- R / RStudio (v4.6.1+)
- Git & GitHub

### Paket R (Libraries)
- `sf` : Pengolahan data spasial vektor (shapefile, GeoJSON).
- `terra` : Pengolahan data raster resolusi tinggi, aljabar raster, dan statistik zonal.
- `shiny` : Kerangka kerja pembangun antarmuka aplikasi web interaktif.
- `leaflet` : Pustaka pemetaan peta interaktif berbasis web.
- `raster` : Konversi layer raster ke format kompatibel Leaflet.
- `dplyr` : Manipulasi data tabel & pengelompokan agregasi.
- `DT` : Pengolahan tabel interaktif pada Shiny.

---

## Panduan Memulai (Workflow Execution)

### 1. Prasyarat & Instalasi Package
Pastikan R dan RStudio telah terinstal. Buka konsol RStudio dan jalankan perintah berikut untuk menginstal seluruh pustaka yang dibutuhkan:
```R
install.packages(c("sf", "terra", "raster", "dplyr", "shiny", "leaflet", "DT"))
```

### 2. Pra-pemrosesan Data (preprocess.R)
Jalankan file `preprocess.R` untuk mengeksekusi alur pemrosesan data:
- Pemotongan (*clipping*) citra sesuai Region of Interest (ROI) Kabupaten Probolinggo.
- Konversi nilai Digital Number (DN) menjadi nilai Reflectance:
  $$\text{Reflectance} = \frac{\text{DN} - 1000}{10000}$$
- Perhitungan raster indeks spektral (NDVI, NDWI, NDCI, NDBI) & komposit tutupan lahan.
- Perhitungan Zonal Statistics per kecamatan (`Zonal_Statistics_Probolinggo.csv`).
- Reproyeksi data spasial ke EPSG:4326 (WGS84) untuk kebutuhan Leaflet WebGIS.
```R
source("preprocess.R")
```

### 3. Menjalankan Aplikasi WebGIS (app.R)
Setelah proses pra-pemrosesan selesai dan file output pada folder `data/hasil/` lengkap, jalankan aplikasi WebGIS:
```R
library(shiny)
runApp("app.R")
```

---

## Sumber Data
1. **Citra Satelit**: Sentinel-2B Level-2A (Tanggal Akuisisi: 03 Juli 2026), diunduh dari Copernicus Data Space Ecosystem.
2. **Batas Administrasi**: Shapefile Batas Wilayah Kabupaten & Kecamatan Kabupaten Probolinggo, diunduh dari Peta Rupabumi Indonesia (RBI) - Badan Informasi Geospasial (BIG).

---

## Tautan Publik & Dokumentasi
- **Aplikasi WebGIS (Shinyapps.io)**: [Tautan WebGIS Probolinggo](https://tugas-besar-pemspa-kelompok14.shinyapps.io/webgis-probolinggo/)
- **Repositori GitHub Rekan Kelompok**:
  - Talita Adzra Junita Arief: [ETS-pemspa](https://github.com/talitaaja/ETS-pemspa)
  - Linda Ambala Tifa Ramadhani: [Project-ETS-Pemrograman-Spasial](https://github.com/lindaa31/Project-ETS-Pemrograman-Spasial)

---

## Lisensi & Keterangan
Proyek ini dibuat untuk keperluan akademik dalam mata kuliah Pemrograman Spasial, Departemen Teknik Geomatika ITS Surabaya.
