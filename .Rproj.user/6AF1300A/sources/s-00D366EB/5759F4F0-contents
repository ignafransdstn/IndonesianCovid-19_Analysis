##inisiasi packages httr
library(httr)
##inisiasi packages dplyr
library(dplyr)
##inisiasi  packages ggplot2 & hrbthemes
library(ggplot2)
library(hrbrthemes)
##inisiasi packages lubridate & dplyr
library(dplyr)
library(lubridate)
##Inisiasi
library(hrbrthemes)
library(tidyr)

##mengambil data pada penyedia API
resp <- GET("https://data.covid19.go.id/public/api/update.json")

##melihat status hubungan API
resp$status_code
identical(resp$status_code, status_code(resp))

## Mengetahui Metadata pada objek resp dan kapan terakir data di update
headers(resp)

##Ekstrak konten yang tersedia di API
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE)

##Mngekstrak isi respon
length(cov_id_raw)
names(cov_id_raw)
cov_id_update <- cov_id_raw$update

##Analisa Data
lapply(cov_id_update, names)
cov_id_update$penambahan$tanggal
cov_id_update$penambahan$jumlah_sembuh
cov_id_update$penambahan$jumlah_meninggal
cov_id_update$total$jumlah_positif
cov_id_update$total$jumlah_meninggal

##Menghubungkan API Kalimanatan Timur
resp_kaltim <- GET("https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_TIMUR.json")
cov_kaltim_raw <- content(resp_kaltim, as = "parsed", simplifyVector = TRUE)

##Analisa data
names(cov_kaltim_raw)
cov_kaltim_raw$kasus_total
cov_kaltim_raw$meninggal_persen
cov_kaltim_raw$sembuh_persen

##Informasi Lengkap
cov_kaltim <- cov_kaltim_raw$list_perkembangan
str(cov_kaltim)
head(cov_kaltim)

##Menyususn data janggal
new_cov_kaltim <- cov_kaltim %>% select(-contains("DIRAWAT_OR_ISOLASI")) %>% select(-starts_with("AKUMULASI")) %>% rename(kasus_baru = KASUS, meninggal = MENINGGAL, sembuh = SEMBUH) %>% mutate(tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"), tanggal = as.Date(tanggal))
str(new_cov_kaltim)

##Grafik untuk kasus positif harian
ggplot(new_cov_kaltim, aes(x = tanggal, y = kasus_baru)) + geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah Kasus Harian",
    title = "Kasus Harian Positif COVID-19 di Kalimantan Timur",
    subtitle = "Grafik di bulan agustus terus menanjak karena kurangnya aware pemerintah daerah",
    caption = "Sumber Data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 9,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

##Grafik untuk kasus sembuh harian
ggplot(new_cov_kaltim, aes(x = tanggal, y = sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah Sembuh Harian",
    title = "Grafik Jumlah Pasien Sembuh Dari Virus COVID-19 di Kalimantan Timur ",
    subtitle = "Jumlah pasien sembuh setiap harinya di Kalimantan Timur",
    caption = "Sumber Data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 9,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

##Grafik kasus meninggal akibat COVID-19 Di kalimantan Timur
ggplot(new_cov_kaltim, aes(x = tanggal, y = meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah Meninggal Harian",
    title = "Grafik Jumlah Pasien Meninggal akibat COVID-19 di Kalimantan Timur",
    subtitle = "Jumlah Pasien Meninggalsetiap harinya di Kalimantan Timur",
    caption = "Sumber Data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 9,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

##Data perminggu
cov_kaltim_mingguan <- new_cov_kaltim %>%
  count(
    tahun = year(tanggal),
    minggu_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

##inspeksi data
glimpse(cov_kaltim_mingguan)

##Perbandingan minggu ini dengan minggu kemarin
cov_kaltim_perbandingan <- cov_kaltim_mingguan %>%
  mutate(
    jumlah_pekan_lalu = dplyr::lag(jumlah),
    jumlah_pekan_lalu = ifelse(is.na(jumlah_pekan_lalu), 0, jumlah_pekan_lalu),
    lebih_baik = jumlah<jumlah_pekan_lalu
  )
glimpse(cov_kaltim_perbandingan)

##Bar chart untuk menampilkan informasi perbandingan kasus minggu ini dengan kasus minggu lalu
ggplot(cov_kaltim_perbandingan, aes(minggu_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:35, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah Kasus",
    title = "Kasus Mingguan Positif COVID-19 di Kalimnatan Timur",
    subtitle = "Kolom hijau menunjukkan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data : covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

##menghitung jumlah kasus aktif
cov_kaltim_aktif <- new_cov_kaltim %>%
  transmute(
    tanggal,
    total_kasus_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    total_sembuh = cumsum(sembuh),
    total_meninggal = cumsum(meninggal),
    total_kasus_keseluruhan = cumsum(kasus_baru)
  )
tail(cov_kaltim_aktif)

##line chart untuk kasus aktif
ggplot(data = cov_kaltim_aktif, aes(x = tanggal, y = total_kasus_aktif)) +
  geom_line()

##perubahan data untuk pivot
dim(cov_kaltim_aktif)

cov_kaltim_total_pivot <- cov_kaltim_aktif %>%
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>%
  mutate(
    kategori = sub(pattern = "total_", replacement = "", kategori)
  )
dim(cov_kaltim_total_pivot)
glimpse(cov_kaltim_total_pivot)

##finalisasi visualisasi
ggplot(cov_kaltim_total_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "kasus_aktif" = "salmon",
      "sembuh" = "olivedrab2",
      "meninggal" = "darkslategray4",
      "kasus_keseluruhan" = "yellow"
    ),
    labels = c("Aktif", "Total Kasus", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus keseluruhan",
    colour = NULL,
    title = "Visualisasi Grafik Kasus COVID-19 di Kalimnatan Timur",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
