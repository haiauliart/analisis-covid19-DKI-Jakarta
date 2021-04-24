library(httr)
resp <- GET("https://data.covid19.go.id/public/api/update.json")
status_code(resp)
identical(resp$status_code, status_code(resp))
headers(resp)
##mengeksstrak isi respon
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE) 
##mengekstrak isi respon pt.2
length(cov_id_raw)
names(cov_id_raw)
cov_id_update <- cov_id_raw$update
##analisa data
lapply(cov_id_update, names)
cov_id_update$penambahan$tanggal
cov_id_update$penambahan$jumlah_sembuh
cov_id_update$penambahan$jumlah_meninggal
cov_id_update$total$jumlah_positif
cov_id_update$total$jumlah_meninggal
##apa kabar dki jakarta
resp_jakarta <- GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")
cov_jakarta_raw <- content(resp_jakarta, as = "parsed", simplifyVector = TRUE)
names(cov_jakarta_raw)
cov_jakarta_raw$kasus_total
cov_jakarta_raw$meninggal_persen
cov_jakarta_raw$sembuh_persen
##informasi lebih lengkap
cov_jakarta <- cov_jakarta_raw$list_perkembangan
str(cov_jakarta)
head(cov_jakarta)
##menjinakkan data yang tidak konsisten
library(dplyr)
new_cov_jakarta <-
  cov_jakarta %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(new_cov_jakarta
    )  
##menunjukkan melalui gambar
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jakarta, aes(x = tanggal, y = kasus_baru)) +
  geom_col()
##grafik kasus positif
ggplot(new_cov_jakarta, aes(tanggal, kasus_baru)) +
  geom_col(fill="salmon") +
  labs(
    x=NULL,
    y="Jumlah kasus",
    title="Kasus Harian Positif COVID-19 di DKI Jakarta",
    subtitle="Terjadi pelonjakan kasus di awal bulan Februari karena mobilitas warga yang meningkat",
    caption="Sumber data : covid.19.go.id"
  ) +
  theme_ipsum(
    base_size=13,
    plot_title_size=21,
    grid="Y",
    ticks= TRUE
  ) +
  theme(plot.title.position="plot")
##grafik kasus sembuh
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jakarta, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di DKI Jakarta",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
##grafik kasus meninggal
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jakarta, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di DKI Jakarta",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
##apakah pekan ini lebih baik?
library(dplyr)
library(lubridate)
cov_jakarta_pekanan <- new_cov_jakarta %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )
glimpse(cov_jakarta_pekanan)
##menjawab pertanyaan
library(dplyr)
cov_jakarta_pekanan <-
  cov_jakarta_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jakarta_pekanan)
##membuat bar chart untuk kasus pekanan
library(ggplot2)
library(hrbrthemes)
ggplot(cov_jakarta_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:29, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di DKI Jakarta",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
##akumulasi kasus aktif, sembuh, dan meninggal
library(dplyr)
cov_jakarta_akumulasi <- 
  new_cov_jakarta %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
tail(cov_jakarta_akumulasi)
##line chart kasus aktif
library(ggplot2)
ggplot(data = cov_jakarta_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()
##grafik komparasi kasus aktif, sembuh, dan meninggal
library(dplyr)
library(tidyr)
dim(cov_jakarta_akumulasi)
cov_jakarta_akumulasi_pivot <- 
  cov_jakarta_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )
dim(cov_jakarta_akumulasi_pivot)
glimpse(cov_jakarta_akumulasi_pivot)
##menggunakan pivot_longer (membandingkan)
cov_jakarta_akumulasi_pivot <- cov_jakarta_akumulasi %>%
  pivot_longer(
    cols= -tanggal,
    names_to = "kategori",
    names_prefix = "akumulasi",
    values_to = "jumlah"
  )
##grafik komparasi total
library(ggplot2)
library(hrbrthemes)
ggplot(cov_jakarta_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di DKI Jakarta",
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



