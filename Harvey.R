library(devtools)
library(ggalt)
library(rrricanes)
library(dplyr)
library(albersusa)
library(ggthemes)
require(ggmap)
require(maps)

storms <- get_storms(year=2017L, basin="AL")

us_aeqd_proj <- "+proj=aeqd +lat_0=40.08355112574181 +lon_0=-95.44921875"

filter(storms, grepl("Harvey", Name))$Link %>%
  get_storm_data() -> harvey

gis_advisory(harvey$fstadv$Key[1]) %>% 
  last() %>% 
  gis_download() -> harvey_gis

harvey_path <- shp_to_df(harvey_gis[[grep("5day_lin", names(harvey_gis), value=TRUE)]])


us <- map_data("state")
gg <- c("Texas")
gg <- cbind(geocode(as.character("San Antonio")), gg)
gg

ggplot() +
  geom_cartogram(data=us, map=us, aes(map_id=region), 
                 color="#2b2b2b", fill="#b2b2b2", size=0.25) +
  geom_path(data = harvey_path, aes(long, lat, group = FCSTPRD), size=1) +
  geom_point(data = harvey_path, aes(long, lat, group = FCSTPRD, color = STORMTYPE), size=5) +
  geom_text(data = harvey_path, aes(long, lat, label=order, group = FCSTPRD), size=4.5) +
  scale_x_continuous(limits=range(-107, -90)) +
  scale_y_continuous(limits=range(22.5, 36)) +
  # geom_polygon() +
  # coord_map() +
  geom_point(data=gg, aes(x=lon, y=lat), size=5,color="pink")+
  geom_text(data =gg, aes(lon, lat, label="SA"), size=3) +
  #coord_proj(us_aeqd_proj) +
  ggthemes::theme_map()
