library(readxl)
library(sp, sf)
library(mapview)
library(ggplot2)
library(ggmap)
library(ggnewscale, ggthemes)
library(gstat)
library(mapdata, maps)

setwd("C:/Users/user/OneDrive/Documents/Programming/Ancient_DNA")
df <- read_excel("C:/Users/user/OneDrive/Documents/Ancient_DNA/Master.xlsx")
df <- df[!is.na(df$Lat),]
df$Long <- as.numeric(df$Long)
df$Lat <- as.numeric(df$Lat)
df <- df[!is.na(df$Lat),]
df <- df[df$Date_mean_BP >= 100, ]

# Get world map data
world <- map_data("world")

#Global
bbox <- make_bbox(lon = df$Long, lat = df$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=df, aes(x = Long, y = Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("Global.pdf", last_plot(), width = 6, height = 4, dpi = 300)

#C
C <- df[grepl("C", df$Y_haplo, ),]
bbox <- make_bbox(lon = C$Long, lat = C$Lat, f = .1)
filtered_world <- subset(world, long >= bbox["left"] & long <= bbox["right"] & lat >= bbox["bottom"] & lat <= bbox["top"])
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=C, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

#D
D <- df[grepl("D", df$Y_haplo, ),]
bbox <- make_bbox(lon = D$Long, lat = D$Lat, f = .1)
filtered_world <- subset(world, long >= bbox["left"] & long <= bbox["right"] & lat >= bbox["bottom"] & lat <= bbox["top"])
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=D, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")


#E1b1b
E1b1b <- df[grepl("E1b1b", df$Y_haplo, ),]
bbox <- make_bbox(lon = E1b1b$Long, lat = E1b1b$Lat, f = .1)
#filtered_world <- subset(world, long >= bbox["left"] & long <= bbox["right"] & lat >= bbox["bottom"] & lat <= bbox["top"])
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=E1b1b, aes(x = Long, y = Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("E1b1b.pdf", last_plot(), width = 6, height = 4, dpi = 300)

R1a <- df[grepl("R1a", df$Y_haplo, ),]
r1abbox <- make_bbox(lon = R1a$Long, lat = R1a$Lat, f = .1)
bbox <- make_bbox(r1abbox, r1bbbox_rb)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE) +
  theme_void() +
  geom_point(data=R1a, aes(x = Long, y = Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar") +
  new_scale_color() +
  geom_point(data=R1b, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fbff", high = "#08306b", guide = "colorbar")
ggsave("R1.pdf", last_plot(), width = 6, height = 4, dpi = 300)

R1b <- df[grepl("R1b", df$Y_haplo, ),]
r1bbbox_rb <- make_bbox(lon = R1b$Long, lat = R1b$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=R1b, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

R2 <- df[grepl("R2", df$Y_haplo, ),]
bbox <- make_bbox(lon = R2$Long, lat = R2$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=R2, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("R2.pdf", last_plot(), width = 6, height = 4, dpi = 300)

G2 <- df[grepl("G2", df$Y_haplo, ),]
bbox <- make_bbox(lon = G2$Long, lat = G2$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=G2, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("G2.pdf", last_plot(), width = 6, height = 4, dpi = 300)

J1 <- df[grepl("J1", df$Y_haplo, ),]
bbox <- make_bbox(lon = J1$Long, lat = J1$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=J1, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("J1.pdf", last_plot(), width = 6, height = 4, dpi = 300)

J2 <- df[grepl("J2", df$Y_haplo, ),]
bbox <- make_bbox(lon = J2$Long, lat = J2$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=J2, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("J2.pdf", last_plot(), width = 6, height = 4, dpi = 300)

J2a <- df[grepl("J2a", df$Y_haplo, ),]
bbox <- make_bbox(lon = J2$Long, lat = J2$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=J2a, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("J2a.pdf", last_plot(), width = 6, height = 4, dpi = 300)

J2a1a1a2 <- df[grepl("J2a1a1a2", df$Y_haplo, ),]
bbox <- make_bbox(lon = J2a1a1a2$Long, lat = J2a1a1a2$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=J2a, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("J2a1a1a2.pdf", last_plot(), width = 6, height = 4, dpi = 300)

J2b <- df[grepl("J2b", df$Y_haplo, ),]
bbox <- make_bbox(lon = J2$Long, lat = J2$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=J2b, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("J2b.pdf", last_plot(), width = 6, height = 4, dpi = 300)

H <- df[grepl("H", df$Y_haplo, ),]
bbox <- make_bbox(lon = H$Long, lat = H$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=H, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

L <- df[grepl("L", df$Y_haplo, ),]
bbox <- make_bbox(lon = L$Long, lat = L$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=L, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")


T <- df[grepl("T", df$Y_haplo, ),]
bbox <- make_bbox(lon = T$Long, lat = T$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=T, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

#I1
I1 <- df[grepl("I1", df$Y_haplo, ),]
bbox <- make_bbox(lon = I1$Long, lat = I1$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=I1, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

#I2
I2 <- df[grepl("I2", df$Y_haplo, ),]
bbox <- make_bbox(lon = I2$Long, lat = I2$Lat, f = .1)
filtered_world <- subset(world, long >= bbox["left"] & long <= bbox["right"] & lat >= bbox["bottom"] & lat <= bbox["top"])
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=I2, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

#Q
Q <- df[grepl("Q", df$Y_haplo, ),]
bbox <- make_bbox(lon = Q$Long, lat = Q$Lat, f = .1)
filtered_world <- subset(world, long >= bbox["left"] & long <= bbox["right"] & lat >= bbox["bottom"] & lat <= bbox["top"])
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=Q, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

#N
N <- df[grepl("N", df$Y_haplo, ),]
bbox <- make_bbox(lon = N$Long, lat = N$Lat, f = .1)
filtered_world <- subset(world, long >= bbox["left"] & long <= bbox["right"] & lat >= bbox["bottom"] & lat <= bbox["top"])
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=N, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")


#O
O <- df[grepl("O", df$Y_haplo, ),]
bbox <- make_bbox(lon = O$Long, lat = O$Lat, f = .1)
filtered_world <- subset(world, long >= bbox["left"] & long <= bbox["right"] & lat >= bbox["bottom"] & lat <= bbox["top"])
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=O, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")


#mtDNA

M <- df[grepl("M", df$mt_haplo, ),]
bbox <- make_bbox(lon = M$Long, lat = M$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=M, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

U <- df[grepl("U", df$mt_haplo, ),]
bbox <- make_bbox(lon = U$Long, lat = U$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=U, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("U_mt.pdf", last_plot(), width = 6, height = 4, dpi = 300)

V <- df[grepl("V", df$mt_haplo, ),]
bbox <- make_bbox(lon = V$Long, lat = V$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=V, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")

HV <- df[grepl("HV", df$mt_haplo, ),]
bbox <- make_bbox(lon = HV$Long, lat = HV$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=HV, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("HV_mt.pdf", last_plot(), width = 6, height = 4, dpi = 300)

W <- df[grepl("W", df$mt_haplo, ),]
bbox <- make_bbox(lon = HV$Long, lat = HV$Lat, f = .1)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "antiquewhite") +
  coord_equal() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)+
  theme_void() +
  geom_point(data=W, aes(x=Long, y=Lat, color = Date_mean_BP), size = 2, alpha = 0.5) +
  scale_color_gradient(low = "#f7fcfd", high = "#00441b", guide = "colorbar")
ggsave("W_mt.pdf", last_plot(), width = 6, height = 4, dpi = 300)