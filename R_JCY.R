#Call libraries required for graphs
library(ggplot2)
library(grid)
library(sqldf)
library(gridExtra)
library(grid)
library(plotly)
library(scales)
library(celestial)
library(ggrepel)
library(ggmap)
library(rworldmap)
library(mapdata)
library(maptools)
library(mapproj)
library(ggthemes)
library(devtools)
#setpath for directory where data is there and file will be generated
setwd("E:/VDR Analaysis/In Progress/chris schulte/Data")

#read data from CSV file given in directory
Data = read.csv("data.csv", header = FALSE)
maxtime = sqldf('Select max(V1) from Data where V3 like "%GGA"')
max_t <- strptime(maxtime, "%Y/%m/%d %T", tz = "GMT")
mintime = sqldf('Select min(V1) from Data where V3 like "%GGA"')
min_t <- strptime(mintime, "%Y/%m/%d %T", tz = "GMT")


#Rudder Sensor Angle
Mytable = sqldf('Select * from Data where V3 like "%RSA"')
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
trunc(Time_, units = c("secs", "mins", "hours"))
Rudder_Angle <-
  transform(Mytable$V4, class = as.double(as.character(Mytable$V4)))
positive = Rudder_Angle$class > 0
p = ggplot(data = Mytable, aes(Time_, Rudder_Angle$class, fill = positive)) +
  geom_bar(stat = 'identity', position = 'identity') + scale_fill_manual(values =
                                                                           c("red", " dark green")) + theme(
                                                                             axis.text.x = element_text(angle = 0),
                                                                             axis.text.y = element_text(angle = 0),
                                                                             legend.position = 'none'
                                                                           ) + ggtitle(paste("Rudder Angle vs Time", "(from ", min_t, " to ", max_t, ")")) + scale_x_datetime(
                                                                             labels = date_format("%H:%M"),
                                                                             breaks = date_breaks("60 min"),
                                                                             name = "Time in UTC"
                                                                           ) + scale_y_continuous(
                                                                             name = "Rudder Angle (Degrees)",
                                                                             limits = c(-40, 40),
                                                                             breaks = seq(-40, 40, by = 10)
                                                                           )
grob1 <- grobTree(textGrob(
  "Starboard",
  x = 0.1,
  y = 0.95,
  hjust = 0,
  gp = gpar(
    col = "dark gREEN",
    fontsize = 13,
    fontface = "italic"
  )
))
grob2 <-
  grobTree(textGrob(
    "Port",
    x = 0.2,
    y = .95,
    hjust = 0,
    vjust = 25,
    gp = gpar(
      col = "red",
      fontsize = 13,
      fontface = "italic"
    )
  ))
grob3 <-
  grobTree(textGrob(
    mintime,
    x = 0.4,
    y = .95,
    hjust = 0,
    vjust = 32,
    gp = gpar(
      col = "red",
      fontsize = 13,
      fontface = "italic"
    )
  ))
grob4 <- grobTree(textGrob(
  "-",
  x = 0.65 ,
  y = .95,
  hjust = 0,
  vjust = 32,
  gp = gpar(
    col = "red",
    fontsize = 13,
    fontface = "italic"
  )
))
grob5 <-
  grobTree(textGrob(
    maxtime,
    x = 0.7,
    y = .95,
    hjust = 0,
    vjust = 32,
    gp = gpar(
      col = "red",
      fontsize = 13,
      fontface = "italic"
    )
  ))
p + theme(panel.grid.minor = element_line(
  colour = "gray",
  linetype = "dotted",
  size = .5
)) + annotation_custom(grob1) + annotation_custom(grob2) + annotation_custom(grob3) +
  annotation_custom(grob4) + annotation_custom(grob5) + geom_hline(yintercept =
                                                                     0)

ggsave("RudderAnglevstime.png")

#Depth Under Kneel
Mytable = sqldf('Select * from Data where V3 like "%DPT"')
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
DPT <- transform(Mytable$V4, class = as.double(as.character(Mytable$V4)))
ggplot(data = Mytable, aes(Time_, DPT$class)) + geom_line(color = "Blue", aes(group =
                                                                                1)) + theme(axis.text.x = element_text(angle = 0),
                                                                                            axis.text.y = element_text(angle = 0)) + ggtitle(paste("UKC vs Time", "(from ", min_t, " to ", max_t, ")")) + scale_x_datetime(
                                                                                              labels = date_format("%H:%M"),
                                                                                              breaks = date_breaks("60 min"),
                                                                                              name = "Time in UTC"
                                                                                            ) + theme(panel.grid.minor = element_line(
                                                                                              colour = "gray",
                                                                                              linetype = "dotted",
                                                                                              size = .5
                                                                                            )) + scale_y_continuous(
                                                                                              name = "Depth in Meters",
                                                                                              limits = c(0, 80),
                                                                                              breaks = seq(0, 80, by = 10)
                                                                                            )
ggsave("UKCvsTime.png")

#True Heading
Mytable = sqldf('Select * from Data where V3 like "%HDT"')
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
True_heading <-
  transform(Mytable$V4, class = as.double(as.character(Mytable$V4)))
heading <-
  transform(Mytable$V4, class = as.double(as.character(Mytable$V4)))
ggplot(data = Mytable, aes(Time_, True_heading$class)) + geom_line(color =
                                                                     "Blue", aes(group = 1)) + theme(axis.text.x = element_text(angle = 0),
                                                                                                     axis.text.y = element_text(angle = 0)) + ggtitle(paste("True Heading vs Time", "(from ", min_t, " to ", max_t, ")")) + scale_x_datetime(
                                                                                                       labels = date_format("%H:%M"),
                                                                                                       breaks = date_breaks("60 min"),
                                                                                                       name = "Time in UTC"
                                                                                                     ) + scale_y_continuous(
                                                                                                       name = "True heading (Degrees)",
                                                                                                       limits = c(0, 360),
                                                                                                       breaks = seq(0, 360, by = 20)
                                                                                                     ) + theme(panel.grid.minor = element_line(
                                                                                                       colour = "gray",
                                                                                                       linetype = "dotted",
                                                                                                       size = .5
                                                                                                     ))
ggsave("TrueHeadingvsTime.png")

#Relative WindSpeed vs time
Mytable = sqldf('Select * from Data where V3 like "%MWV" and V5="R"')
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
windspeed <-
  transform(Mytable$V6, class = as.double(as.character(Mytable$V6)))
p2 = ggplot(data = Mytable, aes(Time_, windspeed$class)) + geom_line(color =
                                                                       "blue", aes(group = 10)) + theme(axis.text.x = element_text(angle = 0),
                                                                                                        axis.text.y = element_text(angle = 90)) + ggtitle(paste("Relative WindSpeed vs Time for time between ", min_t, "-", max_t)) + scale_x_datetime(
                                                                                                          labels = date_format("%H:%M"),
                                                                                                          breaks = date_breaks("60 min"),
                                                                                                          name = "Time in UTC"
                                                                                                        ) + theme(panel.grid.minor = element_line(
                                                                                                          colour = "gray",
                                                                                                          linetype = "dotted",
                                                                                                          size = .5
                                                                                                        )) + scale_y_continuous(
                                                                                                          name = "Wind Speed (m/s)",
                                                                                                          limits = c(-35, 35),
                                                                                                          breaks = seq(-35, 35, by = 5)
                                                                                                        )
p2
ggsave("RelativeWindSpeedvsTime.png")



#Speed Through Water
Mytable = sqldf('Select * from Data where V3 like "%VBW%"')
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
MySpeed <-
  transform(Mytable$V4, class = as.double(as.character(Mytable$V4)))
positive = MySpeed$class > 0
ggplot(data = Mytable, aes(Time_, MySpeed$class)) + geom_line(color = "Blue", aes(group =
                                                                                    1)) + theme(axis.text.x = element_text(angle = 0),
                                                                                                axis.text.y = element_text(angle = 0)) + ggtitle(paste("Ground Speed vs Time", "(from ", min_t, " to ", max_t, ")")) + scale_x_datetime(
                                                                                                  labels = date_format("%H:%M"),
                                                                                                  breaks = date_breaks("60 min"),
                                                                                                  name = "Time in UTC"
                                                                                                ) + scale_y_continuous(
                                                                                                  name = "Ground Speed (Knots)",
                                                                                                  limits = c(-5, 20),
                                                                                                  breaks = seq(-5, 20, by = 1)
                                                                                                ) + theme(panel.grid.minor = element_line(
                                                                                                  colour = "gray",
                                                                                                  linetype = "dotted",
                                                                                                  size = .5
                                                                                                ))
ggsave("GroundSpeedvsTime.png")

#Track Made good Vs time
Mytable = sqldf('Select * from Data where V3 like "%VTG"')
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
TMG <- transform(Mytable$V4, class = as.double(as.character(Mytable$V4)))
ggplot(data = Mytable, aes(Time_, TMG$class)) + geom_line(color = "Blue", aes(group =
                                                                                10)) + theme(axis.text.x = element_text(angle = 0),
                                                                                             axis.text.y = element_text(angle = 0)) + ggtitle(paste("Course Made good vs Time", "(from ", min_t, " to ", max_t, ")")) + scale_x_datetime(
                                                                                               labels = date_format("%H:%M"),
                                                                                               breaks = date_breaks("60 min"),
                                                                                               name = "Time in UTC"
                                                                                             ) + theme(panel.grid.minor = element_line(
                                                                                               colour = "gray",
                                                                                               linetype = "dotted",
                                                                                               size = .5
                                                                                             )) + scale_y_continuous(
                                                                                               name = "Track Made(Degree)",
                                                                                               limits = c(0, 360),
                                                                                               breaks = seq(0, 360, by = 20)
                                                                                             )
ggsave("TrackMadeGoodvsTime.png")


#Save Rate of Turn data
Mytable = sqldf('Select * from Data where V3 like "%ROT"')
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
ROT <- transform(Mytable$V4, class = as.double(as.character(Mytable$V4)))
positive = ROT$class > 0
p = ggplot(data = Mytable, aes(Time_, ROT$class, fill = positive)) + geom_bar(stat =
                                                                                'identity', position = 'identity') + theme(
                                                                                  axis.text.x = element_text(angle = 0),
                                                                                  axis.text.y = element_text(angle = 0),
                                                                                  legend.position = 'none'
                                                                                ) + ggtitle(paste("Rate of Turn vs Time", "(from ", min_t, " to ", max_t, ")")) + scale_x_datetime(
                                                                                  labels = date_format("%H:%M"),
                                                                                  breaks = date_breaks("60 min"),
                                                                                  name = "Time in UTC"
                                                                                ) + scale_y_continuous(
                                                                                  name = "Rate of Turn (Degree/min)",
                                                                                  limits = c(-5, 5),
                                                                                  breaks = seq(-5, 5, by = .5)
                                                                                )
grob1 <- grobTree(textGrob(
  "Starboard",
  x = 0.1,
  y = 0.95,
  hjust = 0,
  gp = gpar(
    col = "Blue",
    fontsize = 13,
    fontface = "italic"
  )
))
grob2 <-
  grobTree(textGrob(
    "Port",
    x = 0.1,
    y = .95,
    hjust = 0,
    vjust = 17,
    gp = gpar(
      col = "red",
      fontsize = 13,
      fontface = "italic"
    )
  ))
A <-
  p + theme(panel.grid.minor = element_line(
    colour = "gray",
    linetype = "dotted",
    size = .5
  )) + annotation_custom(grob1) + annotation_custom(grob2) + geom_hline(yintercept =
                                                                          0)
ggplotly(A)

ggsave("RateofTurn.png")


#Save AIS data
Mytable = sqldf('Select  * from Data where V3 like "%AI%"')
write.csv(Mytable, "AIS.CSV", row.names = FALSE)

#Save GPGLL data
Mytable = sqldf(
  'Select  floor(V4/100)+(V4/100- floor(V4/100))*100/60 latitude, floor(V6/100)+(V6/100- floor(V6/100))*100/60 longitude   from Data where V3 like "%GLL"'
)
write.csv(Mytable, "GPPGLL.CSV")

#ggmap
#Save GPGGA data
Mytable = sqldf(
  'Select  (floor(V5/100)+(V5/100- floor(V5/100))*100/60)*(case WHEN V6 ="S" THEN -1  WHEN V6 ="N" THEN 1 END)  latitude,
  (floor(V7/100)+(V7/100- floor(V7/100))*100/60)*(case WHEN V8 ="W" THEN -1  WHEN V8 ="E" THEN 1 END) longitude
  from Data where V3 like "%GGA"'
)

write.csv(Mytable, "GPPGGA.CSV")


head_cord <- head(Mytable, 1)
tail_cord <- tail(Mytable, 1)

# getting the map
mapamit <-
  get_map(
    location = c(
      lon = mean(Mytable$longitude),
      lat = mean(Mytable$latitude)
    ),
    zoom = 5,
    maptype = "terrain",
    scale = 1
  )

ggmap(mapamit) + geom_point(
  data = Mytable,
  aes(
    x = Mytable$longitude,
    y = Mytable$latitude,
    fill = "green",
    alpha = 0.01
  ),
  size = .05,
  shape = 21
) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE) + geom_point(
    data = head_cord,
    aes(
      x = head_cord$longitude,
      y = head_cord$latitude,
      fill = "green",
      alpha = 0.1
    ),
    size = 6,
    shape = 21
  ) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE) + geom_point(
    data = tail_cord,
    aes(
      x = tail_cord$longitude,
      y = tail_cord$latitude,
      fill = "Yello",
      alpha = 0.1
    ),
    size = 6,
    shape = 21
  ) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE) + ggtitle(paste("GPS Map for time between ", min_t, "-", max_t))


ggsave("Map.png")

#Save ETL data

Mytable = sqldf('Select * from Data where V3 like "%ETL"')
etldistinct = sqldf('Select distinct V6 from Mytable')

ETL <- transform(Mytable$V6, class = as.double(as.character(Mytable$V6)))
Time_ <- strptime(Mytable$V1, "%Y/%m/%d %T", tz = "GMT")
ggplot(data = Mytable, aes(Time_, ETL$class)) + geom_line(color = "Blue", aes(group =
                                                                                1)) + ggtitle(paste("ETL vs Time", "(from ", min_t, " to ", max_t, ")")) + scale_x_datetime(
                                                                                  labels = date_format("%H:%M"),
                                                                                  breaks = date_breaks("60 min"),
                                                                                  name = "Time in UTC"
                                                                                )+ theme_hc()+scale_y_continuous(name="size of diamond (carats)", limits=(0,20))
write.csv(Mytable, "ETL.CSV")

ggsave("ETLVsTime.png")


#Save Distinct values
Mytable = sqldf('Select  distinct Substr(V3,4,3) V3 from Data')
Final_Distinct = sqldf(
  'select
  CASE when V3 ="AAM" then "Waypoint Arrival Alarm"
  when V3 ="ALM" then "Almanac data"
  when V3 ="APA" then "Auto Pilot A sentence"
  when V3 ="APB" then "Auto Pilot B sentence"
  when V3 ="BOD" then "Bearing Origin to Destination"
  when V3 ="BWC" then "Bearing using Great Circle route"
  when V3 ="DTM" then "Datum being used."
  when V3 ="GGA" then "Fix information"
  when V3 ="GLL" then "Lat/Lon data"
  when V3 ="GRS" then "GPS Range Residuals"
  when V3 ="GSA" then "Overall Satellite data"
  when V3 ="GST" then "GPS Pseudorange Noise Statistics"
  when V3 ="GSV" then "Detailed Satellite data"
  when V3 ="MSK" then "send control for a beacon receiver"
  when V3 ="MSS" then "Beacon receiver status information."
  when V3 ="RMA" then "recommended Loran data"
  when V3 ="RMB" then "recommended navigation data for gps"
  when V3 ="RMC" then "recommended minimum data for gps"
  when V3 ="RTE" then "route message"
  when V3 ="TRF" then "Transit Fix Data"
  when V3 ="STN" then "Multiple Data ID"
  when V3 ="VBW" then "dual Ground / Water Spped"
  when V3 ="VTG" then "Vector track an Speed over the Ground"
  when V3 ="WCV" then "Waypoint closure velocity (Velocity Made Good)"
  when V3 ="WPL" then "Waypoint Location information"
  when V3 ="XTC" then "cross track error"
  when V3 ="XTE" then "measured cross track error"
  when V3 ="ZTG" then "Zulu (UTC) time and time to go (to destination)"
  when V3 ="ZDA" then "Date and Time"
  when V3 ="WPL" then " Waypoint Location information"
  when V3 ="XTC" then " cross track error"
  when V3 ="XTE" then " measured cross track error"
  When V3="DBS" then " Depth Below Surface"
  When V3="DPT" then "Depth Below Transducer"
  When V3="GLL" then " Geographic Position "
  When V3="HDT" then "Heading True  Latitude/Longitude Data"
  When V3="VBW" then "Speed Through Water and Speed Over Ground"
  When V3="VTG" then "Track Made Good and Speed Over Ground"
  When V3="VBW" then "Speed Through Water and Speed Over Ground"
  When V3="VTG" then "Track Made Good and Speed Over Ground"
  When V3="ALR" then "Alarm"
  END AS DATA_PRESENT from Mytable'
)

Value_to_write = sqldf('select * from Final_Distinct where DATA_PRESENT is not null')

write.csv(Value_to_write, "Data_Present in VDR.CSV", row.names = F)
