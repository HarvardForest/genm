plan <- drake_plan(
  
  files = list.files(path="data", pattern = "^hf147-1(.*).csv$", 
                     full.names = TRUE),
  
  Data = sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id") %>% 
    select(latitude, longitude, ant.genus, ant.species) %>% 
    as.data.frame() %>% na.omit() %>% filter(ant.genus=="Aphaenogaster") %>%
    select(-ant.genus),
  
  Data[1822, c("Month", "Day", "Year")]<- c(03,08,1878),
  Data[1823, c("Month", "Day", "Year")]<- c(03,08,1878),
  Data[1824, c("Month", "Day", "Year")]<- c(04,29,1889),
  Data[1825, c("Month", "Day", "Year")]<- c(05,10,1896),
  Data[1826, c("Month", "Day", "Year")]<- c(05,01,1898),
  Data[1827, c("Month", "Day", "Year")]<- c(05,01,1898),
  Data[1828, c("Month", "Day", "Year")]<- c(07,14,1899),
  
  Data <- transform(Data, Year = as.numeric(Year)),
  
  Data$Year_Bin <- cut(Data$Year, 28),
  
  sampling_distribution_yr = ggplot(Data, aes(x=longitude, y=latitude, color=factor(Year_Bin))) +
    geom_point(shape=16, size=2) +
    labs(title="Aphaenogaster Sampling Distribution by Year"), 
  
  data.loess <- loess(Year ~ longitude * latitude, data = Data),     
  

  BClim = brick("data/YbrevBC_2.5.grd")
  
)
