plan <- drake_plan{
  files = list.files(path="C:/Users/12094/Desktop/apENM/data", pattern = "^hf147-1(.*).csv$", 
                     full.names = TRUE)
  
  Data = sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id") %>% 
    select(latitude, longitude, ant.genus, ant.species) %>% 
    as.data.frame() %>% na.omit() %>% filter(ant.genus=="Aphaenogaster") %>%
    select(-ant.genus)
}
