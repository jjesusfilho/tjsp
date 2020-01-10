latlong_comarca <- function(df){

i <- imoveis %>%
     dplyr::group_by(comarca) %>%
     dplyr::mutate(lat = mean(lat),
                   lng = mean(lng)) %>%
     distinct(comarca,lat,lng)

df %>%
    dplyr::filter(!is.na(comarca)) %>%
    dplyr::mutate(comarca = fuzzy_search(comarca,imoveis$comarca)) %>%
    dplyr::left_join(i,by="comarca")

}




