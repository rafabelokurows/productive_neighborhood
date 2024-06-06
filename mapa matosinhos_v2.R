suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(sf)
  library(stringi)
  library(geos)
  library(tidyverse)
  library(stringr)
  library(leaflet)
  library(leaflet.extras)
  library(leaflet.extras2)
  library(htmltools)
  sf::sf_use_s2(FALSE)
})

#### Carrega arquivos com os pontos mapeados ####

#Situação existente atual
file = "./data/doc.kml"
layers = st_layers(file)

total_layers = st_read(file,layer = "Urban Farms") %>% mutate(Description = "Urban Farms")
for(layer_name in layers$name[2:23]){
  print(layer_name)
  geo_temp = st_read(file,layer = layer_name)%>%
    mutate(Description = layer_name)
  total_layers = bind_rows(total_layers,geo_temp)

}



area_audac = st_read(file,layer = "AUDAC area- vasilija")%>%
  mutate(Description = "AUDAC area- vasilija")

total_layers = total_layers %>% #filter(Description == "Social facilities")
  mutate(Description_new = case_when(Description == "Hospitals"~"Health facilities",
                                     Description == "Soccer field"~"Recreational Area",
                                     Description == "Sports camp"~"Recreational Area",
                                     Description == "Golf camp"~"Recreational Area",
                                     Description == "Playground"~"Recreational Area",
                                     Description == "Existing park"~"Recreational Area",
                                     Description == "Square"~"Recreational Area",
                                     Description == "Civil objects"~"Social facilities",
                                     Description == "Churches"~"Social facilities",
                                     Description == "Cemitery"~"Brownfield",
                                     Description == "Electricity station"~"Brownfield",
                                     Description == "Production areas nearby brownfields"~"Brownfield",
                                     Description == "Green Roofs"~"Productive Rooftop",
                                     TRUE~Description))

markets = total_layers %>% filter(!st_is(., "MULTIPOINT") & !st_is(., "POINT"))%>%
  filter(!stringr::str_detect(Name,"Multi"))%>%    sf::st_zm() %>%
  filter(Description == "Food markets") %>%
  #st_collection_extract(c("GEOMETRYCOLLECTION")) %>%
  geos_unnest(max_depth = 3) %>% st_as_sf() %>% mutate(Description_new= "Food markets",Description="Food markets")

playground = total_layers %>% filter(!st_is(., "MULTIPOINT") & !st_is(., "POINT"))%>%
  filter(!stringr::str_detect(Name,"Multi"))%>%    sf::st_zm() %>%
  filter(Description == "Playground") %>%
  #st_collection_extract(c("GEOMETRYCOLLECTION")) %>%
  geos_unnest(max_depth = 3) %>% st_as_sf() %>% mutate(Description= "Playground",Description_new= "Recreational Area")

novo_ponto = st_point(c(-8.65613,41.189406)) %>%
  st_sfc(crs = 4326) %>% st_sf() %>% mutate(Description = "Institutional allotment gardens",Description_new="Institutional allotment gardens")

Spoli = total_layers %>%
  filter(str_detect(Name,"Multi")) %>%    sf::st_zm()%>%
  geos_unnest(max_depth = 10)%>% st_as_sf()  %>%
  st_cast(., "POLYGON")

#File with mapped proposed additions
file_proposta = "./data/FINAL_proposed_ua.kml"

ajustes = structure(list(no = 1:69, Name = c("Educational Edible Garden",
                                             "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Educational Edible Garden", "Urban orchard",
                                             "Urban orchard", "Educational Edible Garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Community garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                             "Educational Edible Garden", "Institutional allotment garden",
                                             "Agricultural Park", "Agricultural Park", "Educational Edible Garden",
                                             "Food Forest", "Food Forest", "Urban orchard", "Food Forest",
                                             "Food Forest", "2", "Food Forest", "Food Forest", "Food Forest",
                                             "Food Forest", "Food Forest", "Educational Edible Garden", "Food Forest",
                                             "Food Forest", "Food Forest", "Food Forest", "Food Forest", "Educational Edible Garden",
                                             "Agricultural Park", "Agricultural Park", "Untitled Polygon",
                                             "Untitled Polygon", "Untitled Polygon", "Untitled Polygon", "Untitled Polygon",
                                             "Untitled Polygon", "Untitled Polygon", "Untitled Polygon", "Untitled Polygon",
                                             "Untitled Polygon", "Untitled Polygon", "Untitled Polygon", "Food Forest",
                                             "Food Forest", "Food Forest", "Food Forest"), New_name = c("Educational Edible Garden",
                                                                                                        "Educational Edible Garden", "deletar", "deletar", "deletar",
                                                                                                        "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                                                                                        "Educational Edible Garden", "Urban orchard", "Urban orchard",
                                                                                                        "deletar", "Educational Edible Garden", "deletar", "Educational Edible Garden",
                                                                                                        "Educational Edible Garden", "Educational Edible Garden", "Educational Edible Garden",
                                                                                                        "deletar", "Urban orchard", "Community Garden", "Educational Edible Garden",
                                                                                                        "Community Garden", "Educational Edible Garden", "Educational Edible Garden",
                                                                                                        "deletar", "Urban orchard", "Educational Edible Garden", "Educational Edible Garden",
                                                                                                        "Institutional allotment garden", "Agricultural Park", "Food Forest",
                                                                                                        "Community Garden", "Food Forest", "Community Garden", "Institutional allotment garden",
                                                                                                        "deletar", "Educational Edible Garden", "Community Garden", "Institutional allotment garden",
                                                                                                        "Institutional allotment garden", "Institutional allotment garden",
                                                                                                        "deletar", "Community Garden", "Educational Edible Garden", "Community Garden",
                                                                                                        "Institutional allotment garden", "Educational Edible Garden",
                                                                                                        "Institutional allotment garden", "Institutional allotment garden",
                                                                                                        "Educational Edible Garden", "Agricultural Park", "Agricultural Park",
                                                                                                        "Urban orchard", "deletar", "Educational Edible Garden", "Educational Edible Garden",
                                                                                                        "Institutional allotment garden", "Educational Edible Garden",
                                                                                                        "Institutional allotment garden", "Institutional allotment garden",
                                                                                                        "Educational Edible Garden", "Institutional allotment garden",
                                                                                                        "Community Garden", "Community Garden", "Food Forest", "Food Forest",
                                                                                                        "deletar", "Urban orchard")), class = "data.frame", row.names = c(NA,
                                                                                                                                                                          -69L))

propostas = st_read(file_proposta, promote_to_multi = FALSE) %>%  sf::st_zm() %>% #mutate(Name = tidyr::replace_na(trimws(Name),"edible garden")) %>%
  mutate(Name = case_when(str_detect(tolower(Name),"edible")~"Educational Edible Garden",
                          str_detect(tolower(Name),"agricultural")~"Agricultural Park",
                          str_detect(tolower(Name),"forest")~"Food Forest",
                          str_detect(tolower(Name),"sem nome")~"Food Forest",
                          str_detect(tolower(Name),"sem título")~"Agricultural Park",
                          str_detect(tolower(Name),"orch")~"Urban orchard",
                          str_detect(tolower(Name),"orch")~"Urban orchard",
                          TRUE~Name)) %>%
  mutate(no= row_number())%>% left_join(
    ajustes %>%
      mutate(New_name = stringr::str_to_title(New_name)),by=c("no","Name")) %>%
  filter(New_name != "Deletar")


file_new = "./data/doc2.kml"
file_new_2 = "./data/doc3.kml"
new = st_read(file_new) %>%  sf::st_zm()
new_new = st_read(file_new_2) %>%  sf::st_zm()


propostas = propostas %>% bind_rows(new_new %>% filter(!st_is(., "LINESTRING")) %>% mutate(Name = trimws(Name)) %>%
                                      mutate(New_name = case_when(Name == "Institutional allotment gardens"~"Institutional Allotment Garden",
                                                                  Name == "Institutional allotment garden"~"Institutional Allotment Garden",
                                                                  Name == "Community garden"~"Community Garden",
                                                                  Name == "Urban Orchad"~"Urban Orchard",
                                                                  TRUE~Name)))



dead_ends = new %>% filter(st_is(., "POINT")) %>% mutate(Description_new = trimws(Name))
boulevards = new %>% filter(st_is(., "LINESTRING")) %>% mutate(Name = trimws(Name)) %>%
  bind_rows( new_new %>% filter(st_is(., "LINESTRING")) %>% mutate(Name = trimws(Name)))

#### ICONES PARA OS MAPAS ####
icones_transporte = iconList(
  "Bus stops" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\bus station.png",
    iconWidth = 25,
    iconHeight = 25
  ),
  "Metro stations" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\metro station.png",
    iconWidth = 25,
    iconHeight = 25
  )
)

icone_dead_end = makeIcon(
  iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\road-barrier.png",
  iconWidth = 25,
  iconHeight = 25
)


icones_facilities = iconList(
  "Recreational Area" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\recreational_space.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Social facilities" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\Social facilities_v1.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Health facilities" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\Health facility.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Schools" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\School.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Food markets" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\food market.png",
    iconWidth = 40,
    iconHeight = 40
  )
)



icones_agro= iconList(
  "Agricultural fields" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\agricultural fields.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Brownfield" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\brownfield_v1.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Institutional allotment gardens" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\institutional allotment garden.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Productive Backyards" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\productive backyard_v1.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Productive Rooftop" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\productive rooftop.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Unauthorized production areas" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\unauthorized production areas.png",
    iconWidth = 40,
    iconHeight = 40
  ),
  "Urban Farms" = makeIcon(
    iconUrl = "C:\\Users\\BELOKUROWSR\\Desktop\\Mapa Matosinhos\\icons\\urban farms.png",
    iconWidth = 40,
    iconHeight = 40
  )

)

#Paleta de cores para as áreas propostas
pal <- colorFactor(c('#9D438B','forestgreen','#FF5CBB','gold','#e31a1c','#69A2B0'),
                   levels = c("Agricultural Park", "Food Forest","Educational Edible Garden",
                              "Institutional Allotment Garden",
                              "Community Garden","Urban Orchard"))

#Título do mapa
tag.map.title <- tags$style(HTML("
.leaflet-control.map-title {
transform: translate(40%,50%);
position: fixed !important;
left: 0%;
text-align: left;
padding-left: 10px;
padding-right: 10px;
font-weight: bold;
font-size: 26px;
background: rgba(255,255,255,0.75);
}
"))
title <- tags$div(
  tag.map.title, HTML("Productive Neighborhood: A Case Study in Matosinhos, Northern Portugal")
)



#### Área administrativa matosinhos ####
# pt_adm2 = st_read("C:\\Users\\BELOKUROWSR\\Desktop\\Kantar\\Projeção Mercadona\\CódigoPostaisDistrito\\PT_adm2\\jt394dz4777.shp")
# ventosinhos = pt_adm2 %>%
#   filter(name_2 == "Matosinhos")
# ventosinhos2 = ventosinhos %>%
#   filter(str_detect(name_3,"Infesta|Senhora|Cust|Balio")) %>%
#   mutate(name = c("Senhora Da Hora","Custoias","Leça do Balio","São Mamede de Infesta"))


# pal <- colorFactor(c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','black','darkgray','#b15928'),
#                    domain = c("Urban Farms", "Agricultural fields","Productive Backyards","Unauthorized production areas",
#                               "Metro stations",
#                               "Bus stops","Schools","Civil objects","Hospitals","Sports camp","Playground","Food markets"))





#### MAPA INICIAL com Urban elements ####
mapa = leaflet( options = leafletOptions(attributionControl=FALSE,
                                         contextmenu = TRUE,
                                         contextmenuWidth = 150,
                                         contextmenuItems =
                                           context_mapmenuItems(
                                             context_menuItem("Show Coordinates", "function showCoordinates (e) {alert(e.latlng);}"),
                                             "-",
                                             context_menuItem("Zoom In", "function(e) {this.zoomIn()}"),
                                             "-",
                                             context_menuItem("Zoom Out", "function(e) {this.zoomOut()}", disabled=FALSE)
                                             ))) %>%
  setView(lat = 41.190560, lng = -8.643191,zoom = 15) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = area_audac%>% sf::st_zm(),color="gray",stroke = F
              #,group="Audac"
              ) %>%
  addPolygons(data = Spoli,color="green",stroke = F,group = "Green aeras") %>%
  addMarkers(icon = ~icones_facilities[Description_new],
                   label = ~ Description_new,
                   data = playground,
                   group= ~Description_new) %>%
  addMarkers(icon = ~icones_facilities[Description_new],
             label = ~ Description_new,
             data =markets,
             group= "Food markets") %>%
  addMarkers(icon = ~icones_transporte[Description_new],
             label = ~ Description_new,
             data =total_layers %>% filter(Description_new %in% c("Metro stations", "Bus stops"))%>%
                  sf::st_zm() %>% st_cast(., "POINT") ,
             group= ~Description_new) %>%
  addMarkers(icon = ~icones_facilities[Description_new],
             label = ~ Description_new,
             data =total_layers %>% filter(Description_new %in% c("Social facilities", "Recreational Area","Health facilities","Schools") & !Description %in% c("Playground"))%>%
               sf::st_zm() %>%
               ungroup %>%
             st_cast("MULTIPOINT") %>%
               st_cast(., "POINT") ,
             group= ~Description_new) %>%
  addMarkers(icon = ~icones_agro[Description_new],
             label = ~ Description_new,
             data =total_layers %>% bind_rows(novo_ponto) %>% filter(Description_new %in% c("Brownfield","Agricultural fields","Institutional allotment gardens","Productive Backyards","Unauthorized production areas","Productive Rooftop","Urban Farms"))%>%
               sf::st_zm() %>%
               ungroup %>%
               st_cast("MULTIPOINT") %>%
               st_cast(., "POINT") ,
             group= ~Description_new) %>%
  addLayersControl(overlayGroups = c("Food markets","Metro stations", "Bus stops","Social facilities", "Recreational Area","Health facilities","Schools",
                                     "Brown field",
                                     "Agricultural fields","Institutional allotment gardens","Productive Backyards","Unauthorized production areas","Productive Rooftop",
                                     "Urban Farms"),
                   options = layersControlOptions(collapsed = F))%>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center; font-weight:bold;\">Urban Elements</label>');
            $('.leaflet-control-layers-overlays').css('width', '250px');
            $('.leaflet-control-layers-overlays > label:nth-child(12)').prepend('<label style=\"text-align:center; font-weight:bold;\">Existing Food Production Areas</label>');
        }
    ") %>%
  addContextmenu()

#### ADICIONANDO INFORMAÇÕES DE PROPOSTAS ####


#propostas %>% count(New_name)
#,'#cab2d6','#b15928','#b15928','blue','black

mapa_plus = mapa %>%
  addPolygons(data = propostas, #%>% mutate(Name = tidyr::replace_na(Name,"edible garden")),
              label = ~New_name,
              stroke = T,
              fillOpacity = 0.4,
              weight = 1,
              color = ~pal(New_name),
              group = ~New_name,
              fillColor = ~pal(New_name) )%>%
  addLayersControl(overlayGroups = c(#"Audac",
                                     "Green aeras","Food markets","Metro stations", "Bus stops","Social facilities", "Recreational Area","Health facilities","Schools",
                                     "Brownfield",
                                     "Agricultural fields","Institutional allotment gardens","Productive Backyards","Unauthorized production areas","Productive Rooftop",
                                     "Urban Farms","Agricultural Park", "Food Forest","Educational Edible Garden",
                                     "Institutional Allotment Garden",
                                     "Community Garden","Urban Orchard"),
                   options = layersControlOptions(collapsed = F)) %>%
  # hideGroup(c(#"Audac",
  #             # "Green aeras","Food markets","Metro stations", "Bus stops","Social facilities", "Recreational Area","Health facilities","Schools",
  #             #                                                                     "Brownfield",
  #             #                                                                     "Agricultural fields","Institutional allotment gardens","Productive Backyards","Unauthorized production areas","Productive Rooftop",
  #             #                                                                     "Urban Farms",
  #             "Agricultural Park", "Food Forest","Educational Edible Garden",
  #             "Institutional Allotment Garden",
  #             "Community Garden","Urban Orchard"))%>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays > label:nth-child(18)').prepend('<label style=\"text-align:center; font-weight:bold;\">Proposed Additions</label>');
        }
    ")
#Se precisar adicionar título na legenda
#$('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center; font-weight:bold;\">---Titulo bonitinho que o meu mozão vai querer colocar---</label>');



#### boulevards atualizados e dead ends - enviado em 05/06 ####
#boulevards = st_read("C:\\Users\\BELOKUROWSR\\Downloads\\FINAL_food_boulevards.kml")  %>%  sf::st_zm()




mapa_plus_plus = mapa_plus %>%
  # showGroup(c(#"Audac",
  #   # "Green aeras","Food markets","Metro stations", "Bus stops","Social facilities", "Recreational Area","Health facilities","Schools",
  #   #                                                                     "Brownfield",
  #   #                                                                     "Agricultural fields","Institutional allotment gardens","Productive Backyards","Unauthorized production areas","Productive Rooftop",
  #   #                                                                     "Urban Farms",
  #   "Agricultural Park", "Food Forest","Educational Edible Garden",
  #   "Institutional Allotment Garden",
  #   "Community Garden","Urban Orchard"))%>%
  addPolylines(data = boulevards,color = "orange",dashArray = "1,7",group="Food Boulevards",weight=4) %>%
  addMarkers(icon = icone_dead_end,
             label = "Dead-end streets",
             data =dead_ends ,
             group= "Dead-end streets") %>%
  addLayersControl(overlayGroups = c(#"Audac",
    "Green aeras","Food markets","Metro stations", "Bus stops","Dead-end streets","Social facilities", "Recreational Area","Health facilities","Schools",
    "Brownfield",
    "Agricultural fields","Institutional allotment gardens","Productive Backyards","Unauthorized production areas","Productive Rooftop",
    "Urban Farms","Agricultural Park", "Food Forest","Educational Edible Garden",
    "Institutional Allotment Garden",
    "Community Garden","Urban Orchard","Food Boulevards"),
    options = layersControlOptions(collapsed = F))%>%
  addControl(title, position = "topleft", className="map-title")%>%
  htmlwidgets::onRender("
        function() {
    // Increase font size for all elements in leaflet-control-layers-overlays
    var overlays = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < overlays.length; i++) {
      overlays[i].style.fontSize = '14px'; // Set desired font size

    }

    // Add custom label with larger font size and bold text
    var label = document.createElement('label');
    label.style.textAlign = 'center';
    label.style.fontSize = '14px'; // Set desired font size for the custom label
    document.querySelector('.leaflet-control-layers-overlays').prepend(label);
  }
    ")
htmlwidgets::saveWidget(mapa_plus_plus,"final_map.html",selfcontained = T)
# htmlwidgets::saveWidget(mapa_plus_plus,"mapa_com_boulevards.html",selfcontained = T)
#
# save.image("20240603_mapas_matosinhos.rdata")

#### CONTANDO A POPULAÇÃO ####
# area_audac%>% sf::st_zm()%>%
#   st_intersection(counties) %>% # raw intersection
#   mutate(in_area = units::drop_units(st_area(.)))%>% # area of polygon
#   # calculate a proportion of births equal to proportion of area
#   mutate(N_INDIVIDUOS = N_INDIVIDUOS * in_area / SHAPE_Area)%>%
#   summarize(N_INDIVIDUOS = sum(N_INDIVIDUOS))




# leaflet() %>%
#   addPolygons(data = propostas, #%>% mutate(Name = tidyr::replace_na(Name,"edible garden")),
#               label = ~paste(Name," - "),
#               stroke = F,
#               group = ~Name )
#
#
# selectMap(
#   leaflet(propostas) %>%
#     addTiles() %>%
#     addPolygons(label = ~paste(Name," - ",no))
#     #addCircleMarkers(layerId = ~brewery)
# )
#
# propostas %>% as.data.frame() %>% select(no,Name) %>% clipr::write_clip()
#
# mapa_propostas_ajustar = leaflet(propostas %>% filter(no == 68)) %>%
#   addTiles() %>%
#   addPolygons(label = ~paste(Name," - ",no))


#usethis::edit_r_profile()

#htmltools::save_html(lplot, "leaflet.html")
#Adicionar no HTML depois
#<label style="text-align:center; font-weight:bold">Existing Agricultural Activities</label>



#filter(Description %in% c("Sports camp","Soccer field","Hospitals","Metro stations","Schools","Bus stops","Agricultural fields")),
  #addPolygons(data=ventosinhos2,stroke = F) %>%

  # addCircleMarkers(data = total_layers %>% filter(st_is(., "POINT")),label = ~Description,color = ~pal(Description),
  #                  stroke = F,radius = 10) %>%
  # addCircleMarkers(data = total_layers %>% filter(st_is(., "MULTIPOINT")) %>% st_cast(., "POINT") %>%
  #                    filter(!Description %in% c("Bus stops","Metro stations","Soccer field","Hospitals","Sports camp","Schools")),
  #                  color = ~pal(Description),
  #                  stroke = F,weight = 2,label=~Description,fillOpacity = 0.5,radius = 10
  #            ) %>%



#
#
# total_layers %>% filter(st_is(., "MULTIPOINT")) %>% st_cast(., "POINT") %>%
#   filter(!Description %in% c("Bus stops","Metro stations")) %>%
#   count(Description)


