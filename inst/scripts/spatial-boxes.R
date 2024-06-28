
detroit_tracts = sf::st_crop(mi_tracts, xmin = -83.541, xmax = -82.775, ymin = 42.05, ymax = 42.719)
flint_tracts = sf::st_crop(mi_tracts, xmin = -83.9, xmax = -83.541, ymin = 42.93, ymax = 43.1)
ann_arbor_tracts = sf::st_crop(mi_tracts, xmin = -83.9, xmax = -83.541, ymin = 42.15, ymax = 42.35)
lansing_tracts = sf::st_crop(mi_tracts, xmin = -84.9, xmax = -84.3, ymin = 42.45, ymax = 42.88)
jackson_tracts = sf::st_crop(mi_tracts, xmin = -84.55, xmax = -84.2, ymin = 42.15, ymax = 42.35)
battle_creek_tracts = sf::st_crop(mi_tracts, xmin = -85.8, xmax = -85, ymin = 42.1, ymax = 42.5)
grand_rapids_group_tracts = sf::st_crop(mi_tracts, xmin = -86.5, xmax = -85.4, ymin = 42.6, ymax = 43.35)
saginaw_group_tracts = sf::st_crop(mi_tracts, xmin = -84.5, xmax = -83.5, ymin = 43.2, ymax = 43.8)

pdf(file = 'michigan-tracts-maps.pdf', width = 12, height = 12)
ggplot() + 
  geom_sf(data = mi_tracts, aes(fill = log10(ALAND))) +
  geom_sf(data = detroit_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  geom_sf(data = flint_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  geom_sf(data = lansing_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  geom_sf(data = ann_arbor_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  geom_sf(data = jackson_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  geom_sf(data = battle_creek_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  geom_sf(data = grand_rapids_group_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  geom_sf(data = saginaw_group_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  theme_minimal() + theme(legend.position = "none")
  
ggplot() + 
  geom_sf(data = detroit_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
   theme_minimal() + theme(legend.position = "none") + 
   ggtitle("Detroit Area")
ggplot() + 
  geom_sf(data = flint_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
   theme_minimal() + theme(legend.position = "none") +
   ggtitle("Flint Area")
ggplot() + 
  geom_sf(data = ann_arbor_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
   theme_minimal() + theme(legend.position = "none") +
   ggtitle("Ann Arbor and Ypsilanti Area")
ggplot() + 
  geom_sf(data = lansing_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  theme_minimal() + theme(legend.position = "none") +
  ggtitle("Lansing Area")
ggplot() +
  geom_sf(data = jackson_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  theme_minimal() + theme(legend.position = "none") +
  ggtitle("Jackson Area")
ggplot() +  
  geom_sf(data = battle_creek_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  theme_minimal() + theme(legend.position = "none") +
  ggtitle("Kalamazoo and Battle Creek")
ggplot() +
  geom_sf(data = grand_rapids_group_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  theme_minimal() + theme(legend.position = "none") +
  ggtitle("Grand Rapids and coastal area")
ggplot() +
  geom_sf(data = saginaw_group_tracts, aes(fill = log10(ALAND)), color = 'blue', linewidth = 0.1) +
  theme_minimal() + theme(legend.position = "none") +
  ggtitle("Saginaw and coastal area")
dev.off()


