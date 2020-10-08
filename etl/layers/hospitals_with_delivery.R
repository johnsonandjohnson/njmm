## Libraries
pacman::p_load(dplyr, rgdal)

# All NJ hospitals
hospitals <- readOGR("data/raw/nj_hospitals/Hospitals_of_New_Jersey.shp", verbose = F)

# List of NJ hospitals with delivery -- from https://nj.gov/health/maternal/morbidity/mhh_reportcard/all_hospitals.shtml
delivery_hosps <- c(
  "Atlanticare Regional Medical Center Mainland Division", "Cape Regional Medical Center",
  "Capital Health System - Hopewell", "Christ Hospital", "Hoboken University Medical Center",
  "CentraState Medical Center", "Chilton Medical Center", "Clara Maass Medical Center", "Community Medical Center",
  "Cooper Hospital/University Medical Center", "Englewood Hospital and Medical Center",
  "Hackensack UMC at Pascack Valley", "Hackensack University Medical Center", "Hackensack UMC Mountainside",
  "Holy Name Medical Center", "Meadowlands Hospital Medical Center", "Hunterdon Medical Center",
  "Inspira Medical Center Elmer", "Inspira Medical Center Vineland", "Inspira Medical Center Woodbury",
  "Kennedy Memorial Hospitals UMC Washington Township", "Jersey City Medical Center", "Jersey Shore University Medical Center",
  "JFK Medical Center", "Monmouth Medical Center", "Morristown Medical Center", "Newark Beth Israel Medical Center",
  "Newton Medical Center", "Ocean Medical Center", "Our Lady of Lourdes Medical Center", "Overlook Medical Center",
  "Palisades Medical Center", "University Medical Center of Princeton at Plainsboro",
  "Raritan Bay Medical Center - Perth Amboy", "Riverview Medical Center", "Robert Wood Johnson University Hospital",
  "Robert Wood Johnson University Hospital Somerset", "Saint Barnabas Medical Center", "Saint Clare's Hospital/Denville",
  "Saint Peter's University Hospital", "Shore Medical Center", "Southern Ocean Medical Center",
  "St. Joseph's Regional Medical Center", "St. Mary's Hospital", "Trinitas Regional Medical Center",
  "University Hospital", "Valley Hospital", "Virtua Memorial Hospital of Burlington County",
  "Virtua West Jersey Hospital - Voorhees"
)

birth_hosps <- hospitals@data %>% filter(NAME %in% delivery_hosps)

saveRDS(birth_hosps, "data/processed/hospitals_with_delivery.rds")
