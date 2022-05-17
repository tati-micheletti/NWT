# Extracting Results

########## TO LOAD FILES ###############

birdsFireCaribouV1 <- readRDS(file = file.path(getwd(), "outputs/birdsFireCaribouV1_05MAR19.rds"))
birdsFireCaribouV2 <- readRDS(file = file.path(getwd(), "outputs/birdsFireCaribouV2_11MAR19.rds"))
birdsFireCaribouV3 <- readRDS(file = file.path(getwd(), "outputs/19MAR19/birdsFireCaribouV3_19MAR19.rds"))

#########################################

# ~~~~~~~~~~~~~~~~~~~~ BIRDS
createBirdsGIFFromList(species = c("BBWA", "BOCH", "RBNU"),
                       dataPath = "/mnt/data/Micheletti/NWT/modules/birdsNWT/data/predicted", 
                       version = "V3")

createBirdsGIF(simList = birdsFireCaribouV2)

# ~~~~~~~~~~~~~~~~~~~~ BIOMASS
# TOTAL BIOMASS
biomassYearGIF(dataPath = "/mnt/data/Micheletti/NWT/outputs/19MAR19")

# BIOMASS PER SPECIES
biomassPerSpeciesYearGIF(dataPath = "/mnt/data/Micheletti/NWT/outputs/19MAR19")

# ~~~~~~~~~~~~~~~~~~~~ FIRE
# CUMMULATIVE FIRE -- Still to make...
# biomassYearGIF(dataPath = "/mnt/data/Micheletti/NWT/outputs/19MAR19")

# ~~~~~~~~~~~~~~~~~~~~ CARIBOU
# POPULATION GROWTH -- Still to make...
# biomassYearGIF(dataPath = "/mnt/data/Micheletti/NWT/outputs/19MAR19")

# RSF -- Still to make...
# biomassYearGIF(dataPath = "/mnt/data/Micheletti/NWT/outputs/19MAR19")
