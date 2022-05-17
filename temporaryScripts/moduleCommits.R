
# Stuck at   Dispersal for pixels 11255148 to 11364899 and crashed... > 5hs 
# Modules and Submodules commits used in version 3 (whole NWT)
# 
# RESULTS:
# NWT no climate change: 08JUN19_CS
# NWT climate change: 08JUN19
# 
# Commits on 03rd June - Whole NWT run
# Assumptions: 
# FIRE: fireSense runs a "climate sensitive" fire from 2001 to 2011 using the 2011 climate layer
# LANDR: Assumption at a landscape level is ok. We are assuming that you (we) are essentially making 
# the assumption that 2011 looked like 2001. At the _landscape_ level that's not a terrible assumption
# Oldest PSP stand is way younger than the data we have
# FOR THIS RUN: LandR package - script `maps.R`, function `vegTypeMapGenerator`'s unit test was not run as it was erroring. 
# I will check it ASAP

# NWT project: 96f1a017e289077a2a5a3fb30f0a3bfe1a6b544f (heads/master) + next commit
# LandR: 4f55d34707769e706a410cfa93435ff933fd8157 + commenting out maps/line 229 on.

# 35335d977c5e432fbaa7301bdcc8112b5d4dfc80 modules/Biomass_regeneration (heads/development)
# 019d688e1858792d3eddf3e9d1551a117d55535c modules/Boreal_LBMRDataPrep (heads/development)
# 84886af3a99fd341c7461158c1ae8c25a8f0c099 modules/LBMR (heads/development)
# cf3af3428ee24cc5ca665ce7a1190118593d2d0d modules/LBMR2LCC_DataPrep (heads/master)
# 0d674d3a264e59148aa03fa4f3d004a007d115d2 modules/LandR_BiomassGMOrig (heads/development)
# 881957951841d667734556b84e39789622518343 modules/MDC_NWT_DataPrep (heads/master)
# 6e140843c8b307b69128289b8e377072b9265676 modules/PSP_Clean (heads/master)
# c120d8e1b106cd8d31e54f76bc07e8afbbf23309 modules/birdsNWT (heads/master)
# 1d617235d129b410ece776c07e18972e1bfd9bd0 modules/caribouPopGrowthModel (heads/master)
# 7797b61b6291400fe3c709744ea367a33c072c3b modules/caribouRSF (heads/master)
# 61cc11a15502b7ee7199faead74b39d5e2801cb7 modules/climate_NWT_DataPrep (heads/master)
# dba12450d340ef2a277cac00b633967745db0f80 modules/comm_metricsNWT (heads/master)
# d976b680f99d7a029dc216984c5fa96c96df42c4 modules/fireSense_EscapePredict (heads/master)
# 78ddd05efd17d9e9931201ee9449ba8dc99f18e7 modules/fireSense_FrequencyPredict (heads/master)
# dfc61faa8272e9c06c2f4ebeeada58d298c874cc modules/fireSense_NWT (heads/master)
# 3beeb13eb18c2e7f1961bdc9dca29b9253024691 modules/fireSense_NWT_DataPrep (heads/master)
# 656e182a94422ee6157ae64ca3c0e006811b4b16 modules/gmcsDataPrep (heads/development)
# 38557980e8675ce39aecafbd6fc6886a7a0cd57b modules/scfm (heads/development)

# Commits of key packages: updated to changes made until the 30th at 11:30am
# 
# THIS IS WHAT WAS WORKING BEFORE PULLING LBMR BorealDataPrep, reproducible & LandR:
#  35335d977c5e432fbaa7301bdcc8112b5d4dfc80 modules/Biomass_regeneration (heads/development)
# 019d688e1858792d3eddf3e9d1551a117d55535c modules/Boreal_LBMRDataPrep (heads/development)
# 84886af3a99fd341c7461158c1ae8c25a8f0c099 modules/LBMR (heads/development)
# cf3af3428ee24cc5ca665ce7a1190118593d2d0d modules/LBMR2LCC_DataPrep (heads/master)
# 0d674d3a264e59148aa03fa4f3d004a007d115d2 modules/LandR_BiomassGMOrig (heads/development)
# 881957951841d667734556b84e39789622518343 modules/MDC_NWT_DataPrep (heads/master)
# 6e140843c8b307b69128289b8e377072b9265676 modules/PSP_Clean (heads/master)
# c120d8e1b106cd8d31e54f76bc07e8afbbf23309 modules/birdsNWT (heads/master)
# +45a513724844c025e2abc9f2bee2983a87afd7a0 modules/caribouPopGrowthModel (heads/master)
# 7797b61b6291400fe3c709744ea367a33c072c3b modules/caribouRSF (heads/master)
# 9db261490ee982ed25dcf9b4af59ab43482b2e6d modules/climate_NWT_DataPrep (heads/master)
# dba12450d340ef2a277cac00b633967745db0f80 modules/comm_metricsNWT (heads/master)
# d976b680f99d7a029dc216984c5fa96c96df42c4 modules/fireSense_EscapePredict (heads/master)
# 78ddd05efd17d9e9931201ee9449ba8dc99f18e7 modules/fireSense_FrequencyPredict (heads/master)
# dfc61faa8272e9c06c2f4ebeeada58d298c874cc modules/fireSense_NWT (heads/master)
# 3beeb13eb18c2e7f1961bdc9dca29b9253024691 modules/fireSense_NWT_DataPrep (heads/master)
# 656e182a94422ee6157ae64ca3c0e006811b4b16 modules/gmcsDataPrep (heads/development)
# 38557980e8675ce39aecafbd6fc6886a7a0cd57b modules/scfm (heads/development)
# 
# THIS IS WHAT I USED TO RUN fS + CS + increased seed dispersal on 27th JUNE 19
# 
# 9ed7adae7cb00efb7fb2ed226048872e1db15bcc modules/Biomass_regeneration (heads/development)
# 8e7f3bbf3aaafc7bd6d477d4050850e4df3ca424 modules/Boreal_LBMRDataPrep (heads/development)
# f55bf3d34a7754dd9c68029a7e9fc739b150760f modules/LBMR (heads/development)
# cf3af3428ee24cc5ca665ce7a1190118593d2d0d modules/LBMR2LCC_DataPrep (heads/master)
# 0d674d3a264e59148aa03fa4f3d004a007d115d2 modules/LandR_BiomassGMOrig (heads/development)
# 881957951841d667734556b84e39789622518343 modules/MDC_NWT_DataPrep (heads/master)
# 6e140843c8b307b69128289b8e377072b9265676 modules/PSP_Clean (heads/master)
# bda790931b74670f9ec06ea4128bd4ca0f2787bd modules/birdsNWT (heads/master)
# 47f31bbc7a0a97479f4ea82d2750ced91c864381 modules/caribouPopGrowthModel (heads/master)
# 4e973ab33a1ab8da6cf395b95f3ce69e2deb580c modules/caribouRSF (heads/master)
# 682f998265da4b7b6c52a3bb9c955f3fbc8136e2 modules/climate_NWT_DataPrep (heads/master)
# 6c67a16e8562d938d0ac036bff3bd60f2a8cb630 modules/comm_metricsNWT (heads/master)
# 6dc80116b3a9746bae03496367b8490a46bb55f6 modules/fireSense_EscapePredict (heads/master)
# 3571e94c1ef3f361a2f33acbb9daf63282f5fe7d modules/fireSense_FrequencyPredict (heads/master)
# dfc61faa8272e9c06c2f4ebeeada58d298c874cc modules/fireSense_NWT (heads/master)
# 3beeb13eb18c2e7f1961bdc9dca29b9253024691 modules/fireSense_NWT_DataPrep (heads/master)
# 656e182a94422ee6157ae64ca3c0e006811b4b16 modules/gmcsDataPrep (heads/development)
# 38557980e8675ce39aecafbd6fc6886a7a0cd57b modules/scfm (heads/development)
# 
#  VERSION THAT MIGHT BE BROKEN? AFTER TRYING PLAYING WITH SEED DISPERSAL
# 9ed7adae7cb00efb7fb2ed226048872e1db15bcc modules/Biomass_regeneration (heads/development)
# 0db1c85d34c1a82044eff7c668bb15a43cd102c4 modules/Boreal_LBMRDataPrep (heads/development)
# b0d58e77e9852889fffedb06a2262371efadfc8e modules/LBMR (heads/development)
# cf3af3428ee24cc5ca665ce7a1190118593d2d0d modules/LBMR2LCC_DataPrep (heads/master)
# 0d674d3a264e59148aa03fa4f3d004a007d115d2 modules/LandR_BiomassGMOrig (heads/development)
# 881957951841d667734556b84e39789622518343 modules/MDC_NWT_DataPrep (heads/master)
# 6e140843c8b307b69128289b8e377072b9265676 modules/PSP_Clean (heads/master)
# bda790931b74670f9ec06ea4128bd4ca0f2787bd modules/birdsNWT (heads/master)
# 890e45a1fea94bcec2261c86ed18b5a1b9e7a411 modules/caribouPopGrowthModel (heads/master)
# 4e973ab33a1ab8da6cf395b95f3ce69e2deb580c modules/caribouRSF (heads/master)
# 83b214a7ddd106320c019f41a467d869330e2a45 modules/climate_NWT_DataPrep (heads/master)
# 6c67a16e8562d938d0ac036bff3bd60f2a8cb630 modules/comm_metricsNWT (heads/master)
# 6dc80116b3a9746bae03496367b8490a46bb55f6 modules/fireSense_EscapePredict (heads/master)
# b51da3830f014fae481e53e257c6adcdba5d351e modules/fireSense_FrequencyPredict (heads/master)
# dfc61faa8272e9c06c2f4ebeeada58d298c874cc modules/fireSense_NWT (heads/master)
# 6eb4d028be71dc2de7d2ec4c7c37605d87f75c7b modules/fireSense_NWT_DataPrep (heads/master)
# 8e2ac7c7fdf8d0e73123bcd2b776afb77f4db5f2 modules/gmcsDataPrep (heads/development)
# c511df63afb5483cd633c4706edf6ac52fe13650 modules/scfm (heads/development)