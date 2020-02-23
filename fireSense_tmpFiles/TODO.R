# Steps for fireSense
# 0. Prepare the formula and layers (the same for all three components): need for SpreadFit and will be useful later for the WB project
# CHECK: LBMR2LCC_DataPrep module. It is using the following (should not use all species!):
# Running prepInputs for Abie_Bal, Betu_Pap, Lari_Lar, Pice_Gla, Pice_Mar, Pinu_Ban, Pinu_Con, Pinu_Con, Popu_Bal, Popu_Tre
# RCP 4.5
# ==> This should be a module, and named `fireSense_DataPrep`
# 1. Try to run IgnitionFitted + EscapeFitted (objects from Fit modules) with SpreadFit
# 2. Try to run 1 + predict of all
# 3. Try to run IgnitionFit, EscapeFit, SpreadFit + predicts
# 4. Try to run 3 + SizeFit + SizePredict 