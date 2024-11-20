# Developing a remote sensing monitoring programme to evaluate the effectiveness of restoration measures

Optical TRApezoid Model (OPTRAM) is based on Shortwave Infrared and reveals temporal water table dynamics/soil moisture in northern peatlands (Burdun et al. 2023). Positive OPTRAM values indicate a higher water content and vice versa. OPTRAM performance is known to suffer from oversaturated pixels, e.g., pixels covered by standing water or wet vegetation (Sadeghi et al. 2017) which have high STR values (OPTRAM values > 1) and influence the wrong estimation of wet edge (Sadeghi et al. 2017). OPTRAM values > 1 are therefore removed. 

An area covered by vegetation strongly sensitive to water fluctuations (sedges, mosses, etc.) can be used as a "best pixel", and OPTRAM values over this area will reflect temporal changes in water table depth. However, OPTRAM values derived over tree-covered areas will not be informative. Given the absence of long-time series of in-situ water table depth in Polesia to select the "best pixels", we select pixels with < 5% tree cover using NASA's 30m resolution [Landsat Vegetation Continuous Fields](https://developers.google.com/earth-engine/datasets/catalog/NASA_MEASURES_GFCC_TC_v3) tree cover layers available in the Google Earth Engine (GEE) catalogue. 

The Sentinel satellite imagery was chosen as the basis to calculate OPTRAM indices for this project, due to the unique high spatial and temporal resolution (from 10 m to 60 m; ca. every 3 to 4 days) available for Europe (though we resampled the images to a 50m spatial resolution for the calculation of edge parameters due to the computational limits of GEE). The Sentinel-2 imagery consists of 13 bands which describe the measured reflected radiance of the Earth's surface. GEE provides online access to Sentinel-1 C band SAR and Senintel-2 level-2A data that is pre-processed using Sen2Cor. Level-2A processing comprises a scene classification (indicates presence of snow/clouds) and an atmospheric correction (providing bottom of the atmosphere reflectance products) (Obregón et al. 2019). 

The code provided here is adapted from Burdun et al. (2023) and can be used to monitor moisture content in peatlands over time. Because of the large size of our restoration areas, they were subdivided into smaller sections prior to uploading to GEE in QGIS, and the GEE code was run on each section. We used this code to monitor peatland moisture content between April to September 2016–2024. Because they dry edge estimation might suffer from the missing dry pixels with high NDVI values, we limited the range of NDVI values between 0.1 and 0.9 within which maximal (wet edge) and minimal (dry edge) STR values would be derived. We identified these ranges visually by examining NDVI–STR spaces for our study sites. We provide GEE code to extract NDVI-STR values and R code (plotting_NDVI_STR_space.R) to visualise these, to determine the most appropriate NDVI threshold for a given study area. 

The code provided is run using the **Somyne** restoration area as an example. 

## Analysis steps

1. Identify treeless areas (< 5% tree cover) to select “best pixels”
3. Extract OPTRAM parameters in GEE following the methodology of Burdun et al. (2023)
4. Calculate OPTRAM using OPTRAM_restoration_areas.R

### Links to GEE code

[Sample treeless pixels](https://code.earthengine.google.com/576710d96fc3929014cb0c099f5396bf)

[Wet edge](https://code.earthengine.google.com/58b4b7b1e53dfae855a1a4176d24616b)

[Dry edge](https://code.earthengine.google.com/bb3d09b8392c2f9719fa26e97be20700)

[NDVI and STR of treeless pixels](https://code.earthengine.google.com/f4652b1982d6aed00aa4c26c9ce010f0)

[NDVI-STR space](https://code.earthengine.google.com/1fd6aad91e494d8a700ee4b6fc821155)

## Example results 
![Rplot10](https://github.com/user-attachments/assets/8a5e374f-5ab9-4850-87d4-f0380345a6c9)

## References

Burdun, Iuliia, Michel Bechtold, Mika Aurela, Gabrielle De Lannoy, Ankur R. Desai, Elyn Humphreys, Santtu Kareksela, et al. 2023. ‘Hidden Becomes Clear: Optical Remote Sensing of Vegetation Reveals Water Table Dynamics in Northern Peatlands’. Remote Sensing of Environment 296 (October):113736. https://doi.org/10.1016/j.rse.2023.113736.

Obregón, María Ángeles, Gonçalo Rodrigues, Maria Joao Costa, Miguel Potes, and Ana Maria Silva. 2019. ‘Validation of ESA Sentinel-2 L2A Aerosol Optical Thickness and Columnar Water Vapour during 2017–2018’. Remote Sensing 11 (14): 1649. https://doi.org/10.3390/rs11141649.

Sadeghi, Morteza, Ebrahim Babaeian, Markus Tuller, and Scott B. Jones. 2017. ‘The Optical Trapezoid Model: A Novel Approach to Remote Sensing of Soil Moisture Applied to Sentinel-2 and Landsat-8 Observations’. Remote Sensing of Environment 198 (September):52–68.          https://doi.org/10.1016/j.rse.2017.05.041.
