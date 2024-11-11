# Developing a remote sensing monitoring programme to evaluate effect of restoration measures

Optical TRApezoid Model (OPTRAM) is based on Shortwave Infrared and reveals temporal water table dynamics/soil moisture in northern peatlands (Burdun et al. 2023). It does not directly observe the position of the water table, but indicates vegetation moisture content (Kalacska et al. 2018). An area covered by vegetation strongly sensitive to water fluctuations (sedges, mosses, etc.) can be used as a "best pixel", and OPTRAM values over this area will reflect temporal changes in water table depth. However, OPTRAM values derived over tree-covered areas will not be informative. OPTRAM performance is known to suffer from oversaturated pixels, e.g., pixels covered by standing water or wet vegetation (Sadeghi et al. 2017), which have high STR values (OPTRAM values > 1) and influence the wrong estimation of wet edge (Sadeghi et al. 2017). Positive OPTRAM values indicate a higher water content and vice versa. OPTRAM ranges from 0-1. 

The Sentinel satellite imagery was chosen as the basis to calculate remote sensing derived indices for this project due to the unique high spatial and temporal resolution (from 10 m to 60 m; ca. every 3 to 4 days) available for Europe. The Sentinel-2 imagery consists of 13 bands which describe the measured reflected radiance of the Earth's surface. Google Earth Engine (GEE) provides online access to Sentinel-1 C band SAR and Senintel-2 level-2A data that is pre-processed using Sen2Cor. Level-2A processing comprises a scene classification (indicates presence of snow/clouds) and an atmospheric correction (providing bottom of the atmosphere reflectance products) (Obregón et al. 2019). 

The code provided here can be used to monitor moisture content in peatlands over time. 

## Analysis steps: 

1. Identify treeless areas (<10% tree cover) to select “best pixels” (Figure 1), in the absence of  long-time series of in-situ water table depth
2. Sample area of interest using https://code.earthengine.google.com/3006482d8a5c8a37a92b11af089e7f02 
3. Extract OPTRAM parameters in GEE following methodology of Burdun et al. (2023), pre-processing Sentinel-2 images to remove cloud, snow, and shadows 
    - Wet edge https://code.earthengine.google.com/6393cf65a60f823d3e7a799f6c8623b9 
    - Dry edge https://code.earthengine.google.com/11025012e8fb646da96bd37a40ab2d11?noload=true 
    - NDVI and STR https://code.earthengine.google.com/0f6f99fb6a857d36d675bbe19cd3ae08 
4. Calculate OPTRAM using OPTRAM_restoration_areas.R

## References:

Burdun, Iuliia, Michel Bechtold, Mika Aurela, Gabrielle De Lannoy, Ankur R. Desai, Elyn Humphreys, Santtu Kareksela, et al. 2023. ‘Hidden Becomes Clear: Optical Remote Sensing of Vegetation Reveals Water Table Dynamics in Northern Peatlands’. Remote Sensing of Environment 296 (October):113736. https://doi.org/10.1016/j.rse.2023.113736.

Obregón, María Ángeles, Gonçalo Rodrigues, Maria Joao Costa, Miguel Potes, and Ana Maria Silva. 2019. ‘Validation of ESA Sentinel-2 L2A Aerosol Optical Thickness and Columnar Water Vapour during 2017–2018’. Remote Sensing 11 (14): 1649. https://doi.org/10.3390/rs11141649.

Sadeghi, Morteza, Ebrahim Babaeian, Markus Tuller, and Scott B. Jones. 2017. ‘The Optical Trapezoid Model: A Novel Approach to Remote Sensing of Soil Moisture Applied to Sentinel-2 and Landsat-8 Observations’. Remote Sensing of Environment 198 (September):52–68.          https://doi.org/10.1016/j.rse.2017.05.041.
