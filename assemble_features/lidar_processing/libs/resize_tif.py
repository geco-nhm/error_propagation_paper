#%%
import json
import logging
import rasterio
import geopandas as gpd
from rasterio.mask import mask
from shapely.geometry import mapping, box
from rasterio.crs import CRS
#%%
def resize_tiff(ground_tif, canopy_tif, canopy_tif_masked):
    logging.info('Mask canopy tif if out of bounds from ground')
    # Open the two images
    with rasterio.open(ground_tif) as ground:
        ground_image = ground.read(1)

    with rasterio.open(canopy_tif) as canopy:
        canopy_image = canopy.read(1)

    # Get the ground image dimensions
    height = ground_image.shape[0]
    width = ground_image.shape[1]

    bounds = ground.bounds

    bbox = box(bounds.left, bounds.bottom, bounds.right, bounds.top)
    geo = gpd.GeoDataFrame({'geometry': bbox}, index=[0], crs=ground.crs.data)

    # Project the Polygon into another CRS
    geo = geo.to_crs(crs=canopy.crs.data)

    def get_features(gdf):
        return [json.loads(gdf.to_json())['features'][0]['geometry']]

    with rasterio.open(canopy_tif) as src:
        out_image, out_transform = mask(src, get_features(geo), crop=True)
        out_meta = src.meta.copy()

    # Save the masked image
    out_meta.update({
            "driver": "GTiff",
            "height": out_image.shape[1],
            "width": out_image.shape[2]
            # "transform": out_transform,
            # "crs": CRS.from_epsg(4326)
        })

    with rasterio.open(canopy_tif_masked, "w", **out_meta) as dest:
        dest.write(out_image)

    return canopy_tif_masked


# %%
