#%%
import os
from pathlib import Path
import logging
import shutil
from config import TIFF_RESOLUTIONS
from libs.classify_point_cloud import classify_point_cloud
from libs.compress_las_to_laz import compress_las_to_laz
from libs.extract_canopy import extract_canopy
from libs.extract_ground import extract_ground
from libs.extract_veg_height import extract_veg_height
from libs.merge_las_files import merge_las_files
from libs.merge_laz import merge_laz
from libs.split_laz_file import split_laz_file

from libs.misc import check_output_dir_empty
from libs.tiff_postprocessing import fill_nodata_tiff

#%%
# Set logger to output all messages
logging.getLogger().setLevel(logging.DEBUG)
logging.basicConfig(format='%(asctime)s %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p')
# define paths
os.environ['PROJ_LIB'] = '/opt/conda/share/proj'
input_dir = Path('/input')  #input_dir = Path('/src/input')   
output_dir = Path('/output')    #output_dir = Path('/src/output') 
input_files = list(input_dir.glob('*.las'))
tmp_dir = Path('/dev/shm/lidar-part') #tmp_dir = Path('/src/tmp')
tmp_dir.mkdir(exist_ok=True)
laz_file = Path('/src/output/01-compressed.laz')
tmp_unclassified = tmp_dir / 'unclassified' 
tmp_file_template = tmp_unclassified / 'part_#.laz'
ground_tif_template = Path('/src/output/02-ground.tif')
ground_tif_fill = Path("/src/output/02-ground_fill.tif")
canopy_tif_template = Path('/src/output/03-canopy.tif')
veg_height_tif_template = Path('/src/output/04-veg_height.tif')

#%% Main workflow
if __name__ == '__main__':
    #check_output_dir_empty(output_dir)
    las_file = merge_las_files(input_files)

    if not laz_file.exists():
        compress_las_to_laz(las_file, laz_file)
    else:
        logging.info('Compressed already exists, skipped')
    
    tmp_unclassified.mkdir(exist_ok=True)
    classified_laz = Path('/src/output/02-classified_points.laz')
    if not classified_laz.exists():
        split_laz_file(laz_file.as_posix(), tmp_file_template)
        part_files = list(tmp_unclassified.glob('*.laz'))
        classified_parts_dir = tmp_dir / 'classified'
        classified_parts_dir.mkdir(exist_ok=True)
        
        classify_point_cloud(part_files, classified_parts_dir)
        merge_laz(classified_parts_dir, classified_laz.as_posix())
    else:
        logging.info('Classified already exists, skipped')

    
    for resolution in TIFF_RESOLUTIONS:
        ground_tif = ground_tif_template.with_stem(f'{ground_tif_template.stem}_{resolution}')
        if ground_tif.exists():
            continue
        #extract ground and fill in the gaps of no-data pixels
        logging.info('extract ground and fill in the gaps')
        extract_ground(classified_laz.as_posix(), ground_tif, resolution)
        filled_tif = ground_tif_fill.with_stem(f'{ground_tif_fill.stem}_{resolution}')
        fill_nodata_tiff(ground_tif.as_posix(), filled_tif, window_size=100)

        #extract canopy and mask it to always overlap with ground
        logging.info('extract canopy for')
        canopy_tif = canopy_tif_template.with_stem(f'{canopy_tif_template.stem}_{resolution}')
        extract_canopy(classified_laz.as_posix(), canopy_tif, resolution)
        
        # extract vegetation height from laz and fill no-data pixels
        logging.info('extract vegetation height for')
        veg_height_tif = veg_height_tif_template.with_stem(f'{veg_height_tif_template.stem}_{resolution}')
        extract_veg_height(classified_laz.as_posix(), veg_height_tif, resolution)
        filled_veg_tif = veg_height_tif.with_stem(f'{veg_height_tif.stem}_fill')
        fill_nodata_tiff(veg_height_tif.as_posix(), filled_veg_tif, window_size=100)


    else:
        logging.info('All resolutions exist; finished')
    shutil.rmtree(tmp_dir)

# %%
