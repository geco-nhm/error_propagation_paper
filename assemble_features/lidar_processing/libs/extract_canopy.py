import logging
from pathlib import Path
from libs.run_pdal_pipeline import run_pdal_pipeline



def extract_canopy(laz_file, out_tiff:Path, resolution):
    """
    Selects only ground points in the given LAZ and writes the output as a tiff.
    """
    logging.info(f'Extract canopy for resolution {resolution}')
    
    pipeline = [
        {
            "type": "readers.las",
            "filename": laz_file
        },
        {
        "type": "filters.range",
        "limits": "returnnumber[1:1]" 
        },
        {
            "type": "writers.gdal",
            "filename": out_tiff.as_posix(),
            "output_type":"mean",
            "gdaldriver":"GTiff",
            "resolution": str(resolution),
            "radius": 0.1
        }
    ]

    run_pdal_pipeline(pipeline)
        
    logging.info('canopy extracted')
    return out_tiff