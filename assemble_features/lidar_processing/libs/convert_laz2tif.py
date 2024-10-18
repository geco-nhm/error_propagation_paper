import logging
from libs.run_pdal_pipeline import run_pdal_pipeline
from config import TIFF_RESOLUTION


def convert_laz2tif(input, output):
    """
    Converts a LAZ file to a TIFF file.
    """
    logging.info('Converting LAZ to TIFF')
    pipeline = [
        {
            "type": "readers.las",
            "filename": input
        },
        {
            "type": "writers.gdal",
            "filename": output,
            "output_type":"mean",
            "gdaldriver":"GTiff",
            "resolution": TIFF_RESOLUTION,
            "radius": 1
        }
    ]
    run_pdal_pipeline(pipeline)