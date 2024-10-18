import logging
from pathlib import Path
from libs.run_pdal_pipeline import run_pdal_pipeline


def compress_las_to_laz(las_file:Path, laz_file:Path):
    """
    Compresses a LAS file to a LAZ file.
    """
    logging.info('Compressing')
    compress_pipeline_dict = [
        {
            'type': 'readers.las',
            'filename': las_file.as_posix()
        },
        {
            'type': 'writers.las',
            'compression': 'laszip',
            'filename': laz_file.as_posix()
        }
    ]
    run_pdal_pipeline(compress_pipeline_dict)
    logging.info('Compression successful')
