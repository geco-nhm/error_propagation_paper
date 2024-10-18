import logging
from config import TILE_BUFFER, TILE_LENGHT
from libs.run_pdal_pipeline import run_pdal_pipeline


def split_laz_file(laz_file, tmp_file_template):
    """
    Splits a LAZ file into smaller parts.
    """
    logging.info('Splitting into parts')
    split_pipeline_dict = [
        {
            "type": "readers.las",
            "filename": laz_file
        },
        {
            "type":"filters.splitter",
            "length":TILE_LENGHT,
            "buffer":TILE_BUFFER
        },
        {
          'type': 'writers.las',
          'filename': tmp_file_template.as_posix()
        }
    ]
    run_pdal_pipeline(split_pipeline_dict)
    logging.info('Splitting successful')