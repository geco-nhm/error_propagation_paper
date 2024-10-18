import logging
from libs.run_pdal_pipeline import run_pdal_pipeline


def merge_laz(laz_dir, output):
    """
    Merges ground points from laz_dir and writes the output to output.
    """
    logging.info('Merging')
    merge_pipeline = [x.as_posix() for x in laz_dir.glob('*.laz')]
    merge_pipeline.extend([
        {'type': 'filters.merge'},
        {
            'type': 'writers.las',
            'filename': output
        }
    ])
    run_pdal_pipeline(merge_pipeline)
