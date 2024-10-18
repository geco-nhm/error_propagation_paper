import json
import logging
from pathlib import Path
import pdal


def merge_las_files(input_files):
    """
    Merges multiple LAS files into a single LAS file.
    Returns the merged LAS file path.
    """
    if len(input_files) > 1:
        logging.info('Multiple .las files found, Attempting merger')
        merge_pipeline_json = [x.as_posix() for x in input_files]
        merge_pipeline_json.append({'type': 'filters.merge'})
        merge_pipeline_json.append('/src/output/00-processed-input.las')
        merge_pipeline_json = json.dumps(merge_pipeline_json)
        merge_pipeline = pdal.Pipeline(merge_pipeline_json)
        merge_pipeline.execute()
        las_file = Path('/src/output/00-processed-input.las')
        logging.info('Successfully merged.')
        return las_file
    return input_files[0]
