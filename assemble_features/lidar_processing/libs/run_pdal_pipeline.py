import json
import os
import pdal


def run_pdal_pipeline(pipeline_def):
    """
    Executes a PDAL pipeline with the given pipeline definition.
    """
    os.environ['PROJ_LIB'] = '/opt/conda/share/proj'
    pipeline = pdal.Pipeline(json.dumps(pipeline_def))
    return pipeline.execute()
