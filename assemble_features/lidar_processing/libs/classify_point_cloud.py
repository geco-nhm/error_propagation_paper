from concurrent.futures import ProcessPoolExecutor
import logging
import multiprocessing

from libs.run_pdal_pipeline import run_pdal_pipeline


def classify_point_cloud(part_files, laz_dir):
    """
    Classifies ground points in the given part_files and writes the output to laz_dir.
    """
    logging.info('Classify ground and canopy in point cloud')
    laz_dir.mkdir(exist_ok=True)

    def get_pipeline(file):
        out_f = laz_dir / f'{file.stem}.laz'
        return [
            {
              "type": "readers.las",
              "filename": file.as_posix()
            },
            # {
            #   "type":"filters.reprojection",
            #   "out_srs":"EPSG:32632"
            # },
            {
              "type":"filters.assign",
              "assignment":"Classification[:]=0"
            },
            {
              "type":"filters.elm"
            },
            {
              "type":"filters.outlier"
            },
            {
              "type":"filters.smrf",
              "ignore":"Classification[7:7]",
              "slope":0.2,
              "window":10,
              "threshold":0.45,
              "scalar":1.2, 
              "cell":0.5
            },
            {
            "type": "filters.hag_nn"
            },
            
            {
            "type": "filters.assign",
            "assignment": "Classification[0:1] = 5"
            },
            {
              'type': 'writers.las',
              'filename': out_f.as_posix(),
              'extra_dims': 'HeightAboveGround=float32'
            }
        ]

    pipeline_defs = [get_pipeline(x) for x in part_files]
    logging.info(f'Start parrallel execution with {multiprocessing.cpu_count()-2} workers')
    with ProcessPoolExecutor(max_workers=multiprocessing.cpu_count()-2) as executor:
        results = list(executor.map(run_pdal_pipeline, pipeline_defs))

    logging.info('Ground and canopy classified')
