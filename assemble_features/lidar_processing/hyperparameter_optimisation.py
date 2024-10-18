#%% 
import json
import itertools
import subprocess
#%%
def get_params(cell=1.0, slope=0.2, window=16, threshold=0.45, scalar=1.2, name='name'):
    return {
        "pipeline":[
            "/testing/tolga_test-1_1.laz",
            {
                "type":"filters.reprojection",
                "in_srs":"EPSG:4326",
                "out_srs":"EPSG:4326"
            },
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
            "cell": cell, 
            "slope": slope,
            "window": window,
            "threshold": threshold,
            "scalar": scalar
            },
            {
            "type":"filters.range",
            "limits":"Classification[2:2]"
            },
            {
            "type": "writers.gdal",
            "filename":f"/testing/{name}.tif",
            "output_type":"idw",
            "gdaldriver":"GTiff",
            "resolution": 0.5,
            "radius": 1
            }
        ]
    }
#%%
cells = [0.5, 1.0, 5.0]
slope = [0.05, 0.1, 0.3]
window = [6.0, 18.0, 30.0]
threshold = [0.1, 0.5, 0.7]
scalar = [0.5, 1.0, 2.0]

#%%
combinations = list(itertools.product(cells, slope, window, threshold, scalar))
# %%
for cells, slope, window, threshold, scalar in combinations:
    print(cells, slope, window, threshold, scalar)
    name = f'{cells}_{slope}_{window}_{threshold}_{scalar}'
    jsn = get_params(cells, slope, window, threshold, scalar, name)
    json_text = json.dumps(jsn)
    json_file = f'/testing/{name}.json'
    with open(json_file, 'w') as f:
        f.write(json_text)
        f.close()
    subprocess.call(f'pdal pipeline -i {json_file}', shell=True)
    
print('hotovo do boha!!')

# %%
