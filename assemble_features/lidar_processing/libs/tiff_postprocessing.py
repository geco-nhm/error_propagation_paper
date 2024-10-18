
import logging
import subprocess


def fill_nodata_tiff(input, output, window_size=100, smoothing_iterations=2):
    cmd = ['gdal_fillnodata.py', '-md', str(window_size), '-si', str(smoothing_iterations), input, output ]
    logging.info('Filling no data')
    subprocess.run(cmd, check=True)


def canopy_height(inputA, inputB, output):
    # based on oneliner gdal_calc.py -A /src/output/03-ground_points_fill.tif -B /src/output/04-canopy_points.tif --calc="B-A" --outfile chm.tif
    logging.info('calculating canopy height')
    cmd = ['gdal_calc.py', '-A', inputA, '-B', inputB, '--calc="B-A"', '--outfile', output]
    subprocess.run(cmd, check=True)

    
def resample_tiff(input_tiff, output_tiff, raster_resolution):
        #    gdalwarp -overwrite -s_srs EPSG:4326 -t_srs EPSG:25832 -tr 0.01 0.01 -r near -multi -of GTiff D:/Drone_DATA/2023_Landsvik/PROCESSED/DJI_Zenmuse_L1/2023_05_06_Landsvik_RGB_light_uni/result.tif C:/Users/peterhor/AppData/Local/Temp/processing_iEeKkf/732bc2e4bc924cfdb467e1cd860a3013/OUTPUT.tif
    cmd = ['gdalwarp.py', '-overwrite', '-s_srs', 'EPSG:4326', '-t_srs', 'EPSG:25832', '-tr', raster_resolution, raster_resolution, '-r', 'near', '-multi', '-of', 'GTiff', input_tiff, output_tiff ]
    logging.info('Downscaling RGB raster image')
    subprocess.run(cmd, check=True)
           
