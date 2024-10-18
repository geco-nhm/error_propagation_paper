import logging


def check_output_dir_empty(output_dir):
    """
    Checks if the output directory is empty.
    """
    if any(output_dir.iterdir()):
        logging.error('Output dir is not empty')
        exit()
    logging.info('Output dir is empty, proceeding...')
