# Use an official Python runtime as a parent image
FROM continuumio/miniconda3

# Set the working directory in the container to /app
WORKDIR /app

# Add current directory contents to the container at /app
ADD . /app

# Update the system
RUN apt-get update -y && apt-get upgrade -y

# install libraries in requrements file
RUN conda install -c conda-forge --file requirements.txt
