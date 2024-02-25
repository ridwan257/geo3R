# Use the official R base image
FROM r-base:latest

# Install necessary libraries
# RUN apt-get update && apt-get install -y \
#    r-cran-devtools \
#    r-cran-plumber \
#    && rm -rf /var/lib/apt/lists/*

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy the R script containing your API code into the container
COPY . .

# Expose the port your API will run on
EXPOSE 10000

# Command to run the API
CMD ["Rscript", "run.R"]
