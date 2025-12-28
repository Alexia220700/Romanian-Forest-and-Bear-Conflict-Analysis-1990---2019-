# Use R with tidyverse pre-installed
FROM rocker/tidyverse:4.3.0

# Install additional packages
RUN R -e "install.packages(c('maps', 'mapdata'), dependencies = TRUE)"

# Set working directory
WORKDIR /app

# Copy ALL files from current directory
COPY . .

# Run your script
CMD ["Rscript", "script.R"]
