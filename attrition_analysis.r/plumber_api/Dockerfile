FROM rocker/r-ver:4.5.1

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN Rscript -e "install.packages(c('plumber','dplyr','caret','jsonlite','Matrix'), repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /app

# Copy all app files
COPY api.R               ./api.R
COPY plumber.R           ./plumber.R
COPY model_final.rds     ./model_final.rds
COPY scaling_params.rds  ./scaling_params.rds
COPY insights_report.json    ./insights_report.json
COPY feature_importance.json ./feature_importance.json
COPY shap_metadata.json      ./shap_metadata.json

# Render provides PORT env var — expose it
EXPOSE 8000

# Set working dir so plumber.R sources api.R correctly
ENV API_DIR=/app

# Start via plumber.R (includes CORS filter) on the port Render assigns
CMD Rscript -e "setwd('/app'); pr <- source('plumber.R')\$value; pr\$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '8000')))"
