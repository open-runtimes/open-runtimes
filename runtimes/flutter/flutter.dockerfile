RUN dart --disable-analytics

# Install Flutter dependencies
RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    git \
    xz-utils \
    && apt-get clean
