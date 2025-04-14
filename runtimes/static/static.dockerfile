RUN apk update && apk add bash apache2-utils

# Directory for configurable fallback files (like 404)
RUN mkdir -p /mnt/resources