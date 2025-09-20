# API Keys Configuration Template
# Copy this file to 'api_keys.R' and add your actual API keys
# DO NOT commit api_keys.R to version control!

# =================================================================================================== #
# GEOCODING API KEYS (OPTIONAL BUT RECOMMENDED FOR HIGHER LIMITS)
# =================================================================================================== #

# U.S. Census Geocoding API (no key required, but rate limited)
# https://geocoding.geo.census.gov/geocoder/

# Google Maps API (requires key, high accuracy, costs money after free tier)
# Get key from: https://developers.google.com/maps/documentation/geocoding/get-api-key
GOOGLE_MAPS_API_KEY <- "your_google_maps_api_key_here"

# OpenCage Geocoding API (requires key, generous free tier)
# Get key from: https://opencagedata.com/api
OPENCAGE_API_KEY <- "your_opencage_api_key_here"

# MapBox Geocoding API (requires key, good free tier)
# Get key from: https://www.mapbox.com/
MAPBOX_API_KEY <- "your_mapbox_api_key_here"

# Bing Maps API (requires key)
# Get key from: https://www.bingmapsportal.com/
BING_MAPS_API_KEY <- "your_bing_maps_api_key_here"

# =================================================================================================== #
# USAGE INSTRUCTIONS
# =================================================================================================== #

# 1. Copy this file to 'api_keys.R' in the same directory
# 2. Replace the placeholder values with your actual API keys  
# 3. Add 'api_keys.R' to your .gitignore file to avoid committing secrets
# 4. The geocoding functions will automatically use these keys if available

# For the HDS project, the free Census API should be sufficient for most needs
# But having backup APIs can help if you hit rate limits

# =================================================================================================== #
# RATE LIMITS AND BEST PRACTICES
# =================================================================================================== #

# Census API: ~500 requests per hour (unofficial limit)
# OpenCage: 2,500 requests/day free
# Google Maps: $5 per 1,000 requests after free tier
# MapBox: 100,000 requests/month free

# Best practices:
# - Process in small batches
# - Add delays between requests
# - Cache results to avoid re-geocoding
# - Use the most reliable address data first
