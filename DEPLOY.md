# Deployment Guide

This document explains how to deploy the Real Estate Investment Calculator to the web.

## 1. Prerequisites

- **R Account**: Create an account on [shinyapps.io](https://www.shinyapps.io/).
- **RStudio IDE**: Recommended for easy deployment.
- **rsconnect**: The R package used for deployment.

## 2. Setup (First Time Only)

Run the following commands in your R console to install the required package and link your account:

```r
install.packages("rsconnect")
library(rsconnect)

# You can find your token and secret in your shinyapps.io dashboard under "Tokens"
rsconnect::setAccountInfo(name='<YOUR_ACCOUNT_NAME>',
			  token='<YOUR_TOKEN>',
			  secret='<YOUR_SECRET>')
```

## 3. Deploying the App

To deploy the app, run the following command in the R console:

```r
library(rsconnect)
deployApp()
```

Or, if you are using RStudio, simply click the **"Publish"** button (blue icon) in the top right corner of the source editor window (when `ui.R` or `server.R` is open).

## 4. SEO & Analytics Configuration

### SEO Meta Tags
The file `R/ui.R` has been pre-configured with SEO meta tags. You should customize them before deploying:

- Open `R/ui.R`.
- Look for the `tags$head(...)` section.
- Update the `content` attributes for:
  - `description`: A short summary of your tool.
  - `keywords`: Relevant keywords for search engines.
  - `author`: Your name or company name.
  - `og:image`: URL to a preview image for social media sharing.

### Google Analytics
To enable Google Analytics tracking:

1. Create a Google Analytics 4 (GA4) property.
2. Get your **Measurement ID** (starts with `G-`).
3. Open `www/google-analytics.html`.
4. Replace `G-XXXXXXXXXX` with your actual ID.
5. Open `R/ui.R` and **uncomment** the line:
   ```r
   # includeHTML("www/google-analytics.html")
   ```

## 5. Static Assets
The `www` directory is used for static files like images, CSS, or JS.
- `robots.txt`: Already created to allow search engine crawling.
- `favicon.ico`: You can add your own favicon here.

## 6. Verification
After deployment, visit your app URL (e.g., `https://yourname.shinyapps.io/mortgage`).
- Right-click and "View Page Source".
- Verify that your `<meta>` tags and `<title>` are present in the `<head>` section.
