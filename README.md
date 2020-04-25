# personal-finance-monitoring

## docker commands

build:
```bash
docker build \
    --no-cache \
    -f Dockerfile \
    -t personal-finance-monitoring \
    .
```

start container:
```bash
docker run --rm \
    -p 8787:8787 \
    -e PASSWORD=pass \
    -v $(pwd):/home/rstudio \
    -v ~/.rstudio-desktop/monitored/user-settings/user-settings:/home/rstudio/.rstudio/monitored/user-settings/user-settings \
    personal-finance-monitoring:latest
```
