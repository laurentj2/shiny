Shiny Server Pro in Docker
==========================

This is a Dockerfile for Shiny Server Pro on Ubuntu 14.04. It is based on the [rocker-org Shiny image](https://github.com/rocker-org/shiny), except that it builds on Ubuntu instead of Debian, and installs Shiny Server Pro instead of Shiny Server.

This Dockerfile also installs some examples from the an old commit of the [shiny-examples](https://github.com/rstudio/shiny-examples/tree/230acb1990f1ea6049885837eac8e58ed8405f80) repo. The purpose of this Dockerfile is to run specific examples that require Shiny Server Pro, and currently can't be run on Shinyapps.io.

**NOTE:** Building the Docker image requires compiling dplyr, which can consume a lot of memory. If you're running this on a VM, you may need to enable swap for the VM.


## Usage:

To build (assuming you're in this directory):

```sh
docker build -t ssp-examples .
```


To run a temporary container with Shiny Server Pro:

```sh
docker run --rm -p 3838:3838 ssp-examples
```

Then you can visit examples at (e.g.):

* http://127.0.0.1:3838/001-hello/
* http://127.0.0.1:3838/029-authentication-and-database/
* http://127.0.0.1:3838/056-personalized-ui/

(If using boot2docker, visit http://192.168.59.103:3838/appdir/.) A full list of the examples is at the [shiny-examples](https://github.com/rstudio/shiny-examples/tree/230acb1990f1ea6049885837eac8e58ed8405f80) repo.


To expose a directory on the host to the container use `-v <host_dir>:<container_dir>`. The following command will use `/srv/shinylog` **on the host** as the directory for logs. Note that if the directories on the host don't already exist, they will be created automatically.

```sh
docker run --rm -p 3838:3838 \
    -v /srv/shinylog/:/var/log/ \
    ssp-examples
```


In a real deployment scenario, you will probably want to run the container in detached mode (`-d`) and listening on the host's port 80 (`-p 80:3838`):

```sh
docker run -d -p 80:3838 \
    -v /srv/shinylog/:/var/log/ \
    ssp-examples
```
