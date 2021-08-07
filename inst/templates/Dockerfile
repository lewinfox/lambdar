FROM public.ecr.aws/lambda/provided

ENV R_VERSION={{ r_version }}

# Install system packages
RUN  yum -y install wget \
  && yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm \
  && yum -y install openssl-devel \
  && wget https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm \
  && yum -y install R-${R_VERSION}-1-1.x86_64.rpm \
  && rm R-${R_VERSION}-1-1.x86_64.rpm

ENV PATH="${PATH}:/opt/R/${R_VERSION}/bin/"

# Move the runtime and function definition file into the container and make it executable
COPY {{ r_runtime_file }} {{ r_functions_file }} ${LAMBDA_TASK_ROOT}/
RUN chmod 755 -R ${LAMBDA_TASK_ROOT}/

# Write the bootstrap file
RUN printf '#!/bin/sh\ncd $LAMBDA_TASK_ROOT\nRscript runtime.R' > /var/runtime/bootstrap \
  && chmod +x /var/runtime/bootstrap

# The R runtime requires these packages
RUN Rscript -e "install.packages(c('httr', 'jsonlite', 'logger'), repos = c({{ r_package_repos }}))"

# If necessary, install extra R and Linux packages here. This installation step happens last as this
# is the one most likely to change between containers - having it as the last image in the stack
# means we can use cached images as much as possible.
{{#r_packages}}
RUN Rscript -e "install.packages(c({{ r_packages }}), repos = c({{ r_package_repos }}))"
{{/r_packages}}

{{#linux_packages}}
RUN yum -y install {{ linux_packages }}
{{/linux_packages}}

# If any additional files or directories need to be transferred, do that here.
{{#include_files}}
COPY {{ include_files }} ${LAMBDA_TASK_ROOT}
{{/include_files}}
