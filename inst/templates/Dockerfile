FROM public.ecr.aws/lambda/provided

ENV R_VERSION={{ r_version }}

# Install system packages
RUN  yum makecache \
  && yum -y install wget \
  && yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm \
  && yum -y install openssl-devel \
  && wget https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm \
  && yum -y install R-${R_VERSION}-1-1.x86_64.rpm \
  && rm R-${R_VERSION}-1-1.x86_64.rpm

ENV PATH="${PATH}:/opt/R/${R_VERSION}/bin/"

# The R runtime requires these packages
RUN Rscript -e 'install.packages(c("httr", "jsonlite", "logger"), repos = c({{{ r_package_repos }}}))'

# Move the runtime into the container
COPY {{ r_runtime_file }} ${LAMBDA_TASK_ROOT}/

# Write the bootstrap file
RUN printf '#!/bin/sh\ncd $LAMBDA_TASK_ROOT\nRscript lambdar_runtime.R' > /var/runtime/bootstrap \
  && chmod +x /var/runtime/bootstrap

# If necessary, install extra R and Linux packages here. As the base Lambda image is based on CentOS
# the `yum` package manager is used. Make sure you have the correct package name/s.
{{#linux_packages}}
RUN yum -y install {{ linux_packages }}
{{/linux_packages}}

{{#r_packages}}
RUN Rscript -e 'install.packages(c({{{ r_packages }}}), repos = c({{{ r_package_repos }}}))'
{{/r_packages}}

# If your app needs any environment variables set, do it here
{{#env}}
ENV {{{ env }}}
{{/env}}

# Copy over any user-generated files needed for the function to run. This could be R scripts, data
# files or any other resources.
{{#include_files}}
COPY {{{ include_files }}} ${LAMBDA_TASK_ROOT}/
{{/include_files}}

# Make everything in the task root executable.
# TODO: is this a good idea?
RUN chmod 755 -R ${LAMBDA_TASK_ROOT}/

# The entrypoint to be used for the function
CMD [ {{{ lambda_entrypoint }}} ]
