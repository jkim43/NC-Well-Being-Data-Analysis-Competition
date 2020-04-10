FROM jupyter/r-notebook

RUN conda install -c conda-forge r-tidyr==1.0.0

ARG NB_USER
ARG NB_UID

RUN pip3 install jupyterlab==1.0.9

COPY install.R ./
RUN if [ -f install.R ]; then R -f install.R; fi
