FROM python

RUN mkdir -p /apps && \
    mkdir -p /logs
COPY check_gcs_build_logs.py /apps/  
RUN chmod 744 /apps/check_gcs_build_logs.py


ENTRYPOINT ["/apps/check_gcs_build_logs.py"]
