FROM debian:trixie
RUN apt-get  update && apt-get install -y ssl-cert libpq5 ca-certificates curl locales
RUN openssl genrsa -out /etc/ssl/private/share.key &&\
    echo "C.UTF-8 UTF-8" > /etc/locale.gen &&\
    dpkg-reconfigure --frontend=noninteractive locales &&\
    update-locale LANG=C.UTF-8
ENV LANG=C.UTF-8

COPY share-task-runner-entrypoint.sh /usr/local/bin/share-task-runner-entrypoint
RUN chmod 555 /usr/local/bin/share-task-runner-entrypoint

COPY tmp/share-task-runner /usr/local/bin/share-task-runner
RUN chmod 555 /usr/local/bin/share-task-runner

ENTRYPOINT /usr/local/bin/share-task-runner-entrypoint

ARG SHARE_COMMIT
ENV SHARE_COMMIT=$SHARE_COMMIT
