FROM debian:bookworm
RUN apt-get  update && apt-get install -y ssl-cert libpq5 ca-certificates curl locales
RUN openssl genrsa -out /etc/ssl/private/enlil.key &&\
    echo "C.UTF-8 UTF-8" > /etc/locale.gen &&\
    dpkg-reconfigure --frontend=noninteractive locales &&\
    update-locale LANG=C.UTF-8
ENV LANG=C.UTF-8

COPY enlil-entrypoint.sh /usr/local/bin/enlil-entrypoint
RUN chmod 555 /usr/local/bin/enlil-entrypoint

COPY tmp/enlil-exe /usr/local/bin/enlil
RUN chmod 555 /usr/local/bin/enlil

ENTRYPOINT /usr/local/bin/enlil-entrypoint

ARG ENLIL_COMMIT
ENV ENLIL_COMMIT=$ENLIL_COMMIT
