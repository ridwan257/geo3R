FROM r-base:latest


WORKDIR /usr/src/app

COPY . .

RUN Rscript set-up.R

CMD ["Rscript", "run.R"]