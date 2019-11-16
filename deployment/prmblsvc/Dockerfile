FROM ubuntu:disco
RUN apt-get update && apt-get install -y libpq-dev
COPY /server-exe /
CMD ["./server-exe"]
