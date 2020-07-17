# Build Eucalypt test harness as a container. The eu executable must
# be provided at runtime in an attached volume and env var EXECUTABLE
# must contain the path.
FROM ubuntu:20.04

RUN apt-get update \
 && apt-get install -y libgmp10 python3-pip python3-dev
RUN pip3 install pipenv

ADD Pipfile Pipfile
ADD Pipfile.lock Pipfile.lock
ADD eut.py eut.py
ADD test test
ADD entrypoint.sh entrypoint.sh

ENV LC_ALL "C.UTF-8"
ENV LANG "C.UTF-8"
ENV EXECUTABLE "<REPLACEME>"

ENTRYPOINT ["./entrypoint.sh"]

