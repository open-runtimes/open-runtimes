FROM mcr.microsoft.com/dotnet/sdk:3.1

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start

WORKDIR /usr/local/src
COPY . .

RUN ["chmod", "+x", "/usr/local/src/build.sh"]
RUN ["chmod", "+x", "/usr/local/src/start.sh"]

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]