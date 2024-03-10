FROM mcr.microsoft.com/dotnet/sdk:8.0 AS base

RUN apt -y update && apt install -y gcc

FROM base AS build
WORKDIR /app

# Copy everything
COPY . ./

RUN gcc -std=c99 -fPIC -c src/Disk/bplustree.c -o src/Disk/bplustree.o
RUN gcc -std=c99 -fPIC -shared -o src/Disk/libbplustree.so src/Disk/bplustree.o

# Restore as distinct layers
RUN dotnet restore
# Build and publish a release
RUN dotnet publish -c Release -o out src/Server/Server.fsproj

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:8.0
WORKDIR /app
COPY --from=build /app/out .

ENTRYPOINT ["dotnet", "Server.dll"]
