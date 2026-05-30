#!/usr/bin/env bash
set -e

cd "$(dirname "$0")"

dotnet tool restore
dotnet restore ./term-proj.fsproj
dotnet run --project ./term-proj.fsproj