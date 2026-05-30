@echo off
setlocal

cd /d "%~dp0"

dotnet tool restore
dotnet restore ./term-proj.fsproj
dotnet run --project ./term-proj.fsproj