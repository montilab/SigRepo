@echo off
setlocal EnableExtensions EnableDelayedExpansion

cd /d "%~dp0"

echo ============================================================
echo SigRepo reconciled contract tests
echo Working folder: %CD%
echo ============================================================
echo.

set "RSCRIPT="

for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do (
  if not defined RSCRIPT set "RSCRIPT=%%~fI"
)

if not defined RSCRIPT (
  for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
    if exist "%%~fD\bin\x64\Rscript.exe" set "RSCRIPT=%%~fD\bin\x64\Rscript.exe"
  )
)

if not defined RSCRIPT if defined ProgramFiles(x86) (
  for /d %%D in ("%ProgramFiles(x86)%\R\R-*") do (
    if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
    if exist "%%~fD\bin\x64\Rscript.exe" set "RSCRIPT=%%~fD\bin\x64\Rscript.exe"
  )
)

if not defined RSCRIPT (
  for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do (
    if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
    if exist "%%~fD\bin\x64\Rscript.exe" set "RSCRIPT=%%~fD\bin\x64\Rscript.exe"
  )
)

if not defined RSCRIPT (
  echo Rscript.exe was not found automatically.
  echo.
  echo In RStudio, run:
  echo   R.home("bin")
  echo.
  echo Enter the full path to Rscript.exe, for example:
  echo   C:\Program Files\R\R-4.4.0\bin\Rscript.exe
  echo.
  set /p "RSCRIPT=Full path to Rscript.exe: "
  set "RSCRIPT=!RSCRIPT:"=!"
)

if not exist "%RSCRIPT%" (
  echo.
  echo ERROR: Rscript.exe does not exist at:
  echo %RSCRIPT%
  echo.
  pause
  exit /b 1
)

if not exist "test_omicsignature_contract.R" (
  echo ERROR: test_omicsignature_contract.R is missing.
  pause
  exit /b 1
)

if not exist "test_repository_contract.R" (
  echo ERROR: test_repository_contract.R is missing.
  pause
  exit /b 1
)

if not exist "results_v4" mkdir "results_v4"

echo Using:
echo "%RSCRIPT%"
echo.

echo [1/2] Running OmicSignature API contract v4...
"%RSCRIPT%" "test_omicsignature_contract.R"
set "API_EXIT=%ERRORLEVEL%"
echo.

echo [2/2] Running repository contract...
"%RSCRIPT%" "test_repository_contract.R"
set "REPO_EXIT=%ERRORLEVEL%"
echo.

(
  echo SigRepo reconciled test summary
  echo Generated: %DATE% %TIME%
  echo API contract exit code: %API_EXIT%
  echo Repository contract exit code: %REPO_EXIT%
) > "results_v4\all_tests_summary.txt"

if not "%API_EXIT%"=="0" (
  echo API CONTRACT DID NOT PASS. Exit code: %API_EXIT%
)

if not "%REPO_EXIT%"=="0" (
  echo REPOSITORY CONTRACT DID NOT PASS. Exit code: %REPO_EXIT%
)

if "%API_EXIT%"=="0" if "%REPO_EXIT%"=="0" (
  echo SUCCESS: Both reconciled contracts passed.
  echo.
  echo Send these files:
  echo %CD%\results_v4\omicsignature_api_contract_v4_results.txt
  echo %CD%\results_v4\repository_contract_results.txt
  echo %CD%\results_v4\all_tests_summary.txt
  echo.
  pause
  exit /b 0
)

echo.
echo Review the result files under:
echo %CD%\results_v4
echo.
pause
exit /b 2
