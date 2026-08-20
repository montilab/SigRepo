@echo off
setlocal

cd /d "%~dp0"

where Rscript >nul 2>nul
if errorlevel 1 (
  echo ERROR: Rscript was not found on PATH.
  echo Use run_all_tests_windows_autodetect.cmd instead.
  pause
  exit /b 1
)

if not exist "results_v4" mkdir "results_v4"

Rscript test_omicsignature_contract.R
set API_EXIT=%ERRORLEVEL%

Rscript test_repository_contract.R
set REPO_EXIT=%ERRORLEVEL%

(
  echo SigRepo reconciled test summary
  echo API contract exit code: %API_EXIT%
  echo Repository contract exit code: %REPO_EXIT%
) > "results_v4\all_tests_summary.txt"

if not "%API_EXIT%"=="0" exit /b 2
if not "%REPO_EXIT%"=="0" exit /b 2

echo Both reconciled contracts passed.
pause
exit /b 0
