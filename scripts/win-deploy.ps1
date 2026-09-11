# win-deploy.ps1 - Build and package Lem as a Windows application.
#
# Produces:
#   bin\lem.exe + DLLs                          (deploy-op output, icon embedded)
#   build\windows\Lem-<version>-windows-x64.zip (portable archive)
#   build\windows\Lem-<version>-setup.exe       (installer, if Inno Setup is installed)
#
# The application icon is embedded into a copy of the SBCL runtime BEFORE
# the image is dumped: editing PE resources of the finished executable
# corrupts the appended Lisp core.
#
# Usage: powershell -ExecutionPolicy Bypass -File scripts\win-deploy.ps1
#        [-SkipBuild] [-Sign]

param(
    [switch]$SkipBuild,   # reuse existing bin\ output, only package
    [switch]$Sign         # self-sign bin\*.exe/dll via export-and-selfsign.ps1
)

$ErrorActionPreference = 'Stop'

$root     = (Resolve-Path "$PSScriptRoot\..").Path
$buildDir = Join-Path $root 'build\windows'
$binDir   = Join-Path $root 'bin'
$icon     = Join-Path $root 'scripts\install\lem.ico'
New-Item -ItemType Directory -Force $buildDir | Out-Null

# --- Version (from defsystem "lem" in lem.asd) -------------------------------
$asd = Get-Content (Join-Path $root 'lem.asd') -Raw
if ($asd -notmatch '(?s)\(defsystem "lem"\s+:version "([0-9.]+)"') {
    throw "Could not find :version of system `"lem`" in lem.asd"
}
$version = $Matches[1]
Write-Host "== Lem $version =="

# --- rcedit (pinned) ----------------------------------------------------------
$rcedit = Join-Path $buildDir 'rcedit-x64.exe'
$rceditUrl = 'https://github.com/electron/rcedit/releases/download/v2.0.0/rcedit-x64.exe'
$rceditSha256 = '3E7801DB1A5EDBEC91B49A24A094AAD776CB4515488EA5A4CA2289C400EADE2A'
if (-not (Test-Path $rcedit)) {
    Write-Host "Downloading rcedit..."
    Invoke-WebRequest -Uri $rceditUrl -OutFile $rcedit
}
if ((Get-FileHash $rcedit -Algorithm SHA256).Hash -ne $rceditSha256) {
    Remove-Item $rcedit -Force
    throw "rcedit-x64.exe checksum mismatch; delete and retry"
}

# --- ASDF 3.3.7 (pinned; SBCL 2.6.4 bundles 3.3.1, too old for lem) -----------
$asdfLisp = Join-Path $buildDir 'asdf-3.3.7.lisp'
$asdfUrl = 'https://asdf.common-lisp.dev/archives/asdf-3.3.7.lisp'
$asdfSha256 = '874E8F235EA633A090C7AB2665B1F624D1910EB7F6FC10349A596085C1CBB189'
if (-not (Test-Path $asdfLisp)) {
    Write-Host "Downloading ASDF 3.3.7..."
    Invoke-WebRequest -Uri $asdfUrl -OutFile $asdfLisp
}
if ((Get-FileHash $asdfLisp -Algorithm SHA256).Hash -ne $asdfSha256) {
    Remove-Item $asdfLisp -Force
    throw "asdf-3.3.7.lisp checksum mismatch; delete and retry"
}

if (-not $SkipBuild) {
    # --- SBCL runtime with icon/version resources ----------------------------
    $sbcl = Get-Command sbcl -ErrorAction SilentlyContinue
    if ($sbcl) { $sbclDir = Split-Path $sbcl.Source }
    else       { $sbclDir = 'C:\Program Files\Steel Bank Common Lisp' }
    $sbclExe  = Join-Path $sbclDir 'sbcl.exe'
    $sbclCore = Join-Path $sbclDir 'sbcl.core'
    if (-not (Test-Path $sbclExe))  { throw "sbcl.exe not found in $sbclDir" }
    if (-not (Test-Path $sbclCore)) { throw "sbcl.core not found in $sbclDir" }

    $runtime = Join-Path $buildDir 'lem-runtime.exe'
    Copy-Item $sbclExe $runtime -Force
    Write-Host "Embedding icon and version info into runtime..."
    & $rcedit $runtime `
        --set-icon $icon `
        --set-version-string ProductName 'Lem' `
        --set-version-string FileDescription 'Lem Editor' `
        --set-version-string CompanyName 'lem-project' `
        --set-version-string LegalCopyright 'MIT License' `
        --set-file-version "$version.0" `
        --set-product-version "$version.0"
    if ($LASTEXITCODE -ne 0) { throw "rcedit failed" }

    # --- Build (deploy-op dumps into bin\ and exits the process) -------------
    if (Test-Path $binDir) { Remove-Item $binDir -Recurse -Force }
    Write-Host "Building lem.exe (this takes a while)..."
    # The runtime copy lives outside the SBCL installation; point it back
    # at the contrib modules (sb-posix etc.).
    $env:SBCL_HOME = $sbclDir
    & $runtime --core $sbclCore --no-sysinit --no-userinit --disable-debugger `
        --load (Join-Path $root 'scripts\win-deploy.lisp')
    if (-not (Test-Path (Join-Path $binDir 'lem.exe'))) {
        throw "Build failed: bin\lem.exe was not produced"
    }
}

Write-Host "== bin contents =="
Get-ChildItem $binDir | Format-Table Name, Length -AutoSize | Out-String | Write-Host

if ($Sign) {
    & powershell -ExecutionPolicy Bypass -File (Join-Path $root 'scripts\export-and-selfsign.ps1')
}

# --- Portable zip -------------------------------------------------------------
$zip = Join-Path $buildDir "Lem-$version-windows-x64.zip"
if (Test-Path $zip) { Remove-Item $zip -Force }
Compress-Archive -Path "$binDir\*" -DestinationPath $zip
Write-Host "Portable archive: $zip"

# --- Installer (Inno Setup) ---------------------------------------------------
$iscc = Get-Command iscc -ErrorAction SilentlyContinue
if (-not $iscc) {
    $isccPath = @(
        "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe",
        "$env:ProgramFiles\Inno Setup 6\ISCC.exe",
        "$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe"
    ) | Where-Object { Test-Path $_ } | Select-Object -First 1
} else { $isccPath = $iscc.Source }

if ($isccPath) {
    Write-Host "Building installer..."
    & $isccPath "/DAppVersion=$version" "/O$buildDir" (Join-Path $root 'scripts\install\lem.iss')
    if ($LASTEXITCODE -ne 0) { throw "ISCC failed" }
    Write-Host "Installer: $buildDir\Lem-$version-setup.exe"
} else {
    Write-Host "Inno Setup not found; skipping installer. Install with:"
    Write-Host "  winget install JRSoftware.InnoSetup"
}
