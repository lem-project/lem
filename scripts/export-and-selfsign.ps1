# export-and-selfsign.ps1
# Run with: powershell -ExecutionPolicy Bypass -File .\export-and-selfsign.ps1

$certName = "CN=MyApp Test Cert"
$pfxPath  = "$PSScriptRoot\codesign.pfx"
$cerPath  = "$PSScriptRoot\codesign.cer"
$certPassword = "testpass123"

Write-Host "== Create self-signed certificate =="

# Remove existing cert with same name
Get-ChildItem Cert:\CurrentUser\My | Where-Object { $_.Subject -eq $certName } | Remove-Item -Force -ErrorAction SilentlyContinue

# Create self-signed certificate
$cert = New-SelfSignedCertificate `
    -Subject $certName `
    -CertStoreLocation Cert:\CurrentUser\My `
    -Type CodeSigningCert `
    -KeyExportPolicy Exportable `
    -HashAlgorithm SHA256 `
    -KeyLength 2048

# Export PFX
Export-PfxCertificate `
    -Cert $cert `
    -FilePath $pfxPath `
    -Password (ConvertTo-SecureString -String $certPassword -Force -AsPlainText)

# Export CER
Export-Certificate `
    -Cert $cert `
    -FilePath $cerPath

Write-Host "Created certificate:"
Write-Host "  PFX: $pfxPath"
Write-Host "  CER: $cerPath"
Write-Host "  Password: $certPassword"

Write-Host "== Sign all EXE/DLL in bin directory =="

$signtool = 'C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe'
if (-not (Test-Path $signtool)) {
    throw "signtool.exe not found: $signtool"
}

$binDir = Join-Path (Resolve-Path "$PSScriptRoot\..") 'bin'
if (-not (Test-Path $binDir)) {
    throw "bin directory does not exist: $binDir"
}

Get-ChildItem -Path $binDir -Recurse -Include *.exe,*.dll | ForEach-Object {
    Write-Host "Signing: $($_.FullName)"
    & $signtool sign /f $pfxPath /p $certPassword /fd SHA256 /tr http://timestamp.digicert.com /td SHA256 $_.FullName
}
Write-Host "Signing completed."
