# Build the tarball and run R CMD check --as-cran, retrying, from a short path.
#
# Why this exists. devtools::check() builds the vignette in a child R process,
# and the vignette queries the OpenRange database. On this machine a large
# transfer intermittently kills R outright, and it sometimes takes the calling
# session down with it, so a retry loop written in R cannot survive its own
# failure. This loop lives outside R, so a dead R process costs one attempt
# instead of the whole run. It also works around two other local traps:
#
#   * Windows path length. Under a deep working directory the install step
#     overruns the limit and R dies with no message, so the check runs in
#     -WorkDir (a short path) rather than a temp directory.
#   * Network steps. "checking CRAN incoming feasibility" and the system-clock
#     check both reach out over the network and are themselves prone to dying
#     here, so they are off by default. win-builder and R-hub run the incoming
#     checks properly, and dev/release_checks.R section 5 dispatches those.
#
# Usage, from the package root in PowerShell:
#   & .\dev\check_cran.ps1
#   & .\dev\check_cran.ps1 -Attempts 6
#   & .\dev\check_cran.ps1 -NoVignettes   # when the vignette will not build
#   & .\dev\check_cran.ps1 -Remote        # include the remote incoming checks
#
# The leading & is needed because PowerShell will not run a script from the
# current directory without an explicit path.
#
# A green run ends with "Status: OK". Anything else is printed in full. Logs
# are left in -WorkDir as build.log and check.log.
#
# -NoVignettes skips the vignette rebuild in both the build and the check. That
# is the escape hatch for when the database will not cooperate: everything else
# in the check still runs, and the vignette is covered by GitHub Actions and
# R-hub, which build it reliably. Expect one extra NOTE in that mode, "Package
# has a VignetteBuilder field but no prebuilt vignette index", which is caused
# by the flag and not by the package.

param(
  [string]$Pkg      = (Get-Location).Path,
  [string]$WorkDir  = "C:\orchk",
  [string]$Rexe     = "",
  [int]   $Attempts = 4,
  [switch]$Remote,
  [switch]$NoVignettes
)

$ErrorActionPreference = "Continue"

if (-not $Rexe) {
  $found = Get-Command "R.exe" -ErrorAction SilentlyContinue
  if (-not $found) {
    throw "Could not find R.exe on PATH. Pass -Rexe 'C:\Program Files\R\R-4.4.2\bin\R.exe'."
  }
  $Rexe = $found.Source
}

if (-not (Test-Path (Join-Path $Pkg "DESCRIPTION"))) {
  throw "No DESCRIPTION in $Pkg. Run this from the package root, or pass -Pkg."
}

function Get-Field([string]$name) {
  $m = Select-String -Path (Join-Path $Pkg "DESCRIPTION") -Pattern "^${name}:\s*(.+)$"
  if (-not $m) { throw "No $name field in DESCRIPTION." }
  return $m.Matches[0].Groups[1].Value.Trim()
}

# Run R and return its exit code, with stdout and stderr merged into $LogPath.
# Start-Process is used rather than "& R ... 2>&1" because in Windows
# PowerShell redirecting a native command's stderr wraps every line in a
# NativeCommandError, which makes a successful build look like it failed.
function Invoke-R([string[]]$RArgs, [string]$LogPath) {
  $o = "$LogPath.stdout"
  $e = "$LogPath.stderr"
  $proc = Start-Process -FilePath $Rexe -ArgumentList $RArgs -NoNewWindow -Wait -PassThru `
                        -RedirectStandardOutput $o -RedirectStandardError $e
  $text = @()
  foreach ($f in @($o, $e)) {
    if (Test-Path $f) { $text += Get-Content $f -ErrorAction SilentlyContinue }
  }
  $text | Out-File -Encoding utf8 $LogPath
  Remove-Item $o, $e -ErrorAction SilentlyContinue
  return $proc.ExitCode
}

$pkgName = Get-Field "Package"
$version = Get-Field "Version"
$tar     = "${pkgName}_${version}.tar.gz"
$log     = "$pkgName.Rcheck\00check.log"

Write-Output "R:       $Rexe"
Write-Output "package: $pkgName $version"
Write-Output "workdir: $WorkDir"
if ($NoVignettes) { Write-Output "mode:    skipping the vignette rebuild" }
Write-Output ""

New-Item -ItemType Directory -Force -Path $WorkDir | Out-Null
Set-Location $WorkDir
Remove-Item -Force $tar -ErrorAction SilentlyContinue

if ($Remote) {
  Remove-Item Env:\_R_CHECK_CRAN_INCOMING_REMOTE_ -ErrorAction SilentlyContinue
  Remove-Item Env:\_R_CHECK_SYSTEM_CLOCK_ -ErrorAction SilentlyContinue
} else {
  $env:_R_CHECK_CRAN_INCOMING_REMOTE_ = "false"
  $env:_R_CHECK_SYSTEM_CLOCK_ = "0"
}

$buildLog = Join-Path $WorkDir "build.log"
$checkLog = Join-Path $WorkDir "check.log"

$buildArgs = @("CMD", "build")
$checkArgs = @("CMD", "check", "--as-cran")
if ($NoVignettes) {
  $buildArgs += "--no-build-vignettes"
  $checkArgs += "--no-build-vignettes"
}
$buildArgs += $Pkg

# Build. Without -NoVignettes the vignette is built here, so this is the step
# most likely to die.
$built = $false
for ($i = 1; $i -le $Attempts; $i++) {
  Write-Output "=== build attempt $i of $Attempts ==="
  $code = Invoke-R -RArgs $buildArgs -LogPath $buildLog
  Get-Content $buildLog -ErrorAction SilentlyContinue | ForEach-Object { "  $_" }
  if (Test-Path $tar) { $built = $true; break }
  Write-Output "    build died (exit $code); retrying"
  Start-Sleep -Seconds 5
}

if (-not $built) {
  Write-Output ""
  Write-Output "BUILD FAILED after $Attempts attempts. Full log: $buildLog"
  Write-Output ""
  Write-Output "Exit 15 with no message at 'creating vignettes' is the local crash."
  Write-Output "Check free memory, since a build spawns several R processes, then"
  Write-Output "either run this again or use -NoVignettes to check everything else."
  Write-Output "Exit 1 there is a real vignette error and the log is the thing to read."
  exit 1
}

# Check. Retry until the log carries a Status line, which only a check that ran
# to completion writes.
for ($i = 1; $i -le $Attempts; $i++) {
  Write-Output "=== check attempt $i of $Attempts ==="
  Remove-Item -Recurse -Force "$pkgName.Rcheck" -ErrorAction SilentlyContinue
  $code = Invoke-R -RArgs ($checkArgs + $tar) -LogPath $checkLog
  if ((Test-Path $log) -and (Select-String -Path $log -Pattern "^Status:" -Quiet)) { break }
  Write-Output "    check died (exit $code); retrying"
  Start-Sleep -Seconds 5
}

Write-Output ""
if (-not (Test-Path $log)) {
  Write-Output "CHECK NEVER PRODUCED A LOG after $Attempts attempts. See $checkLog"
  exit 1
}

if (-not (Select-String -Path $log -Pattern "^Status:" -Quiet)) {
  Write-Output "CHECK NEVER RAN TO COMPLETION after $Attempts attempts. Last steps:"
  Get-Content $log | Select-Object -Last 15
  exit 1
}

Write-Output "=== result ($WorkDir\$log) ==="
if ($NoVignettes) {
  Write-Output "(vignette rebuild skipped; GitHub Actions and R-hub cover it)"
}
Select-String -Path $log -Pattern "^\* checking" | ForEach-Object { $_.Line } |
  Where-Object { $_ -notmatch "OK$" }
Select-String -Path $log -Pattern "^Status:" | ForEach-Object { $_.Line }
