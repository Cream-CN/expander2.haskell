<#
#>
param(
  [string]$Profile = '',
  [ValidateSet('auto', 'on', 'off')]
  [string]$Llvm = 'auto',
  [string]$RtsOpts = '-A64m',
  [int]$Jobs = 0,
  [string]$Ghc = '',
  [switch]$Compat,
  [switch]$Info,
  [switch]$NoProbe,
  [switch]$Clean
)

$params = @{ Target = 'shell' }
if ($Profile -ne '') { $params['Profile'] = $Profile }
$params['Llvm'] = $Llvm
$params['RtsOpts'] = $RtsOpts
$params['Jobs'] = $Jobs
if ($Ghc -ne '') { $params['Ghc'] = $Ghc }
if ($Compat) { $params['Compat'] = $true }
if ($Info) { $params['Info'] = $true }
if ($NoProbe) { $params['NoProbe'] = $true }
if ($Clean) { $params['Clean'] = $true }

& (Join-Path $PSScriptRoot 'build.ps1') @params
exit $LASTEXITCODE
