<#
  expander2.haskell — 唯一构建入口：只编译 app\Main.hs 这一个 CLI。

  用法：
    .\build.ps1                     # 默认档（ghc-flags.mk 的 DEFAULT_PROFILE）-> .build\expander2-hs.exe
    .\build.ps1 -Profile fast       # fast / strong / extreme / compat
    .\build.ps1 -Compat             # 等价 -Profile compat（GHC 内置 NCG 后端）
    .\build.ps1 -Llvm off           # auto（默认，PATH 里有 llc/opt 才启用）/ on / off
    .\build.ps1 -Jobs 6             # -j6；默认 0 = 不传 -j
    .\build.ps1 -Info               # 只打印将要执行的命令
    .\build.ps1 -Clean              # 清掉 .build* 后退出
#>
param(
  [ValidateSet('fast', 'strong', 'extreme', 'compat')]
  [string]$Profile = '',
  [ValidateSet('auto', 'on', 'off')]
  [string]$Llvm = 'auto',
  [int]$Jobs = 0,
  [string]$Ghc = '',
  [switch]$Compat,
  [switch]$Clean,
  [switch]$Info
)

$ErrorActionPreference = 'Stop'
$root = [System.IO.Path]::GetFullPath($PSScriptRoot).TrimEnd('\', '/')

if ($Clean) {
  Get-ChildItem -LiteralPath $root -Directory -Filter '.build*' |
    ForEach-Object { Remove-Item -LiteralPath $_.FullName -Recurse -Force; "removed: $($_.Name)" }
  exit 0
}

# 参数真源是 ghc-flags.mk；这里只做正则抽取，不另抄一份
$mkPath = Join-Path $root 'ghc-flags.mk'
if (-not (Test-Path -LiteralPath $mkPath)) { throw "缺少 $mkPath" }
$profileFlags = @{}
$defaultProfile = 'extreme'
$ghcHome = ''
foreach ($line in Get-Content -LiteralPath $mkPath) {
  if ($line -match '^\s*PROFILE_(FAST|STRONG|EXTREME|COMPAT)\s*:=\s*(.*)$') {
    $profileFlags[$Matches[1]] = $Matches[2].Trim()
  } elseif ($line -match '^\s*DEFAULT_PROFILE\s*:=\s*(.*)$') {
    $defaultProfile = $Matches[1].Trim()
  } elseif ($line -match '^\s*GHC_HOME\s*:=\s*(.*)$') {
    $ghcHome = $Matches[1].Trim()
  }
}

$prof = if ($Profile -ne '') { $Profile } elseif ($Compat) { 'compat' } else { $defaultProfile }
$optFlags = $profileFlags[$prof.ToUpper()]
if (-not $optFlags) { throw "未知 profile：$prof" }

# GHC：显式 -Ghc > GHC_HOME 下的 ghc > PATH
$ghcExe = 'ghc'
if ($Ghc -ne '') {
  $ghcExe = $Ghc
} elseif ($ghcHome -ne '') {
  $cand = Join-Path $ghcHome 'ghc.exe'
  if (Test-Path -LiteralPath $cand) { $ghcExe = $cand }
}

# LLVM 后端：compat 固定关闭
$llvmFlags = @()
$llvmOn = $false
switch ($Llvm) {
  'on' { $llvmOn = $true }
  'auto' { $llvmOn = [bool]((Get-Command llc -ErrorAction SilentlyContinue) -and (Get-Command opt -ErrorAction SilentlyContinue)) }
  'off' { $llvmOn = $false }
}
if ($prof -eq 'compat') { $llvmOn = $false }
if ($Llvm -eq 'on' -and -not $llvmOn) { throw 'Llvm=on 但 PATH 里找不到 llc/opt' }
if ($llvmOn) { $llvmFlags = @('-fllvm', '-optlo=-O3', '-optlc=-O3') }

$argsList = @()
$argsList += $optFlags.Split(' ', [System.StringSplitOptions]::RemoveEmptyEntries)
$argsList += $llvmFlags
$argsList += @('-rtsopts', '-with-rtsopts=-A64m', '-isrc', '-iapp', '--make')
if ($Jobs -gt 0) { $argsList += "-j$Jobs" }

$outdir = '.build'
$bin = '.build\expander2-hs.exe'
$argsList += @('-outputdir', $outdir, '-o', $bin, 'app\Main.hs')

Push-Location $root
try {
  New-Item -ItemType Directory -Force -Path $outdir | Out-Null
  $shown = "$ghcExe " + ($argsList -join ' ')
  if ($Info) { Write-Host $shown; exit 0 }
  Write-Host $shown
  & $ghcExe @argsList
  if ($LASTEXITCODE -ne 0) { throw "ghc 编译失败（exit $LASTEXITCODE）" }
  Write-Host "built: $bin  (profile=$prof, llvm=$(if ($llvmOn) { 'on' } else { 'off' }))"
} finally {
  Pop-Location
}
