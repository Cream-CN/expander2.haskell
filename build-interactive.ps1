<#
  expander2.haskell - 交互式构建菜单（build-menu）

  用法：
    .\build-interactive.ps1                  # 打开菜单，按数字选；直接回车 = 构建主程序
    .\build-interactive.ps1 -NoPause         # 构建完不停留（给脚本/任务调用）
    .\build-interactive.ps1 -Target all -Profile fast -NoPause
                                             # 带参数时跳过菜单，直接按参数构建一次
#>
param(
  [ValidateSet('main', 'shell', 'all')]
  [string]$Target = '',
  [string]$Profile = '',
  [ValidateSet('auto', 'on', 'off')]
  [string]$Llvm = 'auto',
  [int]$Jobs = -1,
  [string]$RtsOpts = '-A64m',
  [ValidateSet('all', 'normal', 'compat')]
  [string]$DllWhich = '',
  [switch]$Example,
  [switch]$Info,
  [switch]$NoPause
)

$ErrorActionPreference = 'Stop'
$script:root = [System.IO.Path]::GetFullPath($PSScriptRoot).TrimEnd('\', '/')
$script:buildScript = Join-Path $script:root 'build.ps1'
$script:dllScript = Join-Path $script:root 'build-dll.ps1'
$script:allScript = Join-Path $script:root 'build-all.ps1'
$script:mkFile = Join-Path $script:root 'ghc-flags.mk'

if (-not (Test-Path -LiteralPath $script:buildScript)) {
  Write-Host "[err] 找不到 build.ps1（应当与本脚本放在同一目录）" -ForegroundColor Red
  exit 1
}

# --- 从 ghc-flags.mk 读档位清单（只读名字和默认值，不读参数内容） ---------

function Get-ProfileCatalog {
  if (-not (Test-Path -LiteralPath $script:mkFile)) {
    Write-Host "[warn] 缺少 ghc-flags.mk，档位清单退回到内置列表" -ForegroundColor Yellow
    return @{ Names = @('fast', 'compat', 'strong', 'extreme'); Default = 'extreme' }
  }
  $names = @()
  $default = 'extreme'
  foreach ($raw in (Get-Content -LiteralPath $script:mkFile -Encoding utf8)) {
    $line = $raw.Trim()
    if ($line -match '^(PROFILE_[A-Z]+)\s*:=') { $names += $Matches[1].Substring(8).ToLowerInvariant() }
    elseif ($line -match '^(DEFAULT_PROFILE)\s*:=\s*(.*)$') { $default = $Matches[2].Trim().ToLowerInvariant() }
  }
  if ($names.Count -eq 0) { $names = @('fast', 'compat', 'strong', 'extreme') }
  return @{ Names = $names; Default = $default }
}

function Get-ProfileDesc([string]$name) {
  switch ($name) {
    'fast'    { '只开 GHC 标准 -O2，日常改一行就看结果最快' }
    'compat'  { '-O2 + 强制内置 NCG 后端；换机器 / 换 GHC / 排错用' }
    'strong'  { '标准优化 + 4 轮化简 + 静态参数变换 + 严格字段拆箱' }
    'extreme' { '默认最强劲档：跨模块激进特化 + 20 次化简迭代' }
    default   { 'ghc-flags.mk 里的自定义档' }
  }
}

# --- 状态 ---------------------------------------------------------------

$catalog = Get-ProfileCatalog
if ($Profile -eq '') { $Profile = $catalog.Default }
$Profile = $Profile.ToLowerInvariant()

# 目标 / 档位 / 后端 / 并行度 / RTS / DLL：命令行给了就用命令行的，否则用默认
$state = @{
  Target   = if ($Target -ne '') { $Target } else { 'main' }
  Profile  = $Profile
  Llvm     = $Llvm
  Jobs     = $Jobs                 # -1 = 自动
  RtsOpts  = $RtsOpts
  DllWhich = if ($DllWhich -ne '') { $DllWhich } else { 'all' }
  Example  = [bool]$Example
  NoDeploy = $false
  NoProbe  = $false
}

function Get-JobsText { if ($state.Jobs -lt 0) { '自动' } else { "$($state.Jobs)" } }
function Get-RtsText { if ([string]::IsNullOrWhiteSpace($state.RtsOpts)) { '（不传）' } else { $state.RtsOpts } }
function Get-ExampleText { if ($state.Example) { '开' } else { '关' } }
function Get-StateLine {
  '目标={0}  档位={1}  后端={2}  并行={3}  RTS={4}  DLL={5}  示例={6}' -f `
    $state.Target, $state.Profile, $state.Llvm, (Get-JobsText), (Get-RtsText),
    $state.DllWhich, (Get-ExampleText)
}

# --- 调用 build.ps1（子进程，失败退出码不会带走菜单） ---------------------

function Invoke-Build([hashtable]$extra) {
  $argList = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', $script:buildScript)
  $all = @{
    Target  = $state.Target
    Profile = $state.Profile
    Llvm    = $state.Llvm
    RtsOpts = $state.RtsOpts
  }
  if ($state.Jobs -ge 0) { $all['Jobs'] = $state.Jobs }
  if ($state.NoDeploy) { $all['NoDeploy'] = $true }
  if ($state.NoProbe) { $all['NoProbe'] = $true }
  if ($extra) { foreach ($k in $extra.Keys) { $all[$k] = $extra[$k] } }

  foreach ($k in $all.Keys) {
    $v = $all[$k]
    if ($v -is [bool]) { if ($v) { $argList += "-$k" } }
    else { $argList += "-$k"; $argList += [string]$v }   # 空串也要原样传（RtsOpts 靠它表示“不传”）
  }

  $hostExe = Join-Path $PSHOME 'pwsh.exe'
  if (-not (Test-Path -LiteralPath $hostExe)) { $hostExe = Join-Path $PSHOME 'powershell.exe' }
  if (-not (Test-Path -LiteralPath $hostExe)) { $hostExe = 'powershell.exe' }

  Write-Host ''
  Write-Host ('[menu] {0}' -f (Get-StateLine)) -ForegroundColor DarkGray
  Write-Host ('[menu] & {0} {1}' -f $hostExe, ($argList -join ' ')) -ForegroundColor DarkGray
  Write-Host ''

  Push-Location -LiteralPath $script:root
  try {
    # Out-Host：子进程的输出直接上屏，不混进函数返回值（否则会把退出码冲掉）
    & $hostExe @argList 2>&1 | Out-Host
    $code = $LASTEXITCODE
  } finally {
    Pop-Location
  }
  Write-Host ''
  if ($code -eq 0) { Write-Host '[menu] 构建成功' -ForegroundColor Green }
  else { Write-Host "[menu] 构建失败（退出码 $code），修好后再试" -ForegroundColor Red }
  return $code
}

function Get-HostExe {
  $hostExe = Join-Path $PSHOME 'pwsh.exe'
  if (-not (Test-Path -LiteralPath $hostExe)) { $hostExe = Join-Path $PSHOME 'powershell.exe' }
  if (-not (Test-Path -LiteralPath $hostExe)) { $hostExe = 'powershell.exe' }
  return $hostExe
}

# --- 调用 build-dll.ps1（DLL：normal / compat / all） ----------------------

function Invoke-Dll([hashtable]$extra) {
  $argList = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', $script:dllScript)
  $all = @{
    Which    = $state.DllWhich
    Example  = $state.Example
    NoDeploy = $state.NoDeploy
    NoProbe  = $state.NoProbe
  }
  if ($state.Jobs -ge 0) { $all['Jobs'] = $state.Jobs }
  if ($extra) { foreach ($k in $extra.Keys) { $all[$k] = $extra[$k] } }

  foreach ($k in $all.Keys) {
    $v = $all[$k]
    if ($v -is [bool]) { if ($v) { $argList += "-$k" } }
    else { $argList += "-$k"; $argList += [string]$v }
  }

  $hostExe = Get-HostExe
  Write-Host ''
  Write-Host ('[menu] {0}' -f (Get-StateLine)) -ForegroundColor DarkGray
  Write-Host ('[menu] & {0} {1}' -f $hostExe, ($argList -join ' ')) -ForegroundColor DarkGray
  Write-Host ''

  Push-Location -LiteralPath $script:root
  try {
    & $hostExe @argList 2>&1 | Out-Host
    $code = $LASTEXITCODE
  } finally {
    Pop-Location
  }
  Write-Host ''
  if ($code -eq 0) { Write-Host '[menu] DLL 构建成功' -ForegroundColor Green }
  else { Write-Host "[menu] DLL 构建失败（退出码 $code），修好后再试" -ForegroundColor Red }
  return $code
}

# --- 调用 build-all.ps1（主程序 + 壳子 + DLL 一键全部构建） ----------------

function Invoke-All([hashtable]$extra) {
  $argList = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', $script:allScript)
  $all = @{
    Target   = 'all'
    Profile  = $state.Profile
    Llvm     = $state.Llvm
    RtsOpts  = $state.RtsOpts
    DllWhich = $state.DllWhich
    Example  = $state.Example
    NoDeploy = $state.NoDeploy
    NoProbe  = $state.NoProbe
  }
  if ($state.Jobs -ge 0) { $all['Jobs'] = $state.Jobs }
  if ($extra) { foreach ($k in $extra.Keys) { $all[$k] = $extra[$k] } }

  foreach ($k in $all.Keys) {
    $v = $all[$k]
    if ($v -is [bool]) { if ($v) { $argList += "-$k" } }
    else { $argList += "-$k"; $argList += [string]$v }
  }

  $hostExe = Get-HostExe
  Write-Host ''
  Write-Host ('[menu] {0}' -f (Get-StateLine)) -ForegroundColor DarkGray
  Write-Host ('[menu] & {0} {1}' -f $hostExe, ($argList -join ' ')) -ForegroundColor DarkGray
  Write-Host ''

  Push-Location -LiteralPath $script:root
  try {
    & $hostExe @argList 2>&1 | Out-Host
    $code = $LASTEXITCODE
  } finally {
    Pop-Location
  }
  Write-Host ''
  if ($code -eq 0) { Write-Host '[menu] 全部构建成功' -ForegroundColor Green }
  else { Write-Host "[menu] 全部构建失败（退出码 $code），修好后再试" -ForegroundColor Red }
  return $code
}

# --- 菜单 ---------------------------------------------------------------

function Show-Menu {
  Write-Host ''
  Write-Host '  expander2.haskell  交互式构建' -ForegroundColor Cyan
  Write-Host ('  {0}' -f (Get-StateLine)) -ForegroundColor DarkGray
  Write-Host '  ------------------------------------------------------------'
  Write-Host '   [1] 构建主程序            -> .build* \expander2-hs.exe'
  Write-Host '   [2] 构建交互式壳子        -> .build-shell*\expander2-shell.exe'
  Write-Host '   [3] 两个都建'
  Write-Host '   [4] 快速档构建主程序      (profile=fast，日常增量编译)'
  Write-Host '   [5] 兼容档构建主程序      (profile=compat，排错用)'
  Write-Host '   [6] 只打印 ghc 命令行     (-Info，不实际编译)'
  Write-Host '   [7] 清理构建目录          (.build* / build-dll*)'
  Write-Host '   [8] 一键全部构建          (主程序 + 壳子 + DLL)'
  Write-Host '   [9] 只构建 DLL            (normal / compat)'
  Write-Host '  ------------------------------------------------------------'
  Write-Host '   [p] 切换优化档位          [l] 切换 LLVM 后端'
  Write-Host '   [j] 设置并行度            [r] 设置 RTS 选项'
  Write-Host ('   [w] 选择 DLL 变体          (当前：{0})' -f $state.DllWhich)
  Write-Host ('   [x] 编译并运行 DLL 示例   (当前：{0})' -f (Get-ExampleText))
  $deployText = if ($state.NoDeploy) { '不发布' } else { '发布到 Release/Debug' }
  Write-Host ('   [d] 切换发布开关          (当前：{0})' -f $deployText)
  Write-Host '   [0] 退出'
  Write-Host ''
}

function Read-Choice([string]$prompt, [string]$default = '') {
  $tip = if ($default -ne '') { "（回车 = $default）" } else { '' }
  $v = (Read-Host ("{0}{1}" -f $prompt, $tip)).Trim()
  if ($v -eq '') { return $default }
  return $v
}

function Change-Profile {
  Write-Host ''
  $i = 1
  foreach ($n in $catalog.Names) {
    $mark = if ($n -eq $state.Profile) { '*' } else { ' ' }
    Write-Host ('   {0} {1}) {2,-8} {3}' -f $mark, $i, $n, (Get-ProfileDesc $n))
    $i++
  }
  $v = Read-Choice '档位编号或名字' $state.Profile
  $hit = $null
  if ($v -match '^\d+$') {
    $idx = [int]$v - 1
    if ($idx -ge 0 -and $idx -lt $catalog.Names.Count) { $hit = $catalog.Names[$idx] }
  } elseif ($catalog.Names -contains $v.ToLowerInvariant()) {
    $hit = $v.ToLowerInvariant()
  }
  if ($null -eq $hit) { Write-Host "[menu] 没有这个档位：$v" -ForegroundColor Yellow; return }
  $state.Profile = $hit
  Write-Host "[menu] 档位 = $hit" -ForegroundColor Green
}

function Change-Llvm {
  Write-Host ''
  Write-Host '   auto = PATH 里有 opt/llc 才用 LLVM'
  Write-Host '   on   = 强制 LLVM（缺 llc/opt 直接报错）'
  Write-Host '   off  = 只走内置 NCG 后端（最稳）'
  $v = (Read-Choice 'auto / on / off' $state.Llvm).ToLowerInvariant()
  if (@('auto', 'on', 'off') -notcontains $v) { Write-Host "[menu] 只能是 auto / on / off：$v" -ForegroundColor Yellow; return }
  $state.Llvm = $v
  Write-Host "[menu] LLVM = $v" -ForegroundColor Green
}

function Change-Jobs {
  $v = Read-Choice '并行度（正整数，0 = 自动）' (Get-JobsText)
  if ($v -eq '自动' -or $v -eq 'auto') { $state.Jobs = -1; Write-Host '[menu] 并行度 = 自动' -ForegroundColor Green; return }
  if ($v -notmatch '^\d+$') { Write-Host "[menu] 需要一个非负整数：$v" -ForegroundColor Yellow; return }
  $n = [int]$v
  $state.Jobs = if ($n -eq 0) { -1 } else { $n }
  Write-Host "[menu] 并行度 = $(Get-JobsText)" -ForegroundColor Green
}

function Change-Rts {
  $v = (Read-Choice 'RTS 选项（如 -A64m；输入 none 表示不传）' (Get-RtsText)).Trim()
  if ($v -in @('none', '-', 'off', '（不传）')) { $state.RtsOpts = '' }
  else { $state.RtsOpts = $v }
  Write-Host "[menu] RTS = $(Get-RtsText)" -ForegroundColor Green
}

function Change-DllWhich {
  Write-Host ''
  Write-Host '   all     = 两个变体都建（normal LLVM 版 + compat NCG 版）'
  Write-Host '   normal  = 只建 extreme + LLVM 的 expander2.dll'
  Write-Host '   compat  = 只建 -O2 + GHC NCG 的 expander2-compat.dll'
  $v = (Read-Choice 'all / normal / compat' $state.DllWhich).ToLowerInvariant()
  if (@('all', 'normal', 'compat') -notcontains $v) { Write-Host "[menu] 只能是 all / normal / compat：$v" -ForegroundColor Yellow; return }
  $state.DllWhich = $v
  Write-Host "[menu] DLL 变体 = $v" -ForegroundColor Green
}

function Pause-Menu {
  if (-not $NoPause) { [void](Read-Host '按回车返回菜单') }
}

# 带参数调用时：不显示菜单，按参数构建一次就退出
$hasArgs = ($PSBoundParameters.ContainsKey('Target') -or $PSBoundParameters.ContainsKey('Profile') -or
            $PSBoundParameters.ContainsKey('Llvm') -or $PSBoundParameters.ContainsKey('Jobs') -or
            $PSBoundParameters.ContainsKey('RtsOpts') -or
            $PSBoundParameters.ContainsKey('DllWhich') -or $Example -or $Info)
if ($hasArgs) {
  $extra = @{}
  if ($Info) { $extra['Info'] = $true }
  if ($Example) { exit (Invoke-All $extra) }
  if ($PSBoundParameters.ContainsKey('DllWhich')) { exit (Invoke-Dll $extra) }
  exit (Invoke-Build $extra)
}

if ([Console]::IsInputRedirected) {
  Write-Host '[err] 当前输入不是终端（stdin 被重定向），没法走菜单。' -ForegroundColor Red
  Write-Host '      直接带参数调用，例如：.\build-interactive.ps1 -Target all -Profile fast' -ForegroundColor Yellow
  exit 2
}

try {
  while ($true) {
    Show-Menu
    $choice = Read-Choice '选择' '1'
    switch ($choice.ToLowerInvariant()) {
      '1' { Invoke-Build @{} | Out-Null; Pause-Menu }
      '2' { Invoke-Build @{ Target = 'shell' } | Out-Null; Pause-Menu }
      '3' { Invoke-Build @{ Target = 'all' } | Out-Null; Pause-Menu }
      '4' { Invoke-Build @{ Target = 'main'; Profile = 'fast' } | Out-Null; Pause-Menu }
      '5' { Invoke-Build @{ Target = 'main'; Profile = 'compat' } | Out-Null; Pause-Menu }
      '6' { Invoke-Build @{ Info = $true } | Out-Null; Pause-Menu }
      '7' {
        Invoke-Build @{ Clean = $true } | Out-Null
        Write-Host '[menu] 已清理（.build* / build-cmake）' -ForegroundColor Green
        Pause-Menu
      }
      '8' { Invoke-All @{} | Out-Null; Pause-Menu }
      '9' { Invoke-Dll @{} | Out-Null; Pause-Menu }
      'p' { Change-Profile; Pause-Menu }
      'l' { Change-Llvm; Pause-Menu }
      'j' { Change-Jobs; Pause-Menu }
      'r' { Change-Rts; Pause-Menu }
      'w' { Change-DllWhich; Pause-Menu }
      'x' {
        $state.Example = -not $state.Example
        Write-Host "[menu] DLL C 示例 = $(Get-ExampleText)" -ForegroundColor Green
        Pause-Menu
      }
      'd' {
        $state.NoDeploy = -not $state.NoDeploy
        Write-Host "[menu] 发布 = $(if ($state.NoDeploy) { '关闭' } else { '开启' })" -ForegroundColor Green
        Pause-Menu
      }
      '0' { break }
      ''  { Invoke-Build @{} | Out-Null; Pause-Menu }
      default { Write-Host "[menu] 没有这个选项：$choice" -ForegroundColor Yellow; Pause-Menu }
    }
    if ($choice.ToLowerInvariant() -eq '0') { break }
  }
} finally {
  Write-Host ''
  Write-Host '[menu] 已退出' -ForegroundColor DarkGray
}
