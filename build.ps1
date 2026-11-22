<#
  expander2.haskell - PowerShell 构建入口（默认 profile = extreme，即最强劲档）

  用法：
    .\build.ps1                               # 主程序  -> .build\expander2-hs.exe
    .\build.ps1 -Target shell                 # 交互壳子 -> .build-shell\expander2-shell.exe
    .\build.ps1 -Target all                   # 两个都建
    .\build.ps1 -Profile strong               # 降档（日常增量编译更快）
    .\build.ps1 -Profile fast                 # 快档产物 -> .\Debug\
                                              # 默认 extreme 档产物 -> .\Release\
    .\build.ps1 -NoDeploy                     # 只编译，不往 Debug/Release 拷贝
    .\build.ps1 -Compat                       # 兼容模式：-O2 + 强制 GHC 内置 NCG 后端（不走 LLVM）
    .\build.ps1 -Jobs 6                       # 指定并行度（默认按 CPU/内存自动算）
    .\build.ps1 -Llvm on                      # 强制 LLVM 后端（PATH 里没有 llc/opt 直接报错）
    .\build.ps1 -Llvm off                     # 只走原生代码生成器 NCG
    .\build.ps1 -Info                         # 只打印将要执行的命令，不编译
    .\build.ps1 -Clean                        # 清掉 .build* / build-cmake 后退出
    .\build-interactive.ps1                   # 不想记参数就走菜单（内部调用本脚本）
#>
param(
  [ValidateSet('main', 'shell', 'all')]
  [string]$Target = 'main',
  [string]$Profile = '',
  [ValidateSet('auto', 'on', 'off')]
  [string]$Llvm = 'auto',
  [string]$RtsOpts = '-A64m',
  [int]$Jobs = 0,
  [string]$Ghc = '',
  [string[]]$ExtraFlags = @(),
  [switch]$Compat,
  [switch]$Clean,
  [switch]$Info,
  [switch]$NoProbe,
  [switch]$NoDeploy
)

$ErrorActionPreference = 'Stop'
$script:root = [System.IO.Path]::GetFullPath($PSScriptRoot).TrimEnd('\', '/')

# --- 小工具 ---------------------------------------------------------------

function Fail([string]$msg) {
  Write-Host ''
  Write-Host "[err] $msg" -ForegroundColor Red
  exit 1
}

function Resolve-Inside([string]$rel) {
  $full = [System.IO.Path]::GetFullPath((Join-Path $script:root $rel))
  if (-not $full.StartsWith($script:root + [System.IO.Path]::DirectorySeparatorChar, [System.StringComparison]::OrdinalIgnoreCase)) {
    Fail "拒绝操作仓库外的路径：$full"
  }
  return $full
}

function Get-LogTail([string]$text, [int]$lines = 30) {
  if ([string]::IsNullOrEmpty($text)) { return '' }
  $arr = $text -split "`r`n|`n"
  if ($arr.Count -le $lines) { return ($arr -join [Environment]::NewLine) }
  return (($arr | Select-Object -Last $lines) -join [Environment]::NewLine)
}

# 解析 ghc-flags.mk：DEFAULT_PROFILE / GHC_HOME / PROFILE_*
function Read-Config {
  $path = Join-Path $script:root 'ghc-flags.mk'
  if (-not (Test-Path -LiteralPath $path)) { Fail "缺少参数文件：$path" }
  $table = [ordered]@{}
  $default = 'extreme'
  $ghcHome = ''
  foreach ($raw in (Get-Content -LiteralPath $path -Encoding utf8)) {
    $line = $raw.Trim()
    if ($line.Length -eq 0 -or $line.StartsWith('#')) { continue }
    if ($line -match '^(PROFILE_[A-Z]+)\s*:=\s*(.*)$') {
      $table[$Matches[1].Substring(8).ToLowerInvariant()] = $Matches[2].Trim()
      continue
    }
    if ($line -match '^(DEFAULT_PROFILE)\s*:=\s*(.*)$') {
      $default = $Matches[2].Trim().ToLowerInvariant()
      continue
    }
    if ($line -match '^(GHC_HOME)\s*:=\s*(.*)$') {
      $ghcHome = $Matches[2].Trim()
    }
  }
  if ($table.Count -eq 0) { Fail "ghc-flags.mk 里没有解析到任何 PROFILE_* 定义" }
  return @{ Table = $table; Default = $default; GhcHome = $ghcHome }
}

# 默认并行度：内核数 - 1，且每个 job 预留约 1.5 GB 内存
# （-fexpose-all-unfoldings 的单模块内存开销明显高于 -O2，7~8 GB 机器开太满会被 OOM 杀掉）
function Get-AutoJobs {
  $cores = [Environment]::ProcessorCount
  $ramGB = 8.0
  try {
    $ramGB = (Get-CimInstance -ClassName Win32_ComputerSystem -ErrorAction Stop).TotalPhysicalMemory / 1GB
  } catch { }
  $byRam = [math]::Max(1, [int][math]::Floor($ramGB / 1.5))
  return [math]::Max(1, [math]::Min($cores - 1, $byRam))
}

# 用一组参数真编一个谁都会写的小程序：GHC 版本不支持某个 flag 时在这里暴露，而不是几十个模块之后。
function Test-FlagSet {
  param([string]$GhcExe, [string[]]$Flags)
  $tmp = Join-Path ([System.IO.Path]::GetTempPath()) ('ghc-probe-' + [guid]::NewGuid().ToString('N'))
  New-Item -ItemType Directory -Path $tmp -Force | Out-Null
  try {
    $src = Join-Path $tmp 'Probe.hs'
    $code = "module Main (main) where`n`nmain :: IO ()`nmain = print (sum [1 .. 10 :: Int])`n"
    [System.IO.File]::WriteAllText($src, $code, (New-Object System.Text.UTF8Encoding($false)))
    $exe = Join-Path $tmp 'flagprobe.exe'
    $argv = @($Flags)
    if ($argv -notcontains '--make') { $argv += '--make' }
    $argv += @('-outputdir', $tmp, '-o', $exe, $src)
    $log = (& $GhcExe @argv 2>&1 | Out-String)
    return @{ Ok = ($LASTEXITCODE -eq 0); Log = $log }
  } catch {
    return @{ Ok = $false; Log = $_.Exception.Message }
  } finally {
    Remove-Item -LiteralPath $tmp -Recurse -Force -ErrorAction SilentlyContinue
  }
}

# --- 解析参数 -------------------------------------------------------------

$defs = Read-Config
if ($Compat) { $Profile = 'compat' }
if ($Profile -eq '') { $Profile = $defs.Default }
$Profile = $Profile.ToLowerInvariant()
if (-not $defs.Table.Contains($Profile)) {
  Fail "未知 profile '$Profile'；ghc-flags.mk 里有：$(($defs.Table.Keys) -join ', ')"
}

# GHC：-Ghc 显式指定 > ghc-flags.mk 的 GHC_HOME > PATH
if ($Ghc -ne '') {
  if (-not (Test-Path -LiteralPath $Ghc)) { Fail "指定的 GHC 不存在：$Ghc" }
  $ghcPath = (Get-Item -LiteralPath $Ghc).FullName
} else {
  $candidate = ''
  if ($defs.GhcHome -ne '') {
    $homeWin = $defs.GhcHome.Replace('/', '\')
    $candidate = Join-Path $homeWin 'ghc.exe'
    if (-not (Test-Path -LiteralPath $candidate)) {
      $cand2 = Join-Path $homeWin 'ghc'
      if (Test-Path -LiteralPath $cand2) { $candidate = $cand2 } else { $candidate = '' }
    }
  }
  if ($candidate -ne '') {
    $ghcPath = (Get-Item -LiteralPath $candidate).FullName
  } else {
    $cmd = Get-Command 'ghc' -ErrorAction SilentlyContinue
    if (-not $cmd) {
      Fail ("找不到 GHC。`n" +
            "  * ghc-flags.mk 里的 GHC_HOME='$($defs.GhcHome)' 下没有 ghc.exe；`n" +
            "  * 也不在 PATH 里。用 -Ghc 'D:\path\to\ghc.exe' 指定一个。")
    }
    $ghcPath = $cmd.Source
  }
}

$ghcVersion = (& $ghcPath --version 2>&1 | Out-String).Trim()

$profileFlags = @($defs.Table[$Profile] -split '\s+') | Where-Object { $_.Length -gt 0 }

# 项目硬性要求：不许出现本机指令集相关的 native 优化（跨机器复制产物会 Illegal instruction）
foreach ($f in (@($profileFlags) + @($ExtraFlags))) {
  if ($f -match '^-(march|mcpu)=native' -or $f -match '(march|mcpu)=native') {
    Fail "按项目要求禁用 -march=native / -mcpu=native，但参数里出现了：$f"
  }
}

# LLVM 后端：auto 只在 llc + opt 都在 PATH 时才开
# （GHC 没有 -fno-llvm，默认后端本来就是 NCG；不传 -fllvm 就是强制 NCG 的最稳写法。
#  注意 flag 是 -fllvm：写成 -llvm 会被原样转给链接器当 -lvm 用，最后报
#  "lld: error: unable to find library -llvm"。）
$llvmFlags = @('-fllvm', '-optlo=-O3', '-optlc=-O3')
$useLlvm = $false

switch ($Llvm) {
  'off' { $useLlvm = $false }
  'on' {
    $hasLlc = $null -ne (Get-Command 'llc' -ErrorAction SilentlyContinue)
    $hasOpt = $null -ne (Get-Command 'opt' -ErrorAction SilentlyContinue)
    if (-not ($hasLlc -and $hasOpt)) {
      Fail "要求 -Llvm on，但 PATH 里缺 llc/opt（LLVM 后端需要二者）。装上 LLVM 或改用 -Llvm off。"
    }
    $useLlvm = $true
  }
  'auto' {
    $useLlvm = (($null -ne (Get-Command 'opt' -ErrorAction SilentlyContinue)) -and
                ($null -ne (Get-Command 'llc' -ErrorAction SilentlyContinue)))
    if ($useLlvm) { Write-Host '[info] PATH 里有 opt/llc，启用 LLVM 后端' }
    else { Write-Host '[info] PATH 里没有 opt/llc，跳过 LLVM 后端（要强制用 -Llvm on）' }
  }
}

if ($Target -eq 'all') { $targets = @('main', 'shell') } else { $targets = @($Target) }

# 兼容模式：只用 GHC 内置后端（NCG），LLVM 一律关闭，避免依赖 llc/opt 与外部工具链
if ($Profile -eq 'compat') {
  if ($useLlvm) { Write-Host '[info] compat 模式固定使用 GHC 内置 NCG 后端，已强制关闭 LLVM' }
  $useLlvm = $false
  $Llvm = 'off'
}

$suffix = ''
if ($Profile -ne $defs.Default) { $suffix = '-' + $Profile }

# --- 产物发布目录 ---------------------------------------------------------
# fast 档（日常快速编译）产物进 .\Debug，其余档（strong/extreme/compat）进 .\Release

$deployDirName = if ($Profile -eq 'fast') { 'Debug' } else { 'Release' }

function Publish-One([string]$file) {
  if ($NoDeploy) { return }
  $dstDir = Resolve-Inside $deployDirName
  if (-not (Test-Path -LiteralPath $dstDir)) { New-Item -ItemType Directory -Path $dstDir -Force | Out-Null }
  $dst = Join-Path $dstDir (Split-Path -Leaf $file)
  Copy-Item -LiteralPath $file -Destination $dst -Force
  Write-Host ('[deploy] {0} -> {1}' -f (Split-Path -Leaf $file), (Join-Path $deployDirName (Split-Path -Leaf $file)))
}

# --- 清理模式 -------------------------------------------------------------

if ($Clean) {
  $dirs = Get-ChildItem -LiteralPath $script:root -Force -Directory | Where-Object { $_.Name -like '.build*' -or $_.Name -eq 'build-cmake' }
  foreach ($d in $dirs) {
    $full = [System.IO.Path]::GetFullPath($d.FullName)
    if (-not $full.StartsWith($script:root + [System.IO.Path]::DirectorySeparatorChar, [System.StringComparison]::OrdinalIgnoreCase)) {
      Fail "拒绝删除仓库外的目录：$full"
    }
    Write-Host "[clean] $($d.Name)"
    Remove-Item -LiteralPath $full -Recurse -Force
  }
  exit 0
}

# --- 组装参数 -------------------------------------------------------------

if ($Jobs -eq 0) { $Jobs = Get-AutoJobs }

$baseFlags = @('-isrc', '-iapp', '-rtsopts')
if ($RtsOpts -ne '') { $baseFlags += "-with-rtsopts=$RtsOpts" }

$optFlags = @($profileFlags) + @($ExtraFlags)
if ($useLlvm) { $optFlags += $llvmFlags }

# 戳只记“会影响 .hi/.o / 链接结果”的东西：并行度 -j 不算（换 job 数不该触发全量重编）
$stampText = (($optFlags + @($baseFlags)) -join ' ')

$allFlags = @($optFlags) + $baseFlags + @("-j$Jobs", '--make')

# --- 参数可用性探针 -------------------------------------------------------

if (-not $NoProbe) {
  Write-Host '[probe] 校验这套参数在当前 GHC 上是否可用 ...'
  $probeFlags = @($allFlags | Where-Object { $_ -notmatch '^-j\d*$' })
  $r = Test-FlagSet -GhcExe $ghcPath -Flags $probeFlags
  if (-not $r.Ok) {
    Write-Host '[probe] 探针编译失败，GHC 说：'
    Write-Host (Get-LogTail $r.Log 25)
    if ($useLlvm -and ($Llvm -ne 'on')) {
      Write-Host '[probe] 丢掉 LLVM 参数，退回 NCG 重试'
      $useLlvm = $false
      $optFlags = @($profileFlags) + @($ExtraFlags)
      $stampText = (($optFlags + @($baseFlags)) -join ' ')
      $allFlags = @($optFlags) + $baseFlags + @("-j$Jobs", '--make')
      $probeFlags = @($allFlags | Where-Object { $_ -notmatch '^-j\d*$' })
      $r = Test-FlagSet -GhcExe $ghcPath -Flags $probeFlags
    }
  }
  if (-not $r.Ok) {
    if ($Llvm -eq 'on') { Fail "强制 LLVM 但参数跑不通（日志见上）：改用 -Llvm auto / -Llvm off" }
    Write-Host "[probe] profile '$Profile' 的额外参数不被当前 GHC 接受，退回 -O2"
    $profileFlags = @('-O2')
    $optFlags = @($profileFlags)
    $stampText = (($optFlags + @($baseFlags)) -join ' ')
    $allFlags = @($optFlags) + $baseFlags + @("-j$Jobs", '--make')
    $probeFlags = @($allFlags | Where-Object { $_ -notmatch '^-j\d*$' })
    $r = Test-FlagSet -GhcExe $ghcPath -Flags $probeFlags
    if (-not $r.Ok) {
      Write-Host (Get-LogTail $r.Log 25)
      Fail '连 -O2 都编不过：GHC 安装本身有问题（检查包数据库/临时目录权限）'
    }
  }
  Write-Host '[probe] ok'
}

# --- 构建 -----------------------------------------------------------------

function Build-One {
  param([string]$Kind, [string]$Suffix, [string[]]$Flags)

  if ($Kind -eq 'shell') {
    $dirName = ".build-shell$Suffix"
    $srcRel = 'app\Shell.hs'
    $exeName = 'expander2-shell.exe'
  } else {
    $dirName = ".build$Suffix"
    $srcRel = 'app\Main.hs'
    $exeName = 'expander2-hs.exe'
  }

  $outDir = Resolve-Inside $dirName
  $exe = Join-Path $outDir $exeName

  # .hi/.o 与参数强相关：参数变了、或来源不明的旧产物，一律先清掉再编
  if (Test-Path -LiteralPath $outDir) {
    $stampPath = Join-Path $outDir '.ghcflags'
    $old = ''
    if (Test-Path -LiteralPath $stampPath) { $old = (Get-Content -LiteralPath $stampPath -Raw -Encoding utf8).Trim() }
    if ($old -ne $stampText) {
      if ($old -eq '') { Write-Host "[clean] $dirName 里没有参数戳（旧产物），清掉重编" }
      else { Write-Host "[clean] 参数变了，清掉 $dirName 后重编" }
      Remove-Item -LiteralPath $outDir -Recurse -Force
    }
  }
  if (-not (Test-Path -LiteralPath $outDir)) { New-Item -ItemType Directory -Path $outDir | Out-Null }
  [System.IO.File]::WriteAllText(
    (Join-Path $outDir '.ghcflags'),
    $stampText,
    (New-Object System.Text.UTF8Encoding($false)))

  $ghcArgs = @($Flags) + @('-outputdir', $outDir, '-o', $exe, (Join-Path $script:root $srcRel))
  $cmdLine = ($ghcArgs | ForEach-Object { if ($_ -match '[ ]') { '"{0}"' -f $_ } else { $_ } }) -join ' '

  Write-Host "[build] $cmdLine"
  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  # -isrc / -iapp 是相对路径，必须在仓库根目录下调用 ghc（用户从别的目录执行本脚本也能编）
  Push-Location -LiteralPath $script:root
  try {
    & $ghcPath @ghcArgs 2>&1 | ForEach-Object { Write-Host $_ }
    $exit = $LASTEXITCODE
  } finally {
    Pop-Location
  }
  $sw.Stop()
  if ($exit -ne 0) { Fail "ghc 退出码 $exit（命令行见上）" }
  if (-not (Test-Path -LiteralPath $exe)) { Fail "没有产出 $exe" }

  Publish-One -file $exe

  $mb = (Get-Item -LiteralPath $exe).Length / 1MB
  Write-Host ('[done] {0}  {1:N2} MB  {2:N1} s' -f $exe, $mb, $sw.Elapsed.TotalSeconds)
}

if ($Info) {
  Write-Host ('GHC      : {0}' -f $ghcVersion)
  Write-Host ('ghc path : {0}' -f $ghcPath)
  Write-Host ('profile  : {0}{1}' -f $Profile, $(if ($Profile -eq $defs.Default) { '（默认）' } else { '' }))
  Write-Host ('flags    : {0}' -f ($allFlags -join ' '))
  Write-Host ('llvm     : {0}' -f $(if ($useLlvm) { 'on' } else { 'off（NCG）' }))
  Write-Host ('outdir   : {0}' -f $(if ($targets -contains 'shell') { ".build-shell$suffix" } else { ".build$suffix" }))
  Write-Host ('jobs     : {0}' -f $Jobs)
  Write-Host ('rtsopts  : {0}' -f $RtsOpts)
  exit 0
}

Write-Host ('[ghc] {0}' -f $ghcVersion)
foreach ($t in $targets) { Build-One -Kind $t -Suffix $suffix -Flags $allFlags }
Write-Host "[ok] profile=$Profile llvm=$(if ($useLlvm) { 'on' } else { 'off' }) jobs=$Jobs"
