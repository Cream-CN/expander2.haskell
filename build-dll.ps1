<#
  expander2.haskell - DLL 构建入口（两个产物，一套 ABI）

    .\build-dll.ps1                     # 两个都建：
                                        #   正常版  -> .build-dll\expander2.dll        (extreme + LLVM 后端)
                                        #   兼容版  -> .build-dll-compat\expander2-compat.dll (compat = -O2 + GHC NCG 后端)
    .\build-dll.ps1 -Which normal       # 只建 LLVM 版
    .\build-dll.ps1 -Which compat       # 只建 GHC NCG 版
    .\build-dll.ps1 -Example            # 建完后再编译并跑一遍 C 示例（dll\example）
    .\build-dll.ps1 -Info               # 只打印将要执行的命令
    .\build-dll.ps1 -Clean              # 清掉 .build-dll* 后退出
    .\build-dll.ps1 -NoDeploy           # 只构建，不往 .\Release\ 拷贝

  构建成功后产物（dll / 导入库 .a / expander2.h / example-*.exe）会直接拷贝到 .\Release\。
#>
param(
  [ValidateSet('all', 'normal', 'compat')]
  [string]$Which = 'all',
  [string]$Ghc = '',
  [int]$Jobs = 0,
  [switch]$NoThreaded,
  [switch]$NoProbe,
  [switch]$Example,
  [switch]$Clean,
  [switch]$Info,
  [switch]$NoDeploy
)

$ErrorActionPreference = 'Stop'
$script:root = [System.IO.Path]::GetFullPath($PSScriptRoot).TrimEnd('\', '/')

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

# 与 build.ps1 同一份解析口径：DEFAULT_PROFILE / GHC_HOME / PROFILE_*
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

function Get-AutoJobs {
  $cores = [Environment]::ProcessorCount
  $ramGB = 8.0
  try {
    $ramGB = (Get-CimInstance -ClassName Win32_ComputerSystem -ErrorAction Stop).TotalPhysicalMemory / 1GB
  } catch { }
  $byRam = [math]::Max(1, [int][math]::Floor($ramGB / 1.5))
  return [math]::Max(1, [math]::Min($cores - 1, $byRam))
}

# 用一组参数真编一个谁都会写的小模块，GHC 不支持某个 flag 时在这里暴露
function Test-FlagSet {
  param([string]$GhcExe, [string[]]$Flags)
  $tmp = Join-Path ([System.IO.Path]::GetTempPath()) ('ghc-probe-' + [guid]::NewGuid().ToString('N'))
  New-Item -ItemType Directory -Path $tmp -Force | Out-Null
  try {
    $src = Join-Path $tmp 'Main.hs'
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

# 找 objdump：PATH 里没有就在常见位置找一个（离开交互 shell 的 PATH 常常不含 msys64）
function Find-Objdump([string]$GhcExe) {
  $cmd = Get-Command 'objdump' -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }
  $cands = New-Object System.Collections.Generic.List[string]
  if ($env:MSYS64) {
    $cands.Add((Join-Path $env:MSYS64 'ucrt64\bin\objdump.exe'))
    $cands.Add((Join-Path $env:MSYS64 'usr\bin\objdump.exe'))
  }
  foreach ($p in @('D:\msys64\ucrt64\bin\objdump.exe', 'C:\msys64\ucrt64\bin\objdump.exe',
                   'D:\msys64\mingw64\bin\objdump.exe')) {
    $cands.Add($p)
  }
  if ($GhcExe) {
    $ghcBin = Split-Path -Parent $GhcExe
    $cands.Add((Join-Path $ghcBin 'objdump.exe'))
    $cands.Add((Join-Path (Split-Path -Parent $ghcBin) 'mingw\bin\objdump.exe'))
  }
  foreach ($c in $cands) {
    if ($c -and (Test-Path -LiteralPath $c)) { return $c }
  }
  return ''
}

# 找 C 编译器（编示例用）：优先 GHC 自带 mingw gcc（与 DLL 同一套工具链），再 PATH，再 msys64
function Find-Compiler([string]$GhcExe) {
  $cands = New-Object System.Collections.Generic.List[string]
  if ($GhcExe) {
    $ghcBin = Split-Path -Parent $GhcExe
    $cands.Add((Join-Path (Split-Path -Parent $ghcBin) 'mingw\bin\gcc.exe'))
    $cands.Add((Join-Path $ghcBin 'gcc.exe'))
  }
  if ($env:MSYS64) {
    $cands.Add((Join-Path $env:MSYS64 'ucrt64\bin\clang.exe'))
    $cands.Add((Join-Path $env:MSYS64 'ucrt64\bin\gcc.exe'))
  }
  foreach ($p in @('D:\msys64\ucrt64\bin\clang.exe', 'C:\msys64\ucrt64\bin\clang.exe',
                   'D:\msys64\ucrt64\bin\gcc.exe', 'C:\msys64\ucrt64\bin\gcc.exe')) {
    $cands.Add($p)
  }
  foreach ($c in $cands) {
    if ($c -and (Test-Path -LiteralPath $c)) { return $c }
  }
  foreach ($n in @('clang', 'gcc')) {
    $cmd = Get-Command $n -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }
  }
  return ''
}

# 从 PE 导出表里取符号名（找不到 objdump 就返回 $null，调用方跳过校验）
function Get-DllExports {
  param([string]$DllPath, [string]$Objdump)
  if (-not $Objdump) { return $null }
  $dump = (& $Objdump -p $DllPath 2>&1) | Out-String
  $lines = $dump -split "`r`n|`n"
  $start = -1
  for ($i = 0; $i -lt $lines.Count; $i++) {
    if ($lines[$i] -match 'Ordinal/Name Pointer\] Table --') { $start = $i }
  }
  if ($start -lt 0) { return @() }
  $names = New-Object System.Collections.Generic.List[string]
  for ($i = $start + 1; $i -lt $lines.Count; $i++) {
    if ($lines[$i] -match '^[ \t]+\[[ \t]*\d+\][ \t]+\+base\[[ \t]*\d+\][ \t]+[\da-fA-F]+[ \t]+([^ \t]+)$') {
      $names.Add($Matches[1])
    } elseif ($lines[$i].Trim().Length -eq 0) {
      break
    }
  }
  return $names
}

# ---------------------------------------------------------------------------

$defs = Read-Config

if ($Ghc -ne '') {
  if (-not (Test-Path -LiteralPath $Ghc)) { Fail "指定的 GHC 不存在：$Ghc" }
  $ghcPath = (Get-Item -LiteralPath $Ghc).FullName
} else {
  $candidate = ''
  if ($defs.GhcHome -ne '') {
    $homeWin = $defs.GhcHome.Replace('/', '\')
    foreach ($nm in @('ghc.exe', 'ghc')) {
      $p = Join-Path $homeWin $nm
      if (Test-Path -LiteralPath $p) { $candidate = $p; break }
    }
  }
  if ($candidate -ne '') {
    $ghcPath = (Get-Item -LiteralPath $candidate).FullName
  } else {
    $cmd = Get-Command 'ghc' -ErrorAction SilentlyContinue
    if (-not $cmd) { Fail "找不到 GHC；用 -Ghc 'D:\path\to\ghc.exe' 指定一个。" }
    $ghcPath = $cmd.Source
  }
}
$ghcVersion = (& $ghcPath --version 2>&1 | Out-String).Trim()

if ($Jobs -eq 0) { $Jobs = Get-AutoJobs }

$objdumpPath = Find-Objdump $ghcPath

$baseFlags = @('-isrc')
# 注意：GHC 对 -shared 明确忽略 -rtsopts / -with-rtsopts（运行时提示
# "-rtsopts and -with-rtsopts have no effect with -shared"），所以这里不传；
# DLL 的 RTS 参数请在宿主里用 GHCRTS 环境变量或 hs_init_ghc 设置。
if (-not $NoThreaded) { $baseFlags += '-threaded' }

# 目录：正常版 .build-dll，兼容版 .build-dll-compat
$genDir = Resolve-Inside '.build-dll\gen'
$genObjDir = Resolve-Inside '.build-dll\genobj'
$genExe = Join-Path $genDir 'gen-dll.exe'

$variants = @()
if ($Which -eq 'all' -or $Which -eq 'normal') {
  $variants += @{
    Name = 'normal'
    Dll = 'expander2.dll'
    Dir = (Resolve-Inside '.build-dll')
    Obj = (Resolve-Inside '.build-dll\obj')
    Profile = $defs.Default
    Llvm = $true
  }
}
if ($Which -eq 'all' -or $Which -eq 'compat') {
  $variants += @{
    Name = 'compat'
    Dll = 'expander2-compat.dll'
    Dir = (Resolve-Inside '.build-dll-compat')
    Obj = (Resolve-Inside '.build-dll-compat\obj')
    Profile = 'compat'
    Llvm = $false
  }
}

foreach ($v in $variants) {
  if (-not $defs.Table.Contains($v.Profile)) {
    Fail "未知 profile '$($v.Profile)'；ghc-flags.mk 里有：$(($defs.Table.Keys) -join ', ')"
  }
  $pf = @($defs.Table[$v.Profile] -split '\s+') | Where-Object { $_.Length -gt 0 }
  $v.Flags = $pf
}

# --- 清理 -----------------------------------------------------------------

if ($Clean) {
  $dirs = Get-ChildItem -LiteralPath $script:root -Force -Directory | Where-Object { $_.Name -like '.build-dll*' }
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

# --- 组装命令行 ------------------------------------------------------------

function Get-LinkFlags([hashtable]$v) {
  $opt = @($v.Flags)
  if ($v.Llvm) { $opt += @('-fllvm', '-optlo=-O3', '-optlc=-O3', '-DEXPANDER2_LLVM_BACKEND') }
  return $opt
}

$generatedHs = Join-Path $genDir 'Expander2Exports.hs'
$symbolsTxt = Join-Path $genDir 'symbols.txt'
$plan = @()
foreach ($v in $variants) {
  $flags = @(Get-LinkFlags $v)
  $plan += [pscustomobject]@{
    Name = $v.Name
    Profile = $v.Profile
    Llvm = $v.Llvm
    Flags = $flags
    Dll = (Join-Path $v.Dir $v.Dll)
    Obj = $v.Obj
    Dir = $v.Dir
  }
}

if ($Info) {
  Write-Host ('GHC      : {0}' -f $ghcVersion)
  Write-Host ('ghc path : {0}' -f $ghcPath)
  Write-Host ('gen dir  : {0}' -f $genDir)
  Write-Host ('jobs     : {0}' -f $Jobs)
  Write-Host ('threaded : {0}' -f (-not $NoThreaded))
  if ($objdumpPath) { Write-Host ('objdump  : {0}' -f $objdumpPath) }
  else { Write-Host 'objdump  : (没找到，导出表校验会跳过)' }
  foreach ($p in $plan) {
    Write-Host ''
    Write-Host ('[{0}] profile={1} llvm={2}' -f $p.Name, $p.Profile, $p.Llvm)
    Write-Host ('  out  : {0}' -f $p.Dll)
    Write-Host ('  flags: {0}' -f ($p.Flags -join ' '))
  }
  exit 0
}

# --- 目录 -----------------------------------------------------------------

foreach ($d in @($genDir, $genObjDir)) {
  if (-not (Test-Path -LiteralPath $d)) { New-Item -ItemType Directory -Path $d -Force | Out-Null }
}
foreach ($p in $plan) {
  foreach ($d in @($p.Dir, $p.Obj)) {
    if (-not (Test-Path -LiteralPath $d)) { New-Item -ItemType Directory -Path $d -Force | Out-Null }
  }
}

# --- 生成器 ---------------------------------------------------------------

function Test-GenNeeded {
  if (-not (Test-Path -LiteralPath $genExe)) { return $true }
  $exeTime = (Get-Item -LiteralPath $genExe).LastWriteTimeUtc
  $srcs = @(Get-ChildItem -LiteralPath (Join-Path $script:root 'src') -Recurse -File -Filter '*.hs') +
          @(Get-Item -LiteralPath (Join-Path $script:root 'tools\GenDll.hs'))
  foreach ($s in $srcs) {
    if ($s.LastWriteTimeUtc -gt $exeTime) { return $true }
  }
  return $false
}

if (Test-GenNeeded) {
  Write-Host '[gen] 编译导出名生成器 tools/GenDll.hs ...'
  $genArgs = @('-O0', '-w', '-isrc', '-itools', '-outputdir', $genObjDir, '-o', $genExe,
               (Join-Path $script:root 'tools\GenDll.hs'))
  Push-Location -LiteralPath $script:root
  try {
    & $ghcPath @genArgs 2>&1 | ForEach-Object { Write-Host $_ }
    $exit = $LASTEXITCODE
  } finally {
    Pop-Location
  }
  if ($exit -ne 0) { Fail "生成器编译失败（退出码 $exit）" }
} else {
  Write-Host '[gen] 生成器是最新的，跳过编译'
}

Write-Host '[gen] 生成 Expander2Exports.hs / expander2.h / symbols.txt ...'
& $genExe ($genDir + '\') 2>&1 | ForEach-Object { Write-Host $_ }
if ($LASTEXITCODE -ne 0) { Fail '生成器运行失败' }
foreach ($f in @($generatedHs, (Join-Path $genDir 'expander2.h'), $symbolsTxt)) {
  if (-not (Test-Path -LiteralPath $f)) { Fail "生成器没有产出 $f" }
}

# 头文件发布到仓库里（用户拿到 DLL 就该有头文件），并复制到每个产物目录
$pubHeader = Resolve-Inside 'dll\expander2.h'
$pubHeaderParent = Split-Path -Parent $pubHeader
if (-not (Test-Path -LiteralPath $pubHeaderParent)) { New-Item -ItemType Directory -Path $pubHeaderParent -Force | Out-Null }
Copy-Item -LiteralPath (Join-Path $genDir 'expander2.h') -Destination $pubHeader -Force
foreach ($p in $plan) {
  Copy-Item -LiteralPath (Join-Path $genDir 'expander2.h') -Destination (Join-Path $p.Dir 'expander2.h') -Force
}

# --- .def：精确导出列表 ---------------------------------------------------

$syms = @(Get-Content -LiteralPath $symbolsTxt -Encoding utf8 | Where-Object { $_.Trim().Length -gt 0 })
if ($syms.Count -eq 0) { Fail 'symbols.txt 为空' }
# 每个 DLL 的 LIBRARY 名不同（PE 里的内部名），其余一致
$defTemplate = "EXPORTS`r`n" + (($syms | ForEach-Object { "  $_" }) -join "`r`n") + "`r`n"

function Write-Def([string]$path, [string]$libName) {
  $content = "LIBRARY $libName`r`n" + $defTemplate
  [System.IO.File]::WriteAllText($path, $content, (New-Object System.Text.UTF8Encoding($false)))
}

# --- 逐个构建 -------------------------------------------------------------

foreach ($p in $plan) {
  $defForDll = $p.Dll -replace '\.dll$', '.def'
  $libName = [System.IO.Path]::GetFileNameWithoutExtension($p.Dll)
  Write-Def -path $defForDll -libName $libName

  $stampText = (($p.Flags + $baseFlags) -join ' ')
  $stampPath = Join-Path $p.Obj '.ghcflags'
  $old = ''
  if (Test-Path -LiteralPath $stampPath) { $old = (Get-Content -LiteralPath $stampPath -Raw -Encoding utf8).Trim() }
  if ((Test-Path -LiteralPath $p.Obj) -and $old -ne $stampText) {
    Write-Host "[clean] $($p.Name)：参数变了，清掉 obj 后重编"
    Remove-Item -LiteralPath $p.Obj -Recurse -Force
    New-Item -ItemType Directory -Path $p.Obj -Force | Out-Null
  }
  [System.IO.File]::WriteAllText($stampPath, $stampText, (New-Object System.Text.UTF8Encoding($false)))

  $ghcArgs = @($p.Flags) + $baseFlags + @(
    "-i$genDir",
    "-j$Jobs",
    '--make',
    '-shared',
    '-o', $p.Dll,
    '-outputdir', $p.Obj,
    ('-optl-Wl,' + $defForDll),
    $generatedHs
  )
  $cmdLine = ($ghcArgs | ForEach-Object { if ($_ -match '[ ]') { '"{0}"' -f $_ } else { $_ } }) -join ' '

  if (-not $NoProbe) {
    Write-Host "[probe] $($p.Name)：校验这套参数是否被当前 GHC 接受 ..."
    $probeFlags = @($p.Flags) + @('-isrc', '-threaded') | Where-Object { $_ -notmatch '^\s*$' }
    # -D 也要一起验（CPP 宏写错会在真正编译时才炸）
    $r = Test-FlagSet -GhcExe $ghcPath -Flags ($probeFlags | Where-Object { $_ -notmatch '^-i' })
    if (-not $r.Ok) {
      Write-Host (Get-LogTail $r.Log 20)
      Fail "$($p.Name) 档的参数在当前 GHC 上跑不通（日志见上）"
    }
    Write-Host '[probe] ok'
  }

  Write-Host ''
  Write-Host "[build] $($p.Name) -> $($p.Dll)"
  Write-Host "[build] $cmdLine"
  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  Push-Location -LiteralPath $script:root
  try {
    & $ghcPath @ghcArgs 2>&1 | ForEach-Object { Write-Host $_ }
    $exit = $LASTEXITCODE
  } finally {
    Pop-Location
  }
  $sw.Stop()
  if ($exit -ne 0) { Fail "ghc 退出码 $exit（$($p.Name)）" }
  if (-not (Test-Path -LiteralPath $p.Dll)) { Fail "没有产出 $($p.Dll)" }

  # 导出表校验：.def 生效了吗？全是我们要的名字吗？
  $exports = Get-DllExports -DllPath $p.Dll -Objdump $objdumpPath
  if ($null -eq $exports) {
    Write-Host '[check] 没有 objdump，跳过导出表校验'
  } else {
    $got = @($exports | Sort-Object)
    $want = @($syms | Sort-Object)
    $missing = @($want | Where-Object { $got -notcontains $_ })
    $extra = @($got | Where-Object { $want -notcontains $_ })
    if ($missing.Count -gt 0) { Fail "导出表缺符号（前几个）：$(($missing | Select-Object -First 5) -join ', ')" }
    if ($extra.Count -gt 0) { Fail "导出表多出符号（前几个）：$(($extra | Select-Object -First 5) -join ', ')" }
    Write-Host ('[check] 导出表 OK：{0} 个符号，与 symbols.txt 完全一致' -f $got.Count)
  }

  $mb = (Get-Item -LiteralPath $p.Dll).Length / 1MB
  Write-Host ('[done] {0}  {1:N2} MB  {2:N1} s' -f $p.Dll, $mb, $sw.Elapsed.TotalSeconds)
  if (Test-Path -LiteralPath ($p.Dll + '.a')) {
    Write-Host ('[done] {0}.a（导入库，给 C 链接用）' -f $p.Dll)
  }
}

Write-Host ''
Write-Host '[ok] DLL 构建完成'

# --- C 示例 ---------------------------------------------------------------

if ($Example) {
  $exSrc = Resolve-Inside 'dll\example\expander2_example.c'
  foreach ($p in $plan) {
    $incDir = $p.Dir
    $exe = Join-Path $p.Dir ('example-' + $p.Name + '.exe')
    $libName = [System.IO.Path]::GetFileNameWithoutExtension($p.Dll)
    $cc = Find-Compiler $ghcPath
    if (-not $cc) { Fail '编译 C 示例需要 clang 或 gcc（PATH 和常见位置里都没找到）' }
    $ccArgs = @('-O1', '-o', $exe, $exSrc, '-I', $incDir, '-L', $incDir, ('-l' + $libName))
    $cmdLine = ($ccArgs | ForEach-Object { if ($_ -match '[ ]') { '"{0}"' -f $_ } else { $_ } }) -join ' '
    Write-Host "[example] cc = $cc"
    Write-Host "[example] $cmdLine"
    Push-Location -LiteralPath $incDir
    try {
      & $cc @ccArgs 2>&1 | ForEach-Object { Write-Host $_ }
      $ccExit = $LASTEXITCODE
    } finally {
      Pop-Location
    }
    if ($ccExit -ne 0) { Fail "C 示例编译失败（$($p.Name)）" }
    Write-Host "[example] 运行 $exe"
    Push-Location -LiteralPath $incDir
    try {
      & $exe 2>&1 | ForEach-Object { Write-Host $_ }
      $runExit = $LASTEXITCODE
    } finally {
      Pop-Location
    }
    if ($runExit -ne 0) { Fail "C 示例运行失败（$($p.Name)，退出码 $runExit）" }
  }
}

# --- 发布到 .\Release -----------------------------------------------------

if (-not $NoDeploy) {
  $deployDir = Resolve-Inside 'Release'
  if (-not (Test-Path -LiteralPath $deployDir)) { New-Item -ItemType Directory -Path $deployDir -Force | Out-Null }
  foreach ($p in $plan) {
    $files = @(
      $p.Dll,
      ($p.Dll + '.a'),
      (Join-Path $p.Dir 'expander2.h'),
      (Join-Path $p.Dir ('example-' + $p.Name + '.exe'))
    )
    foreach ($f in $files) {
      if (Test-Path -LiteralPath $f) {
        $leaf = Split-Path -Leaf $f
        Copy-Item -LiteralPath $f -Destination (Join-Path $deployDir $leaf) -Force
        Write-Host ('[deploy] {0} -> Release\{0}' -f $leaf)
      }
    }
  }
}
