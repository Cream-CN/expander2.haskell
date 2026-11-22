<#
  expander2.haskell - 一键全部构建入口（主程序 + 交互式壳子 + DLL）

    .\build-all.ps1                  # 主程序 + 壳子 + 两个 DLL 变体（normal/compat）
    .\build-all.ps1 -SkipExe          # 只建 DLL（等价 .\build-dll.ps1）
    .\build-all.ps1 -SkipDll          # 只建主程序 + 壳子（等价 .\build.ps1 -Target all）
    .\build-all.ps1 -DllWhich normal  # DLL 只建 LLVM 正常版
    .\build-all.ps1 -DllWhich compat  # DLL 只建 NCG 兼容版
    .\build-all.ps1 -Example          # DLL 建完后编译并运行 dll\example 的 C 示例
    .\build-all.ps1 -Profile fast     # 透传给主程序/壳子；DLL 变体档位由 build-dll.ps1 内部定
    .\build-all.ps1 -Llvm off         # 主程序/壳子关闭 LLVM（DLL 变体后端仍由 build-dll.ps1 定）
    .\build-all.ps1 -Jobs 4           # 手动指定并行度（0 = 各脚本自动算）
    .\build-all.ps1 -Info             # 两段都只打印将要执行的命令行，不实际编译
    .\build-all.ps1 -Clean            # 清掉 .build* 与 .build-dll* 后退出
    .\build-all.ps1 -NoDeploy          # 只编译，不往 .\Release\ 拷贝
#>
param(
  [ValidateSet('main', 'shell', 'all')]
  [string]$Target = 'all',
  [string]$Profile = '',
  [ValidateSet('auto', 'on', 'off')]
  [string]$Llvm = 'auto',
  [string]$RtsOpts = '-A64m',
  [int]$Jobs = 0,
  [string]$Ghc = '',
  [ValidateSet('all', 'normal', 'compat')]
  [string]$DllWhich = 'all',
  [switch]$Example,
  [switch]$NoThreaded,
  [switch]$SkipExe,
  [switch]$SkipDll,
  [switch]$NoProbe,
  [switch]$Clean,
  [switch]$Info,
  [switch]$NoDeploy
)

$ErrorActionPreference = 'Stop'
$script:root = [System.IO.Path]::GetFullPath($PSScriptRoot).TrimEnd('\', '/')
$script:ps1 = Join-Path $script:root 'build.ps1'
$script:dllPs1 = Join-Path $script:root 'build-dll.ps1'

function Fail([string]$msg) {
  Write-Host ''
  Write-Host "[err] $msg" -ForegroundColor Red
  exit 1
}

foreach ($s in @($script:ps1, $script:dllPs1)) {
  if (-not (Test-Path -LiteralPath $s)) { Fail "缺少构建脚本：$s" }
}

if ($SkipExe -and $SkipDll) { Fail "-SkipExe 和 -SkipDll 不能同时给，那样没有任何产物要构建" }

# 把子脚本当子进程跑：失败码向上透传，子脚本里的 exit 不会带走本脚本
# 注意：直接调 .ps1 必须用哈希表 splat（具名参数），数组 splat 会把
# "-Target" 之类的开关当成位置参数，触发 ValidateSet 校验失败。
function Invoke-BuildScript {
  param([string]$Script, [hashtable]$ParamHash, [string]$Label)
  Write-Host ''
  Write-Host ('==== [{0}] {1} ====' -f $Label, (Split-Path -Leaf $Script)) -ForegroundColor Cyan
  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  # 子脚本正常时会用 exit 留下退出码；万一它抛出终止错误（例如参数校验失败），
  # 这里接住，转成退出码往上走，而不是让整个 all 构建被异常带崩。
  $code = 1
  try {
    & $Script @ParamHash
    $code = $LASTEXITCODE
  } catch {
    Write-Host ''
    Write-Host ("[err] $Label 抛出异常：" + $_.Exception.Message) -ForegroundColor Red
    $code = 1
  }
  $sw.Stop()
  Write-Host ('==== [{0}] 退出码 {1}，用时 {2:N1} s ====' -f $Label, $code, $sw.Elapsed.TotalSeconds) -ForegroundColor DarkGray
  if ($code -ne 0) { Fail "$Label 构建失败（退出码 $code）" }
}

# --- 清理：两段目录一起清 ---------------------------------------------------

if ($Clean) {
  Write-Host '[clean] 清理 exe / 壳子构建目录 ...'
  & $script:ps1 @{ Clean = $true }
  if ($LASTEXITCODE -ne 0) { Fail 'build.ps1 -Clean 失败' }
  Write-Host '[clean] 清理 DLL 构建目录 ...'
  & $script:dllPs1 @{ Clean = $true }
  if ($LASTEXITCODE -ne 0) { Fail 'build-dll.ps1 -Clean 失败' }
  exit 0
}

# --- 组装 exe / 壳子参数（透传给 build.ps1） -------------------------------

$exeParams = @{ Target = $Target; Llvm = $Llvm; RtsOpts = $RtsOpts }
if ($Profile -ne '') { $exeParams['Profile'] = $Profile }
if ($Jobs -gt 0) { $exeParams['Jobs'] = $Jobs }
if ($Ghc -ne '') { $exeParams['Ghc'] = $Ghc }
if ($NoProbe) { $exeParams['NoProbe'] = $true }
if ($Info) { $exeParams['Info'] = $true }
if ($NoDeploy) { $exeParams['NoDeploy'] = $true }

# --- 组装 DLL 参数（透传给 build-dll.ps1） ---------------------------------

$dllParams = @{ Which = $DllWhich }
if ($Jobs -gt 0) { $dllParams['Jobs'] = $Jobs }
if ($Ghc -ne '') { $dllParams['Ghc'] = $Ghc }
if ($NoThreaded) { $dllParams['NoThreaded'] = $true }
if ($Example) { $dllParams['Example'] = $true }
if ($NoProbe) { $dllParams['NoProbe'] = $true }
if ($Info) { $dllParams['Info'] = $true }
if ($NoDeploy) { $dllParams['NoDeploy'] = $true }

# --- 依次构建 ---------------------------------------------------------------

$totalSw = [System.Diagnostics.Stopwatch]::StartNew()

if (-not $SkipExe) {
  Invoke-BuildScript -Script $script:ps1 -ParamHash $exeParams -Label 'exe/shell'
} else {
  Write-Host '[all] 已跳过 exe / 壳子构建（-SkipExe）'
}

if (-not $SkipDll) {
  Invoke-BuildScript -Script $script:dllPs1 -ParamHash $dllParams -Label 'dll'
} else {
  Write-Host '[all] 已跳过 DLL 构建（-SkipDll）'
}

$totalSw.Stop()
Write-Host ''
Write-Host ('[ok] 全部构建完成，总用时 {0:N1} s' -f $totalSw.Elapsed.TotalSeconds) -ForegroundColor Green
