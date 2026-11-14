# Windows 构建脚本：产物 .build\expander2-hs.exe
param(
  [string]$Ghc = "ghc"
)
$ErrorActionPreference = "Stop"
Push-Location $PSScriptRoot
try {
  New-Item -ItemType Directory -Force -Path ".build" | Out-Null
  & $Ghc -O1 -isrc -iapp -outputdir .build -o .build\expander2-hs.exe app\Main.hs
  if ($LASTEXITCODE -ne 0) { throw "ghc 编译失败（exit $LASTEXITCODE）" }
  Write-Host "built: .build\expander2-hs.exe"
} finally {
  Pop-Location
}
