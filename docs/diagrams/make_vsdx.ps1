param(
    [Parameter(Mandatory = $true)][string[]]$Dirs,
    [string]$OutName = "diagram.vsdx",
    [switch]$Verify
)

$ErrorActionPreference = "Stop"
trap {
    "ERR line $($_.InvocationInfo.ScriptLineNumber): $($_.InvocationInfo.Line.Trim())"
    "  -> $($_.Exception.Message)"
    exit 1
}
$stencilPath = "C:\Program Files\Microsoft Office\root\Office16\Visio Content\2052\BASIC_M.VSSX"

$typeMap = @{
    "process" = "Rounded Rectangle"; "start" = "Ellipse"; "end" = "Ellipse"
    "decision" = "Diamond"; "data" = "Parallelogram"; "sub" = "Rectangle"; "note" = "Rectangle"
}
$heightMap = @{
    "process" = 0.9; "start" = 0.72; "end" = 0.72; "decision" = 1.0
    "data" = 0.9; "sub" = 0.85; "note" = 0.8
}
$fillMap = @{
    "process" = "RGB(218,232,252)"; "start" = "RGB(213,232,212)"; "end" = "RGB(248,206,204)"
    "decision" = "RGB(255,242,204)"; "data" = "RGB(225,213,231)"; "sub" = "RGB(255,255,255)"; "note" = "RGB(255,249,196)"
}
$lineMap = @{
    "process" = "RGB(108,142,191)"; "start" = "RGB(130,179,102)"; "end" = "RGB(184,84,80)"
    "decision" = "RGB(214,182,86)"; "data" = "RGB(150,115,166)"; "sub" = "RGB(102,102,102)"; "note" = "RGB(192,192,192)"
}
$patternMap = @{ "process" = 1; "start" = 1; "end" = 1; "decision" = 1; "data" = 1; "sub" = 2; "note" = 1 }

function Get-TextWidthIn([string]$text, [double]$fontPt = 9.5) {
    $em = $fontPt / 72.0
    $w = 0.0
    foreach ($ch in $text.ToCharArray()) {
        $code = [int]$ch
        if ($code -ge 0x2E80) { $w += 1.0 * $em }
        elseif ($ch -match '[A-Za-z0-9]') { $w += 0.58 * $em }
        elseif ($ch -eq ' ') { $w += 0.3 * $em }
        else { $w += 0.5 * $em }
    }
    return $w
}

function Get-BoxWidth([string]$t, [double]$textW) {
    switch ($t) {
        "decision" { $w = $textW * 1.35 + 0.50; if ($w -lt 1.95) { $w = 1.95 } }
        "start"    { $w = $textW * 1.22 + 0.50; if ($w -lt 2.05) { $w = 2.05 } }
        "end"      { $w = $textW * 1.22 + 0.50; if ($w -lt 2.05) { $w = 2.05 } }
        default    { $w = $textW + 0.50; if ($w -lt 2.35) { $w = 2.35 } }
    }
    return $w
}

$v = New-Object -ComObject Visio.Application
$v.Visible = $false
$v.AlertResponse = 7
$stencil = $v.Documents.OpenEx($stencilPath, 2)

foreach ($dir in $Dirs) {
    $dirAbs = (Resolve-Path -LiteralPath $dir).Path
    $nodes = Import-Csv -Path (Join-Path $dirAbs "nodes.csv") -Encoding UTF8
    $edges = Import-Csv -Path (Join-Path $dirAbs "edges.csv") -Encoding UTF8

    # 最长路径分层
    $rank = @{}
    foreach ($n in $nodes) { $rank[$n.id] = 0 }
    for ($k = 0; $k -lt $nodes.Count + 1; $k++) {
        $changed = $false
        foreach ($e in $edges) {
            if ($rank.ContainsKey($e.source) -and $rank.ContainsKey($e.target)) {
                if ($rank[$e.target] -lt $rank[$e.source] + 1) {
                    $rank[$e.target] = $rank[$e.source] + 1
                    $changed = $true
                }
            }
        }
        if (-not $changed) { break }
    }

    $rowOf = @{}
    $maxRank = 0
    $nodeIndex = @{}
    for ($i = 0; $i -lt $nodes.Count; $i++) { $nodeIndex[$nodes[$i].id] = $i }
    foreach ($n in $nodes) { if ($rank[$n.id] -gt $maxRank) { $maxRank = $rank[$n.id] } }

    # 逐层按前驱重心排序，减少跨越
    $rowCursor = @{}
    for ($r = 0; $r -le $maxRank; $r++) {
        $ids = @($nodes | Where-Object { $rank[$_.id] -eq $r } | ForEach-Object { $_.id })
        $scores = @{}
        foreach ($id in $ids) {
            $preds = @($edges | Where-Object { $_.target -eq $id -and $rank[$_.source] -lt $r })
            if ($preds.Count -gt 0) {
                $sum = 0.0
                foreach ($p in $preds) { $sum += $rowOf[$p.source] }
                $scores[$id] = $sum / $preds.Count
            }
            else { $scores[$id] = [double]$nodeIndex[$id] }
        }
        $sorted = $ids | Sort-Object @{ Expression = { $scores[$_] * 1000 + $nodeIndex[$_] } }
        $rowCursor[$r] = 0
        foreach ($id in $sorted) { $rowOf[$id] = $rowCursor[$r]; $rowCursor[$r]++ }
    }

    $seen = @{}
    $maxRows = 0
    foreach ($n in $nodes) {
        $r = $rank[$n.id]
        if (-not $seen.ContainsKey($r)) { $seen[$r] = 0 }
        $seen[$r]++
        if ($seen[$r] -gt $maxRows) { $maxRows = $seen[$r] }
    }

    # 依据文字估算宽度
    $wOf = @{}
    $maxW = 0.0
    foreach ($n in $nodes) {
        $t = $n.type
        if (-not $t -or -not $typeMap.ContainsKey($t)) { $t = "process" }
        $tw = Get-TextWidthIn $n.label 9.5
        $bw = Get-BoxWidth $t $tw
        $wOf[$n.id] = $bw
        if ($bw -gt $maxW) { $maxW = $bw }
    }

    $colPitch = $maxW + 0.95
    $rowPitch = 1.25
    $margin = 0.75
    $pageW = $margin * 2 + $maxW + $maxRank * $colPitch
    $pageH = $margin * 2 + ($maxRows - 1) * $rowPitch + 1.05
    if ($pageH -lt 6) { $pageH = 6 }

    $doc = $v.Documents.Add("")
    $page = $doc.Pages.Item(1)
    $page.PageSheet.CellsU("PageWidth").FormulaU = "$pageW in"
    $page.PageSheet.CellsU("PageHeight").FormulaU = "$pageH in"
    $page.PageSheet.CellsU("PrintPageOrientation").FormulaU = $(if ($pageW -ge $pageH) { 2 } else { 1 })

    $shapes = @{}
    $geom = @{}
    $colRight = @{}
    $colLeft = @{}
    foreach ($n in $nodes) {
        $t = $n.type
        if (-not $t -or -not $typeMap.ContainsKey($t)) { $t = "process" }
        $w = $wOf[$n.id]; $h = $heightMap[$t]
        $cx = $margin + $maxW / 2 + $rank[$n.id] * $colPitch
        $cy = $pageH - ($margin + 0.52 + $rowOf[$n.id] * $rowPitch)
        $col = $rank[$n.id]
        $rEdge = $cx + $w / 2
        $lEdge = $cx - $w / 2
        if (-not $colRight.ContainsKey($col) -or $rEdge -gt $colRight[$col]) { $colRight[$col] = $rEdge }
        if (-not $colLeft.ContainsKey($col) -or $lEdge -lt $colLeft[$col]) { $colLeft[$col] = $lEdge }
        $s = $page.Drop($stencil.Masters.ItemU($typeMap[$t]), $cx, $cy)
        $s.CellsU("Width").FormulaU = "$w in"
        $s.CellsU("Height").FormulaU = "$h in"
        $s.Text = $n.label
        $s.CellsU("Char.Size").FormulaU = "9.5 pt"
        $s.CellsU("Char.Color").FormulaU = "RGB(26,26,26)"
        $s.CellsU("Para.HorzAlign").FormulaU = "1"
        $s.CellsU("VerticalAlign").FormulaU = "1"
        $s.CellsU("LeftMargin").FormulaU = "0.05 in"
        $s.CellsU("RightMargin").FormulaU = "0.05 in"
        $s.CellsU("TopMargin").FormulaU = "0.01 in"
        $s.CellsU("BottomMargin").FormulaU = "0.01 in"
        $s.CellsU("FillForegnd").FormulaForceU = $fillMap[$t]
        $s.CellsU("FillPattern").FormulaU = "1"
        $s.CellsU("LineColor").FormulaForceU = $lineMap[$t]
        $s.CellsU("LineWeight").FormulaU = "0.9 pt"
        $s.CellsU("LinePattern").FormulaU = "$($patternMap[$t])"
        $shapes[$n.id] = $s
        $geom[$n.id] = @{ cx = $cx; cy = $cy; w = $w; h = $h }
    }

    $labelCount = 0
    $labelNo = @{}
    $segCount = 0
    foreach ($e in $edges) {
        if (-not $geom.ContainsKey($e.source) -or -not $geom.ContainsKey($e.target)) { continue }
        $sg = $geom[$e.source]; $tg = $geom[$e.target]
        $sx = $sg.cx + $sg.w / 2; $sy = $sg.cy
        $tx = $tg.cx - $tg.w / 2; $ty = $tg.cy
        $gap = $rank[$e.target] - $rank[$e.source]

        $pts = @()
        $pts += , @($sx, $sy)
        if ($gap -le 1) {
            $chan = $colRight[$rank[$e.source]] + 0.30
            $pts += , @($chan, $sy)
            $pts += , @($chan, $ty)
        }
        else {
            $chanR = $colRight[$rank[$e.source]] + 0.30
            $chan = $chanR
            $chanL = $colLeft[$rank[$e.target]] - 0.30
            if ($ty -gt $pageH / 2) { $laneY = $ty - $rowPitch / 2 } else { $laneY = $ty + $rowPitch / 2 }
            $pts += , @($chanR, $sy)
            $pts += , @($chanR, $laneY)
            $pts += , @($chanL, $laneY)
            $pts += , @($chanL, $ty)
        }
        $pts += , @($tx, $ty)

        $clean = @()
        foreach ($pt in $pts) {
            if ($clean.Count -eq 0) { $clean += , $pt; continue }
            $last = $clean[$clean.Count - 1]
            if ([math]::Abs($last[0] - $pt[0]) -gt 1e-6 -or [math]::Abs($last[1] - $pt[1]) -gt 1e-6) { $clean += , $pt }
        }

        for ($i = 0; $i -lt $clean.Count - 1; $i++) {
            $ln = $page.DrawLine($clean[$i][0], $clean[$i][1], $clean[$i + 1][0], $clean[$i + 1][1])
            $ln.CellsU("LineColor").FormulaU = "RGB(51,51,51)"
            $ln.CellsU("LineWeight").FormulaU = "1 pt"
            if ($i -eq $clean.Count - 2) {
                $ln.CellsU("EndArrow").FormulaU = "4"
                $ln.CellsU("EndArrowSize").FormulaU = "2"
            }
            $segCount++
        }

        if ($e.label) {
            $labelCount++
            if ([math]::Abs($ty - $sy) -lt 0.05) {
                $cxL = ($sx + $tx) / 2
                $cyL = $sy
            }
            else {
                $cxL = $chan
                if ($gap -le 1) { $cyL = ($sy + $ty) / 2 } else { $cyL = ($sy + $laneY) / 2 }
            }
            $tw = Get-TextWidthIn $e.label 8
            $labW = $tw + 0.20
            $labH = 0.28
            $lx = $cxL - $labW / 2
            $y1 = $cyL - $labH / 2
            if ($lx -lt 0.03) { $lx = 0.03 }
            if ($y1 -lt 0.03) { $y1 = 0.03 }
            $lab = $page.DrawRectangle($lx, $y1, $lx + $labW, $y1 + $labH)
            $lab.Text = $e.label
            $lab.CellsU("Char.Size").FormulaU = "8 pt"
            $lab.CellsU("Char.Color").FormulaU = "RGB(60,60,60)"
            $lab.CellsU("Para.HorzAlign").FormulaU = "1"
            $lab.CellsU("VerticalAlign").FormulaU = "1"
            $lab.CellsU("LeftMargin").FormulaU = "0 in"
            $lab.CellsU("RightMargin").FormulaU = "0 in"
            $lab.CellsU("TopMargin").FormulaU = "0 in"
            $lab.CellsU("BottomMargin").FormulaU = "0 in"
            $lab.CellsU("LinePattern").FormulaU = "0"
            $lab.CellsU("FillPattern").FormulaU = "1"
            $lab.CellsU("FillForegnd").FormulaU = "RGB(255,255,255)"
        }
    }

    $out = Join-Path $dirAbs $OutName
    if (Test-Path $out) { Remove-Item $out -Force }
    $doc.SaveAs($out)
    $doc.Close()

    if ($Verify) {
        $chk = $v.Documents.Open($out)
        $p2 = $chk.Pages.Item(1)
        $n2d = 0; $n1d = 0
        foreach ($s in $p2.Shapes) { if ($s.OneD) { $n1d++ } else { $n2d++ } }
        $pw = [math]::Round($p2.PageSheet.CellsU("PageWidth").ResultIU, 2)
        $ph = [math]::Round($p2.PageSheet.CellsU("PageHeight").ResultIU, 2)
        "{0}: nodes={1} edges={2} labelTexts={3} shapes2D={4} lineSegments={5} (expect {6}) page={7}x{8}in" -f `
            (Split-Path $dirAbs -Leaf), $nodes.Count, $edges.Count, $labelCount, ($n2d - $labelCount), $n1d, $segCount, $pw, $ph
        $chk.Close()
    }
    else {
        "{0}: nodes={1} edges={2} -> {3}" -f (Split-Path $dirAbs -Leaf), $nodes.Count, $edges.Count, $out
    }
}

$stencil.Close()
$v.Quit()
[System.Runtime.InteropServices.Marshal]::ReleaseComObject($v) | Out-Null
[GC]::Collect()
