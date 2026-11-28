-- | 唯一注册表：119 个记号，顺序与 C++ 侧 kMetaArray 一致。
module OmegaY.Registry
  ( allNotations
  , byId
  , missingIds
  ) where

import Data.Char (ord)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import OmegaY.Notation.SeqBasic
import OmegaY.Notation.Types
import OmegaY.Notation.YMagma

import qualified OmegaY.Fam.Aw2mn.Family as Aw2mn
import qualified OmegaY.Fam.Bmlike.Family as Bmlike
import qualified OmegaY.Fam.Bms.Family as Bms
import qualified OmegaY.Fam.Dbms.Family as Dbms
import qualified OmegaY.Fam.Den.Family as Den
import qualified OmegaY.Fam.G11242.Family as G11242
import qualified OmegaY.Fam.Gms.Family as Gms
import qualified OmegaY.Fam.M1ynss.Family as M1ynss
import qualified OmegaY.Fam.Mn.Family as Mn
import qualified OmegaY.Fam.Mrss.Family as Mrss
import qualified OmegaY.Fam.Ton.Family as Ton
import qualified OmegaY.Fam.Tree.Family as Tree
import qualified OmegaY.Fam.Upmn.Family as Upmn
import qualified OmegaY.Fam.Xy.Family as Xy
import qualified OmegaY.Fam.Yseq.Family as Yseq

-- | 全部记号的来源池（顺序无关，最终按 idOrder 重排）。
notationPool :: [Notation]
notationPool =
  [ emptyNotation
  , prssNotation
  , ppsNotation
  , pps4Notation
  , wpps4Notation
  , tpps4Notation
  , ewpps4Notation
  , secondPps4Notation
  , pps2Notation
  ]
    ++ Bms.familyNotations
    ++ Mrss.familyNotations
    ++ Bmlike.familyNotations
    ++ Gms.familyNotations
    ++ Tree.familyNotations
    ++ M1ynss.familyNotations
    ++ Den.familyNotations
    ++ Ton.familyNotations
    ++ Mn.familyNotations
    ++ Upmn.familyNotations
    ++ Dbms.familyNotations
    ++ Aw2mn.familyNotations
    ++ Yseq.familyNotations
    ++ Xy.familyNotations
    ++ G11242.familyNotations

-- | C++ @kMetaArray@ 的 id 顺序（119 项，逐字照抄）。
idOrder :: [String]
idOrder =
  [ "empty"
  , "prss"
  , "pps"
  , "pps4"
  , "wpps4"
  , "tpps4"
  , "ewpps4"
  , "spps4"
  , "2-pps4"
  , "omega-y-medium"
  , "omega-y-strong"
  , "mrss121"
  , "bms-v1"
  , "bms-v2"
  , "bms-v21"
  , "bms-v22"
  , "bms-v23"
  , "bms-v3"
  , "bms-v31"
  , "bms-v32"
  , "bms-v33"
  , "bms-v4"
  , "omega-y-sequence"
  , "epsilon-y"
  , "upms"
  , "bsm"
  , "bhm"
  , "1-bm-bhm"
  , "2-bm-bhm"
  , "3-bm-bhm"
  , "cms"
  , "dsm"
  , "blm"
  , "wmm"
  , "gms-gbms-omega-p"
  , "gms-gbms-pqss"
  , "gms-gbms-qss"
  , "gms-gbms-full"
  , "gms-gbms-weird"
  , "gms-upms-omega-p"
  , "gms-upms-pqss"
  , "gms-upms-qss"
  , "gms-upms-full"
  , "gms-upms-weird"
  , "gms-lpms2-omega-p"
  , "gms-lpms2-pqss"
  , "gms-lpms2-qss"
  , "gms-lpms2-full"
  , "gms-lpms2-weird"
  , "lpms"
  , "lptss"
  , "tbm"
  , "tupms"
  , "btbm"
  , "btbm-weak"
  , "bbm"
  , "-1y-(n+1)ss"
  , "t--1y-(n+1)ss"
  , "bt1ynss"
  , "bt2ynss"
  , "bt3ynss"
  , "btlynss"
  , "abs-bm1ynss"
  , "rel-bm1ynss"
  , "g-y"
  , "xy-v1"
  , "xy-v2"
  , "xy-v3"
  , "den1"
  , "blp"
  , "iblp"
  , "ton-m"
  , "ton-i"
  , "ton-dr"
  , "ton-drc"
  , "ton-drp"
  , "ton-drpc"
  , "ton-ibp"
  , "ton-mc"
  , "ton-mpc"
  , "e0mn"
  , "strong-e0mn"
  , "omega-mn"
  , "pcf-omega-mn"
  , "t-omega-mn"
  , "1-mn"
  , "2-mn"
  , "3-mn"
  , "s-omega2-mn"
  , "sa-omega2-mn"
  , "s-omega-pow-omega-mn"
  , "up1mn"
  , "up2mn-v1a"
  , "up2mn-v1b"
  , "up2mn-v1b-plus"
  , "up1dbms"
  , "up2dbms-v1"
  , "up2dbms-v1b-plus"
  , "up2dbms-v1c"
  , "up2dbms-v2"
  , "s1dbms"
  , "s2dbms"
  , "s-omega-dbms"
  , "s-omega-dbms-v2"
  , "s-omega-dbms-v3"
  , "s-omega-p1-dbms"
  , "s-omega2-dbms"
  , "s-omega-square-dbms"
  , "ts-2-dbms"
  , "ts-omega-dbms"
  , "a-omega2-mn-2"
  , "weak-a-omega2-mn-2"
  , "a-omega2-mn-3"
  , "weak-a-omega2-mn-3"
  , "srd"
  , "rpd"
  , "ard"
  , "ard2"
  , "ipd"
  ]

poolMap :: M.Map String Notation
poolMap = M.fromList [(nId n, n) | n <- notationPool]

-- | 按 kMetaArray 顺序排列（未实现的 id 自动缺省，见 'missingIds'）。
rawNotations :: [Notation]
rawNotations = [n | i <- idOrder, Just n <- [M.lookup i poolMap]]

-- 仅用于展示/枚举；使用哪个记号仍由 id 决定，不受此排序影响。
allNotations :: [Notation]
allNotations = sortOn (firstCodepoint . nName) rawNotations

firstCodepoint :: String -> Int
firstCodepoint [] = 0
firstCodepoint (c : _) = ord c

byId :: String -> Maybe Notation
byId ident = M.lookup ident poolMap

missingIds :: [String]
missingIds = [i | i <- idOrder, M.notMember i poolMap]
