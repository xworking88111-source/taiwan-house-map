"""
特徵函數 (Python 版)：和 R 的 new_features.R / app/shared_features.R「同一套定義」

為什麼要用 Python 再寫一次？
  台灣多數資料科學職缺要求 Python (pandas、scikit-learn)。同一個問題用兩種語言實作，
  而且結果對得起來，面試時可以證明：你懂的是「方法」，不是只會某個語言的套件。

R ↔ Python 對照 (這個檔案用到的)：
  data.table 的 rolling join (roll = TRUE)  ↔  pandas.merge_asof
  data.table 的 non-equi join               ↔  「累計加總 + merge_asof 兩次相減」(同一個技巧)
  RANN::nn2 (k-d tree 最近鄰)               ↔  scipy.spatial.cKDTree
  sf::st_read / st_transform                ↔  geopandas.read_file / to_crs
  stringr::str_match / regmatches           ↔  pandas 的 .str.extract (正規表示式)
"""
from __future__ import annotations

import numpy as np
import pandas as pd
from scipy.spatial import cKDTree

CELL_SIZE = 250        # 附近行情的格子大小 (公尺)，和 R 的 cell_size 相同
ROAD_GRID = 2000       # 道路等級查詢的方格大小 (公尺)，和 R 的 road_grid 相同
ROAD_RANK = {"trunk": 5, "primary": 5, "secondary": 4, "tertiary": 3, "residential": 2,
             "unclassified": 2, "living_street": 2, "service": 1, "motorway": 0}


# --- 共用工具：用「累計加總」算任意時間窗的總和 ---------------------------------------
def _window_sum(src: pd.DataFrame, query: pd.DataFrame, keys: list[str]) -> pd.DataFrame:
    """
    src  ：已經按 keys + m 加總好的表，欄位 keys + [m, s, n, (sm)]
    query：每一列要查的 keys + hi (時間窗結尾) + lo (時間窗開頭前一個月) + qid
    回傳 ：每個 qid 在 (lo, hi] 之間的 s、n、sm 總和

    做法 (和 R 版完全相同)：
      1. 每個 key 依月份排序後算累計值 cs = cumsum(s)
      2. 查「月份 <= hi 的最後一筆累計值」 - 「月份 <= lo 的最後一筆累計值」= 窗內總和
    第 2 步就是 merge_asof：找「不超過」查詢值的最近一筆 (= data.table 的 roll = TRUE)
    """
    val_cols = [c for c in ("s", "n", "sm") if c in src.columns]
    src = src.sort_values(keys + ["m"]).copy()
    for c in val_cols:
        src["c" + c] = src.groupby(keys, sort=False)[c].cumsum()
    cum = src[keys + ["m"] + ["c" + c for c in val_cols]]

    def asof(col: str) -> pd.DataFrame:
        q = query[keys + ["qid", col]].rename(columns={col: "m"}).sort_values("m")
        # merge_asof 要求 on 欄位 (m) 兩邊都排序好；by = 分組欄位 (相當於 R 的 on = .(cx, cy, type, ...))
        out = pd.merge_asof(q, cum.sort_values("m"), on="m", by=keys, direction="backward")
        return out.set_index("qid")[["c" + c for c in val_cols]].fillna(0)

    w = asof("hi") - asof("lo")
    w.columns = val_cols
    return w.groupby(level=0).sum()


# --- 附近行情 (R：neighbor_price) ------------------------------------------------------
def neighbor_price(x, y, btype, month, logp, cap_month=np.inf, window=12, min_n=3) -> pd.DataFrame:
    """附近 9 格 (約 750 公尺見方)、交易月份之前 window 個月的 log 單價平均"""
    d = pd.DataFrame({"cx": np.floor(np.asarray(x) / CELL_SIZE).astype(np.int64),
                      "cy": np.floor(np.asarray(y) / CELL_SIZE).astype(np.int64),
                      "type": np.asarray(btype), "m": np.asarray(month, dtype=float),
                      "lp": np.asarray(logp, dtype=float)})
    d["qid"] = np.arange(len(d))
    out = {}
    for name, type_col in (("same", d["type"]), ("all", pd.Series("ALL", index=d.index))):
        src = d.assign(type=type_col).groupby(["cx", "cy", "type", "m"], as_index=False) \
               .agg(s=("lp", "sum"), n=("lp", "size"))
        q = d.assign(type=type_col, hi=np.minimum(d["m"] - 1, cap_month), lo=d["m"] - window - 1)
        # 每筆交易複製 9 份，分別指向自己和周圍 8 格 (R：rep(..., each = 9))
        q = q.loc[q.index.repeat(9)].reset_index(drop=True)
        q["cx"] += np.tile(np.repeat([-1, 0, 1], 3), len(d))
        q["cy"] += np.tile([-1, 0, 1], 3 * len(d))
        q = q[["cx", "cy", "type", "qid", "hi", "lo"]]
        q["qid_row"] = np.arange(len(q))               # merge_asof 需要唯一的列編號，最後再按 qid 加總
        w = _window_sum(src, q.rename(columns={"qid": "orig", "qid_row": "qid"}), ["cx", "cy", "type"])
        w["orig"] = q.set_index("qid_row").loc[w.index, "qid"].to_numpy()
        out[name] = w.groupby("orig")[["s", "n"]].sum().reindex(d["qid"], fill_value=0)
    same, allt = out["same"], out["all"]
    return pd.DataFrame({
        "nbr_lp": np.where(same["n"] >= min_n, same["s"] / same["n"].replace(0, np.nan), np.nan),
        "nbr_n": same["n"].to_numpy().astype(int),
        "nbr_lp_all": np.where(allt["n"] >= min_n, allt["s"] / allt["n"].replace(0, np.nan), np.nan),
    })


# --- 同棟歷史成交 (R：building_history) ------------------------------------------------
def building_history(addr_key, month, logp, cap_month=np.inf, window=36) -> pd.DataFrame:
    """同門牌、之前 window 個月內 (不含同月)、最多看到 cap_month 的成交：log 平均、筆數、平均距今月數"""
    month = np.asarray(month, dtype=float)
    d = pd.DataFrame({"k": np.asarray(addr_key), "m": month, "lp": np.asarray(logp, dtype=float)})
    src = d[d["m"] <= cap_month].groupby(["k", "m"], as_index=False) \
            .agg(s=("lp", "sum"), n=("lp", "size"))
    src["sm"] = src["m"] * src["n"]                     # 月份 × 筆數的總和 → 用來算「平均距今幾個月」
    qm = np.minimum(month, cap_month + 1)              # 測試期 = 站在 cap_month + 1 往回看
    q = pd.DataFrame({"k": d["k"], "qid": np.arange(len(d)), "hi": qm - 1, "lo": qm - window - 1})
    w = _window_sum(src, q, ["k"]).reindex(np.arange(len(d)), fill_value=0)
    n = w["n"].to_numpy()
    with np.errstate(invalid="ignore", divide="ignore"):
        return pd.DataFrame({"bld_lp": np.where(n > 0, w["s"] / n, np.nan),
                             "bld_n": n.astype(int),
                             "bld_dt": np.where(n > 0, qm - w["sm"] / n, np.nan)})


# --- 臨路等級 (R：build_road_tables / road_features) ------------------------------------
def build_road_tables(shp_path: str) -> dict:
    """OSM 道路圖 → 道路等級表 (路名 + 2 公里方格) + 主要道路每 50 公尺一個點"""
    import geopandas as gpd
    import shapely

    roads = gpd.read_file(shp_path, columns=["name", "fclass"],
                          where="fclass IN (" + ",".join(f"'{k}'" for k in ROAD_RANK) + ")").to_crs(3826)
    major = roads[roads["fclass"].isin(["trunk", "primary", "secondary"])]
    major_xy = shapely.get_coordinates(shapely.segmentize(major.geometry.values, 50)).round()
    pts = shapely.point_on_surface(roads.geometry.values)
    osm = pd.DataFrame({"road": road_name(roads["name"].fillna("")),
                        "rank": roads["fclass"].map(ROAD_RANK),
                        "gx": np.floor(shapely.get_x(pts) / ROAD_GRID).astype(int),
                        "gy": np.floor(shapely.get_y(pts) / ROAD_GRID).astype(int)}).dropna(subset=["road"])
    osm = osm.groupby(["road", "gx", "gy"], as_index=False)["rank"].max()
    # 每個方格的等級複製到周圍 9 格 → 查詢時一次 lookup 就等於查附近 3 公里
    cls = pd.concat([osm.assign(gx=osm.gx + dx, gy=osm.gy + dy) for dx in (-1, 0, 1) for dy in (-1, 0, 1)])
    cls = cls.groupby(["road", "gx", "gy"], as_index=False)["rank"].max()
    return {"cls": dict(zip(cls.road + " " + cls.gx.astype(str) + " " + cls.gy.astype(str), cls["rank"])),
            "major": cKDTree(major_xy)}


def road_name(s: pd.Series) -> pd.Series:
    """第一個「…路 / …街 / …大道」；找不到 → NaN (和 R 的 road_name() 相同)"""
    return pd.Series(s, dtype="string").str.extract(r"^(.+?(?:大道|路|街))", expand=False)


def road_features(rest, xy, rt: dict) -> pd.DataFrame:
    """rest = 地址去掉縣市區之後的部分；xy = n × 2 的 TWD97 座標"""
    rest = pd.Series(rest, dtype="string").fillna("")
    rest = rest.str.replace(r"^[^0-9路街道巷弄段]{1,4}[村里](?=\d+鄰|[^0-9]+(?:路|街|大道|巷))", "", regex=True)
    rest = rest.str.replace(r"^\d+鄰", "", regex=True)
    road = road_name(rest)
    xy = np.asarray(xy, dtype=float)
    key = road + " " + pd.Series(np.floor(xy[:, 0] / ROAD_GRID).astype(int).astype(str)) \
               + " " + pd.Series(np.floor(xy[:, 1] / ROAD_GRID).astype(int).astype(str))
    lane = np.where(rest.str.contains(r"\d+弄"), 2, np.where(rest.str.contains(r"\d+巷"), 1, 0))
    rtype = np.select([road.str.endswith("大道").fillna(False), road.str.endswith("路").fillna(False),
                       road.str.endswith("街").fillna(False)], [3, 2, 1], 0)
    return pd.DataFrame({"lane_depth": lane, "road_type": rtype,
                         "road_class": key.map(rt["cls"]).astype(float).to_numpy(),
                         "d_major_road": np.round(rt["major"].query(xy, k=1)[0])})
