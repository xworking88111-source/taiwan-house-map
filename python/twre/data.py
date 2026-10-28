"""
讀資料、清理、特徵工程 (Python 版)：對應 R 的 new_model_training.R 第 1~4 節

R ↔ Python 對照：
  DBI::dbGetQuery(con, sql)                 ↔  psycopg 連線 + pandas.DataFrame(cur.fetchall())
  dplyr::group_by() %>% filter(between())   ↔  groupby().transform("quantile") + 布林遮罩
  mutate(x = if_else(...))                  ↔  df.assign(x=np.where(...))
  factor(levels = ...) → as.integer         ↔  pd.Categorical(categories=...).codes
"""
from __future__ import annotations

import os

import numpy as np
import pandas as pd
import psycopg

# 和 app/shared_features.R 的 dist_cols / count_cols 相同
DIST_COLS = ["mrt", "lrt", "tra", "hsr", "airport", "bus", "cemetery", "landfill", "wastewater",
             "industrial", "military", "fuel", "school", "park", "hospital", "market"]
COUNT_COLS = ["bus", "convenience"]

FEATURES_V1 = ["city", "district", "b_category", "house_age_at_trans", "pure_house_ping",
               "land_ratio", "floor_num", "total_floors_num", "floor_ratio", "is_top_floor",
               "rooms", "baths", "elevator", "management", "parking", "parking_unknown",
               "t_month_idx", "lng", "lat"]
FEATURES = FEATURES_V1 + ["nbr_lp", "nbr_n", "nbr_lp_all"] \
    + [f"d_{c}" for c in DIST_COLS] + [f"n_{c}_500" for c in COUNT_COLS] \
    + ["bld_lp", "bld_n", "bld_dt", "lane_depth", "road_type", "road_class", "d_major_road"] \
    + ["zoning_code", "main_ratio"]                       # v4 (實驗 05)：使用分區、主建物比例
CATEGORICAL = ["city", "district", "b_category"]


def connect() -> psycopg.Connection:
    """和 R 的 config.R / db_connect() 讀同一組環境變數 (PGHOST、PGUSER…)，本機沒設就用預設值"""
    return psycopg.connect(host=os.getenv("PGHOST", "localhost"), port=os.getenv("PGPORT", "5432"),
                           user=os.getenv("PGUSER", os.getenv("USER")), password=os.getenv("PGPASSWORD", ""),
                           dbname=os.getenv("PGDATABASE", "postgres"))


def load_training_data(schema: str = "re_v2") -> pd.DataFrame:
    """SQL 和 R 版逐字相同 (包含 ORDER BY：資料順序固定，訓練結果才能重現)"""
    fac_sql = ", ".join(f"gf.d_{c}" for c in DIST_COLS)
    cnt_sql = ", ".join(f"gf.n_{c}_500" for c in COUNT_COLS)
    sql = f"""
      SELECT f.addr_key, f.address_norm, f.city, f.district, f.b_category, f.trans_ym, f.zoning,
             f.house_age_at_trans, f.pure_house_ping, f.land_ping, f.main_ping,
             f.floor_num, f.total_floors_num, f.rooms, f.halls, f.baths,
             f.has_elevator, f.has_management, f.include_parking, f.parking_price_unknown,
             f.pure_house_unit_price_ping, f.pure_house_total_price,
             g.lng, g.lat, g.geo_level, gf.x AS tx, gf.y AS ty, {fac_sql}, {cnt_sql}
      FROM {schema}.fact_residential_house f
      JOIN {schema}.geo_cache g ON f.addr_key = g.addr_key
      JOIN {schema}.geo_features gf ON f.addr_key = gf.addr_key
      WHERE f.b_category IN ('透天厝', '電梯大樓', '華廈', '公寓', '套房')
        AND f.house_age_at_trans IS NOT NULL AND g.geo_level <> 'none'
      ORDER BY f.season, f.serial_number"""
    with connect() as con, con.cursor() as cur:   # with：離開區塊時自動關閉連線 (= R 的 on.exit)
        cur.execute(sql)
        df = pd.DataFrame(cur.fetchall(), columns=[c.name for c in cur.description])
    # PostgreSQL 的 NUMERIC 會變成 Python 的 Decimal → 轉成浮點數才能計算
    for c in df.columns:
        if df[c].dtype == object and len(df) and type(df[c].dropna().iloc[0]).__name__ == "Decimal":
            df[c] = df[c].astype(float)
    return df


def clean(df: pd.DataFrame) -> pd.DataFrame:
    """離群值：同縣市 + 同型態內，砍單價最低 1% 和最高 1% (和 R 的 quantile 預設算法 type 7 相同)"""
    df = df[df["trans_ym"].notna() & df["pure_house_ping"].between(5, 300) & (df["house_age_at_trans"] <= 70)]
    g = df.groupby(["city", "b_category"])["pure_house_unit_price_ping"]
    lo, hi = g.transform(lambda s: s.quantile(0.01)), g.transform(lambda s: s.quantile(0.99))
    return df[df["pure_house_unit_price_ping"].between(lo, hi)].reset_index(drop=True)


def engineer(df: pd.DataFrame) -> pd.DataFrame:
    ym = pd.to_datetime(df["trans_ym"])
    floor, total = df["floor_num"].astype(float), df["total_floors_num"].astype(float)
    # R 的 NA 規則：floor 有值但 total 缺值時，floor == total 是 NA (不是 FALSE) → 最後會被填成 -1
    top = np.where(floor.notna() & total.isna(), np.nan, (floor.notna() & (floor == total)).astype(float))
    return df.assign(
        y=np.log(df["pure_house_unit_price_ping"]),
        t_month_idx=(ym.dt.year - 2020) * 12 + ym.dt.month,
        floor_ratio=np.where(floor.notna() & (total > 0), floor / total, np.nan),
        is_top_floor=top,
        land_ratio=np.where(df["pure_house_ping"] > 0, df["land_ping"] / df["pure_house_ping"], np.nan),
        elevator=(df["has_elevator"] == "有").astype(int),
        management=(df["has_management"] == "有").astype(int),
        parking=(df["include_parking"] == "是").astype(float).where(df["include_parking"].notna()),
        parking_unknown=df["parking_price_unknown"].astype(float),
        zoning_code=zoning_code_of(df["zoning"]),
        main_ratio=np.where((df["main_ping"] > 0) & (df["pure_house_ping"] > 0),
                            np.minimum(df["main_ping"] / df["pure_house_ping"], 1.5), np.nan),
    )


def zoning_code_of(z: pd.Series) -> np.ndarray:
    """和 app/shared_features.R 的 zoning_code_of() 相同：0 空白/非都市、1 住、2 商、3 工、4 農、5 其他"""
    z = pd.Series(z, dtype="string").fillna("")
    conds = [(z == "") | (z == "非都市"), (z == "住") | z.str.contains("住宅區"), (z == "商") | z.str.contains("商業區"),
             (z == "工") | z.str.contains("工業區"), (z == "農") | z.str.contains("農業區")]
    return np.select([c.to_numpy(bool) for c in conds], [0, 1, 2, 3, 4], 5)


def to_matrix(df: pd.DataFrame, levels: dict[str, list[str]], features=FEATURES) -> np.ndarray:
    """和 R 的 prep_x() + to_matrix() 相同：類別 → 固定編號 (從 1 開始)、缺值 → -1"""
    x = df[features].copy()
    for c in CATEGORICAL:
        x[c] = pd.Categorical(x[c], categories=levels[c]).codes + 1   # R 的 factor 編號從 1 開始；找不到 = 0
        x.loc[x[c] == 0, c] = -1
    return x.astype(float).fillna(-1).to_numpy()
