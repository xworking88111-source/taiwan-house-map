"""
Python 版估價模型：讀資料 → 特徵 → XGBoost → 評分，和 R 版 (new_model_training.R) 對照

執行：cd python && .venv/bin/python train.py
產出：../models/python_results.csv (和 R 版的成績並排比較)

為什麼結果不會和 R「一模一樣」？
  1. 兩邊的 XGBoost 版本不同 (內部演算法細節會更新)
  2. 類別欄位的「編號順序」：R 的 sort() 依系統語系排序中文，Python 依 Unicode 編碼排序
     → 「鳳山區」在兩邊可能是不同編號，樹的切法就會不同
  3. 浮點數運算順序不同
  所以比較的重點是：整體誤差、各型態誤差要「非常接近」(差 0.1~0.2 個百分點內)，代表兩邊的方法一致
"""
from __future__ import annotations

import time
from pathlib import Path

import numpy as np
import pandas as pd
import xgboost as xgb

from twre.data import FEATURES, CATEGORICAL, clean, engineer, load_training_data, to_matrix
from twre.features import build_road_tables, building_history, neighbor_price, road_features

ROOT = Path(__file__).resolve().parent.parent          # 專案根目錄 (python/ 的上一層)
t0 = time.time()
log = lambda msg: print(f"[{time.time() - t0:6.1f}s] {msg}", flush=True)

# --- 1~3. 讀資料、清理、特徵工程 ---------------------------------------------------------
df = engineer(clean(load_training_data()))
log(f"讀入並清理後 {len(df):,} 筆")

# --- 4. 時間切分 + 需要「依切點」計算的特徵 -----------------------------------------------
cut = df["t_month_idx"].max() - 6                    # 最近 6 個月 = 測試集
btype = df["b_category"].astype(str)
df = pd.concat([df,
                neighbor_price(df.tx, df.ty, btype, df.t_month_idx, df.y, cap_month=cut),
                building_history(df.addr_key, df.t_month_idx, df.y, cap_month=cut)], axis=1)
rt = build_road_tables(str(ROOT / "geo_data" / "gis_osm_roads_free_1.shp"))
rest = [a.replace(c, "", 1).replace(d, "", 1) for a, c, d in zip(df.address_norm, df.city, df.district)]
df = pd.concat([df, road_features(rest, df[["tx", "ty"]].to_numpy(), rt)], axis=1)
log(f"特徵完成：附近行情有值 {df.nbr_lp.notna().mean():.1%}、同棟紀錄 {(df.bld_n > 0).mean():.1%}、"
    f"道路等級對到 {df.road_class.notna().mean():.1%}")

levels = {c: sorted(df[c].astype(str).unique()) for c in CATEGORICAL}
train, test = df[df.t_month_idx <= cut], df[df.t_month_idx > cut]
valid_mask = train.t_month_idx > cut - 3              # 訓練集最後 3 個月 = 驗證集 (決定種幾棵樹)

# --- 5. 基準模型：同區同型態中位數 (永遠先做一個笨模型) -------------------------------------
med = train.groupby(["city", "district", "b_category"])["y"].median().rename("med")
city_med = train.groupby(["city", "b_category"])["y"].median().rename("city_med")
base = test.join(med, on=["city", "district", "b_category"]).join(city_med, on=["city", "b_category"])
pred_base = base["med"].fillna(base["city_med"]).to_numpy()

# --- 6. XGBoost：參數和 R 版相同 ------------------------------------------------------------
params = {"objective": "reg:squarederror", "eta": 0.1, "max_depth": 8, "subsample": 0.8,
          "colsample_bytree": 0.8, "min_child_weight": 10, "seed": 42}
dtrain = xgb.DMatrix(to_matrix(train[~valid_mask], levels), label=train.y[~valid_mask])
dvalid = xgb.DMatrix(to_matrix(train[valid_mask], levels), label=train.y[valid_mask])
model = xgb.train(params, dtrain, num_boost_round=1500, evals=[(dvalid, "valid")],
                  early_stopping_rounds=100, verbose_eval=False)
pred = model.predict(xgb.DMatrix(to_matrix(test, levels)), iteration_range=(0, model.best_iteration + 1))
log(f"XGBoost 完成：{model.best_iteration + 1} 棵樹")


# --- 7. 評分：和 R 版同樣的指標 -------------------------------------------------------------
def score(name: str, pred_log: np.ndarray) -> dict:
    ape = np.abs(np.exp(pred_log) - np.exp(test.y)) / np.exp(test.y)
    by_type = pd.Series(ape.to_numpy()).groupby(test.b_category.astype(str).to_numpy()).mean()
    return {"model": name, "MAPE": ape.mean(), "hit_10": (ape <= .1).mean(), "hit_20": (ape <= .2).mean(),
            **{f"MAPE_{k}": by_type.get(k, np.nan) for k in ["透天厝", "電梯大樓", "華廈", "公寓"]}}


res = pd.DataFrame([score("Python 基準：區域中位數", pred_base), score("Python XGBoost", pred)])
print(res.to_string(index=False, float_format=lambda v: f"{v:.2%}"))
res.to_csv(ROOT / "models" / "python_results.csv", index=False)

# 特徵重要度 (gain)：模型最依賴哪些資訊？和 R 版前幾名應該差不多
imp = pd.Series(model.get_score(importance_type="gain"))
imp.index = [FEATURES[int(k[1:])] for k in imp.index]              # f0, f1… → 欄位名稱
print((imp / imp.sum()).sort_values(ascending=False).head(8).round(3).to_string())
