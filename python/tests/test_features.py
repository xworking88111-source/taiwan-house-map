"""
特徵函數的自動化測試 (pytest)：和 R 的 tests/testthat 測同樣的規則
執行：cd python && .venv/bin/pytest -q
pytest 的規則很簡單：檔名 test_*.py、函數名 test_*，裡面用 assert 檢查；失敗時會印出兩邊的值
"""
import numpy as np
import pandas as pd
import pytest
from scipy.spatial import cKDTree

from twre.features import building_history, neighbor_price, road_features


# --- 同棟歷史 ------------------------------------------------------------------
def test_building_history_excludes_same_month():
    """只看更早的月份，同月份不能算 (防資料洩漏)"""
    r = building_history(["A", "A", "A"], [1, 5, 5], np.log([10, 20, 30]))
    assert r.bld_n.tolist() == [0, 1, 1]
    assert r.bld_lp[1] == pytest.approx(np.log(10))
    assert np.isnan(r.bld_lp[0])


def test_building_history_window_and_cap():
    assert building_history(["A", "A"], [1, 40], [1, 2]).bld_n[1] == 0       # 超過 36 個月
    assert building_history(["A", "A"], [4, 40], [1, 2]).bld_n[1] == 1       # 剛好在窗內
    r = building_history(["A"] * 3, [5, 12, 15], np.log([10, 20, 30]), cap_month=10)
    assert r.bld_n[2] == 1 and r.bld_dt[2] == pytest.approx(6)              # 站在第 11 個月看第 5 個月


def test_building_history_matches_slow_reference():
    """和「逐筆暴力計算」的結果比對 (參考實作測試法)"""
    rng = np.random.default_rng(1)
    k = rng.choice(list("ABC"), 300); m = rng.integers(1, 61, 300); lp = rng.normal(size=300)
    r = building_history(k, m, lp)
    slow = [((k == k[i]) & (m < m[i]) & (m >= m[i] - 36)).sum() for i in range(300)]
    assert r.bld_n.tolist() == slow


# --- 附近行情 ------------------------------------------------------------------
def test_neighbor_price_only_past_months():
    r = neighbor_price([10, 20, 30], [10, 20, 30], ["透天厝"] * 3, [1, 2, 3], [1.0, 2.0, 3.0], min_n=1)
    assert r.nbr_n.tolist() == [0, 1, 2]
    assert r.nbr_lp[2] == pytest.approx(1.5)


def test_neighbor_price_far_away_not_counted():
    r = neighbor_price([0, 2000], [0, 0], ["透天厝"] * 2, [1, 2], [1.0, 2.0], min_n=1)
    assert r.nbr_n[1] == 0


# --- 臨路等級 ------------------------------------------------------------------
RT = {"cls": {"中正路 0 0": 5, "民生街 0 0": 2}, "major": cKDTree([[0.0, 0.0]])}
XY = lambda n: np.tile([300.0, 400.0], (n, 1))                               # 距離原點 500 公尺


def test_road_lane_depth_and_type():
    r = road_features(["中正路10號", "中正路10巷3號", "中正路10巷5弄3號"], XY(3), RT)
    assert r.lane_depth.tolist() == [0, 1, 2]
    r = road_features(["台灣大道一段1號", "中正路1號", "民生街1號", "後壁厝1號"], XY(4), RT)
    assert r.road_type.tolist() == [3, 2, 1, 0]


def test_road_class_village_removed_and_missing():
    r = road_features(["安招里中正路1號", "安招里5鄰中正路1號", "不存在路1號", ""], XY(4), RT)
    assert r.road_class[:2].tolist() == [5, 5]
    assert np.isnan(r.road_class[2]) and np.isnan(r.road_class[3])
    assert r.d_major_road[0] == 500
