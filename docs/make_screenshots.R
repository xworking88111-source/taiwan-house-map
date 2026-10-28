# ==============================================================================
# 自動產生 README 的截圖：用程式操作瀏覽器 (headless Chrome)，每次介面改版重跑一次就好
#
# chromote = R 透過 Chrome DevTools Protocol 控制 Chrome (和 Python 的 Playwright / Selenium 同概念)
#   navigate → 等網頁載入 → 用 JavaScript 點分頁、填輸入框 → screenshot
# 執行：Rscript docs/make_screenshots.R   (約 1~2 分鐘，產出 docs/images/*.png)
# ==============================================================================
library(chromote)
# 用本機的 App 截圖 (先在 app/ 執行 shiny::runApp(port = 3838))：線上版外面包了一層 iframe，自動化操作不穩定
url <- Sys.getenv("SHOT_URL", "http://127.0.0.1:3838")
out <- file.path(Sys.getenv("RE_PROJECT_PATH", "/Volumes/Dzzz_ssd/R所有資料/R_code/Code/All_data"), "docs", "images")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

js <- function(b, code) b$Runtime$evaluate(code, awaitPromise = TRUE)$result$value
wait_for <- function(b, cond, timeout = 60) {          # 一直問網頁「好了沒」，好了才繼續
  for (i in seq_len(timeout * 2)) { if (isTRUE(js(b, cond))) return(invisible(TRUE)); Sys.sleep(0.5) }
  stop("等太久：", cond)
}
shot <- function(b, name) { b$screenshot(file.path(out, name), show = FALSE); message("✅ ", name) }

open_app <- function(width, height, mobile = FALSE) {
  b <- ChromoteSession$new(width = width, height = height, mobile = mobile)
  b$Page$navigate(url); b$Page$loadEventFired(wait_ = TRUE)
  wait_for(b, "!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) && !!document.querySelector('.leaflet-tile-loaded')", 120)
  Sys.sleep(4)                                           # 等地圖圖磚、聚合標記畫完
  b
}
estimate <- function(b) {
  js(b, "document.querySelector('a[href^=\"#tab-\"][href$=\"-3\"]').click()"); Sys.sleep(1.5)
  js(b, "(() => { document.getElementById('p_type').selectize.setValue('電梯大樓');
               const s = document.getElementById('p_road').selectize, v = '經武路153號12樓';
               s.addOption({value: v, label: v}); s.setValue(v);
               const set = (id, v) => { const el = document.getElementById(id); el.value = v; $(el).trigger('change'); };
               set('p_age', 3); set('p_ping', 35); set('p_floor', 12); set('p_tfloor', 15);
               ['p_elevator', 'p_mgmt'].forEach(id => { const el = document.getElementById(id); el.checked = true; $(el).trigger('change'); });
               setTimeout(() => document.getElementById('p_go').click(), 800); })()")
  wait_for(b, "document.getElementById('p_unit') && /萬/.test(document.getElementById('p_unit').innerText)", 60)
  Sys.sleep(3)
}

# --- 電腦版：地圖、探索、估價 -----------------------------------------------------------
b <- open_app(1440, 900)
shot(b, "desktop_map.png")
js(b, "document.querySelector('a[href^=\"#tab-\"][href$=\"-2\"]').click()"); Sys.sleep(6)
shot(b, "desktop_explore.png")
estimate(b); shot(b, "desktop_estimate.png")
b$close()

# --- 手機版：估價 -------------------------------------------------------------------
m <- open_app(390, 844, mobile = TRUE)
shot(m, "mobile_map.png")
estimate(m); js(m, "document.getElementById('p_unit').scrollIntoView({block: 'start'})"); Sys.sleep(1)
shot(m, "mobile_estimate.png")
m$close()
