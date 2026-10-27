# ==============================================================================
# GitHub Actions 專用：把 pak 用「any::」安裝的套件，來源標記改回一般的 standard
#
# 問題：GitHub Actions 的 R 設定工具 (r-lib/actions) 會用 any::套件名 的方式安裝一些套件
#       (例如它自己需要的 httr)。這些套件的 DESCRIPTION 會寫 RemoteType: any。
#       部署時 renv 把它們記成 Source = "any"，而 rsconnect 只會替「套件庫來源」換上下載網址，
#       any 不在其中 → shinyapps.io 拿到的是「RSPM」這個名稱而不是網址 → 安裝失敗
#       (錯誤：Unsupported url scheme: RSPM/src/contrib/RANN_2.6.3.tar.gz)
#
# 修法：把 RemoteType: any 改回 standard (any:: 本來就只是「已安裝就不重裝」的意思，套件本身相同)
#       在本機已用相同情境重現並驗證：修正前 Source=any、修正後有完整網址
#
# 這支程式只在用完即丟的雲端主機上執行 (會修改套件的 DESCRIPTION 檔)，不要在自己電腦上跑
# ==============================================================================
if (!identical(Sys.getenv("GITHUB_ACTIONS"), "true")) stop("這支程式只在 GitHub Actions 上執行")

fixed <- character(0)
for (desc_file in Sys.glob(file.path(.libPaths(), "*", "DESCRIPTION"))) {
  d <- tryCatch(read.dcf(desc_file), error = function(e) NULL)
  if (is.null(d) || !"RemoteType" %in% colnames(d) || d[, "RemoteType"] != "any") next
  d[, "RemoteType"] <- "standard"
  if ("RemotePkgRef" %in% colnames(d)) d[, "RemotePkgRef"] <- sub("^any::", "", d[, "RemotePkgRef"])
  write.dcf(d, desc_file)
  fixed <- c(fixed, basename(dirname(desc_file)))
}
cat("✓ 修正了", length(fixed), "個套件的來源標記：", paste(fixed, collapse = ", "), "\n")
