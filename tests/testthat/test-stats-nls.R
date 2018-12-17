context("test-stats-nls")

fm <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)
predict(fm)

safe_predict(fm, BOD)
