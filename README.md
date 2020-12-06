## インストール
devtools::install_github("pontsuyu/mlbr")

## 概要
**パッケージに含まれる関数**

- 選手情報<br>
`get_playerlist()`

- 指定プレイヤーの試合ごとの成績データ<br>
`fg_milb_gamelog(playerid, year, pit_bat)`<br>
`fg_mlb_gamelog(playerid, year, pit_bat)`

- プレイヤーの年間成績一覧<br>
`fg_leaders(year, league = "all", qual = "y", ind = 0, pit_bat)`

- 指定日の一球データ（マイナーなし）<br>
`sc_pbp(start_date, end_date)`

- 得点期待値の算出<br>
`sc_run_expectancy(df, level = "PA")`

- 線形ウェイト<br>
`sc_linear_weights(df, level = "PA")`

- 投球のスナップショット<br>
`sc_snapshots(df, interval = 0.01)`

- 平均ストライクゾーン<br>
`sc_strikezone_mean(df)`

- 打撃成績<br>
`sc_batting_stats(df)`

- 試合情報<br>
`statsapi_gameinfo(year, level_ids)`

- 一試合の一球データ（マイナーもある）<br>
`statsapi_pbp(game_pk)`

- 打順情報<br>
`statsapi_batting_orders(game_pk)`

- 球場情報<br>
`statsapi_gameinfo_pk(game_pk)`

- チームの試合ごとの成績<br>
`bref_team_results(Team, year)`

- 指定期間内の選手成績取得<br>
`bref_daily_Performance(start_date, end_date, pit_bat)`
