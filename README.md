# statcastr
## インストール
devtools::install_github("pontsuyu/mlbr")

## 概要
**パッケージに含まれる関数**

- 選手情報
get_playerlist()

- 試合情報
statsapi_gameinfo(date, level_ids = 1)

- 試合結果
bref_game_results(Team, year)

- 一試合の一球データ（マイナーもある）
statsapi_pbp(game_pk)

- 打順
statsapi_batting_orders(game_pk)

- 指定日の一球データ（マイナーなし）
sc_pbp(start_date, end_date)

- 投球のスナップショット
sc_snapshots(df, interval = 0.01)
sc_strikezones(df)

- 打撃成績
sc_batting_stats(df)

- 線形ウェイト
sc_linear_weights(df, level = "plate appearance")

- 指定プレイヤーの試合ごとの成績データ
fg_milb_gamelog(playerid, year, pit_bat)<br>
fg_mlb_gamelog(playerid, year, pit_bat)

- プレイヤーの年間成績一覧
fg_leaders(year, league = "all", qual = "y", ind = 0, pit_bat)

