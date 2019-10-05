# statcastr
## インストール
remotes::install_github("pontsuyu/statcastr")

## 概要
**パッケージに含まれる関数**

- scrape_statcast
  - このパッケージのメインの関数で任意の期間のピッチング・バッティングのデータをスクレイピングする。
- get_snapshots
  - scrape_statcastで得たデータから1球ごとの球の軌道を計算する。
- get_strikezones
  - 打者位置ごとにストライクゾーンの平均位置を計算する。

