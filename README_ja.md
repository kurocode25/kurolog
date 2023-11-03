Kurolog
=======
This document is Japanese ver. of [README.md](https://github.com/kurocode25/kurolog/#readme)

## このプロジェクトについて
Kurologは[Haskell](https://www.haskell.org/)で書かれた個人向けブログバックエンドシステムです。APIを提供する他、サイトマップやAtomフィードを動的に生成する機能を持っています。ReactやVue等で作られたフロントエンドと組み合わせて使うことを想定しています。

__注意：このプロジェクトは開発中です。実用には機能が不安定な部分がある可能性があります。__

## 特徴
`Kurolog`には以下の機能があります。

+ Blog用のAPIを提供
+ SPAサイトにおける動的なOGPをサポート
+ サイトマップ生成機能
+ Atomフィード生成機能
+ データベースのマイグレーション機能

## ビルド
このプロジェクトをクローンした後に以下のコマンドを実行してください。

```bash
stack build
```

## 設定ファイル
### config.dhallファイル
Kurologは設定ファイルとして[dhall言語](https://dhall-lang.org/)で書かれた`config.dhall`ファイルを使います。`sample/config.dhall`を`/etc/kurolog/`ディレクトリ配下に配置して編集をして下さい。

```bash
sudo mkdir -p /etc/kurolog
sudo cp sample/config.dhall /etc/kurolog
```

このファイルでデータベース接続設定やブログのタイトルなどを設定します。

### config.dhallファイルの設置場所
このファイルのデフォルトの配置場所は`/etc/kurolog/config.dhall`です。しかしお好みの場所に設置し、コマンド実行時にパスを指定することもできます。

## 実行環境ごとの実行方法
### 開発環境
ローカル環境で実行する場合は以下のコマンドを実行します。

```bash
stack run -- serve [path/to/config.dhall]
```

### 本番環境
以下コマンドでバイナリファイルを作成します。

```bash
stack build
stack install
```
バイナリファイルは通常`~/.local/bin/`以下に作られますが、どこで実行しても構いません。

#### Systemdで使用する場合
もしSystemdを使用して実行する場合は以下のような`kurolog.service`ファイルを作成して`/etc/sysatemd/system/`以下に配置してください。

__kurolog.service(sample)__

```
# This is sample file
[Unit]
Description = Blog backend service

[Service]
ExecStart = /bin/bash -c '/home/username/.local/bin/kurolog serve'
Restart = always
Type = simple

[Install]
WantedBy=multi-user.target
```
`systemctl`コマンドでサービスの登録、開始をします。
```bash
sudo systemctl enable kurolog.service
sudo systemctl start kurolog.servic
```

## コマンドの使い方
以下のように`kurolog`コマンドを使います。

```bash
kurolog [COMMAND] [ARGS]
```
__コマンド一覧__

|コマンド|説明|
|---|---|
|serve|APIの提供開始|
|initdb|データベースの初期化|
|migrate|データベースのマイグレーション|
|createuser|ユーザー作成|
|--help|ヘルプ画面表示|

### 使用例
以下にコマンド実行例を示します。

+ APIの提供を開始

```bash
kurolog serve [path/to/config.dhall]
```

+ ユーザー作成

```bash
kurolog createuser [path/to/config.dhall]
```

+ データベースを初期化

```bash
kurolog initdb [path/to/config.dhall]
```

+ データベースのマイグレーション

```bash
kurolog migrate path/to/migration_dir [path/to/config.dhall]
```

+ ヘルプ画面の表示

```bash
kurolog --help
```

## TODO
+ 第二外国語への対応
+ servant-docsによるAPIドキュメント作成

## Licence
このプロジェクトは[MIT](http://www.opensource.org/licenses/mit-license.php)ライセンスに基づいてライセンスされています。詳細はLICENSEファイルを参照下さい。


