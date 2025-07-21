プログラム設計書
1. プログラム概要
本プログラムは、従業員マスタファイルを入力とし、役職コードが部長職以上（役職コードが '30' 以上）の従業員データを抽出し、管理職マスタファイルとして出力する。

2. プログラムID
EMP_EXTRACT

3. 入出力ファイル
ファイル名	論理名	I/O	レコード長	ブロック化因数	備考
従業員マスタファイル	EMP-MASTER	INPUT	可変長	-	ORGANIZATION IS LINE SEQUENTIAL
管理職マスタファイル	MGR-MASTER	OUTPUT	128	10	
4. レコードレイアウト
4.1. 従業員マスタファイル (EMP-MASTER-REC)
項目名	型	長さ	内容
EMP-ID	X(8)	8	従業員ID
EMP-NAME	X(57)	57	従業員名
DEPT-CODE	X(4)	4	所属部署コード
POS-CODE	X(2)	2	役職コード ('10':一般, '20':課長, '30':部長, '40':本部長)
HIRE-DATE	9(8)	8	入社年月日 (YYYYMMDD)
FILLER	X(49)	49	予備
4.2. 管理職マスタファイル (MGR-MASTER-REC)
従業員マスタファイルと同様のレイアウトとする。

5. 処理概要
初期処理 (INITIALIZE-PROCESS)

従業員マスタファイルおよび管理職マスタファイルを開く。
各種変数を初期化する。
主処理 (MAIN-PROCESS)

従業員マスタファイルを1レコードずつ読み込む。
ファイルの終わりに達するまで、以下の処理を繰り返す。
読み込んだレコードの役職コード (POS-CODE) を判定する。
役職コードが '30' 以上の場合、管理職マスタファイルに出力する。
終了処理 (FINALIZE-PROCESS)

従業員マスタファイルおよび管理職マスタファイルを閉じる。
処理件数（入力件数、出力件数）をコンソールに出力する。
プログラムを終了する。
6. 処理フロー
[開始]
  ↓
[初期処理]
  ↓
[従業員マスタファイル READ]
  ↓
[ファイル終了？] --(YES)--> [終了処理]
  ↓ (NO)
[役職コード >= '30'？] --(NO)--> [従業員マスタファイル READ]
  ↓ (YES)
[管理職マスタファイル WRITE]
  ↓
[従業員マスタファイル READ]
  ↓
[終了処理]
  ↓
[終了]
7. COBOLプログラム
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP-EXTRACT.
       AUTHOR. JULES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-MASTER ASSIGN TO "EMPMAST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MGR-MASTER ASSIGN TO "MGRMAST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMP-MASTER.
       01  EMP-MASTER-REC.
           05 EMP-ID        PIC X(8).
           05 EMP-NAME      PIC X(57).
           05 DEPT-CODE     PIC X(4).
           05 POS-CODE      PIC X(2).
           05 HIRE-DATE     PIC 9(8).
           05 FILLER        PIC X(49).

       FD  MGR-MASTER.
       01  MGR-MASTER-REC  PIC X(128).

       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG         PIC X(1) VALUE 'N'.
           88 EOF-EMP-MASTER          VALUE 'Y'.
       01  WS-IN-COUNT         PIC 9(5) VALUE 0.
       01  WS-OUT-COUNT        PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROCESS.
           PERFORM MAIN-PROCESS UNTIL EOF-EMP-MASTER.
           PERFORM FINALIZE-PROCESS.
           STOP RUN.

       INITIALIZE-PROCESS.
           OPEN INPUT EMP-MASTER
                OUTPUT MGR-MASTER.
           PERFORM READ-EMP-MASTER.

       MAIN-PROCESS.
           IF POS-CODE >= "30"
               WRITE MGR-MASTER-REC FROM EMP-MASTER-REC
               ADD 1 TO WS-OUT-COUNT
           END-IF.
           PERFORM READ-EMP-MASTER.

       READ-EMP-MASTER.
           READ EMP-MASTER
               AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ.
           IF NOT EOF-EMP-MASTER
               ADD 1 TO WS-IN-COUNT
           END-IF.

       FINALIZE-PROCESS.
           CLOSE EMP-MASTER
                 MGR-MASTER.
           DISPLAY "INPUT COUNT : " WS-IN-COUNT.
           DISPLAY "OUTPUT COUNT: " WS-OUT-COUNT.
