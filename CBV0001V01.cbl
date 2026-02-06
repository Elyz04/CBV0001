000000*****************************************************************
000000 IDENTIFICATION                  DIVISION.                            
000000 PROGRAM-ID.                     CBV0001V01.
000000*/-------------------------------------------------------------/*
000000*    SYSTEM NAME    :            INPUT VALIDATION SAMPLE
000000*    PROGRAM-ID     :            CBV0001V01
000000*    JOB NAME       :            CBV0001V01
000000*    INPUT          :            CONSOLE INPUT
000000*    OUTPUT         :            CONSOLE MESSAGE
000000*    AFFECT TABLE   :            N/A
000000*    CBV126836217   :            未入力チェック
000000*    CBV384920561   :            桁数チェック（1～5桁）
000000*    CBV902174683   :            数値チェック
000000*    CREATE DATE    :            2026/02/06
000000*    UPDATE DATE    :            2026/02/06
000000*    AUTHOR         :            Elyz04
000000*    PURPOSE        :            入力値チェック処理のサンプル実装
000000*/-------------------------------------------------------------/*
000000*    UPDATE         :
000000*        2026/02/06 : 初版作成
000000*/-------------------------------------------------------------/*
000000 DATA                            DIVISION.
000000 WORKING-STORAGE                 SECTION.
000000*/-------------------------------------------------------------/*
000000*  ホスト変数                                                    
000000*/-------------------------------------------------------------/*     
000000 01 WS-VARIABLES.
000000    03 WS-RAW-DATA               PIC X(200).
000000    03 WS-DATA                   PIC X(05).
000000    03 WS-TEMP                   PIC X(05).
000000    03 WS-NUM-VALUE              PIC 9(05).
000000    03 WS-LEN                    PIC 9(02).
000000*/-------------------------------------------------------------/*
000000*  定数定義                                                      
000000*/-------------------------------------------------------------/*     
000000 01 CST-VARIABLES.
000000    03 CST-LOOP-FLG              PIC X(01) VALUE 'N'.
000000    03 CST-MIN-LENGTH            PIC 9(02) VALUE 1.
000000    03 CST-MAX-LENGTH            PIC 9(02) VALUE 5.
000000    03 CST-RESULT-CODE           PIC 9(01) VALUE 0.
000000       88 CST-SUCCESS                      VALUE 0.
000000       88 CST-ERR-NUL                      VALUE 1.
000000       88 CST-ERR-LEN                      VALUE 2.
000000       88 CST-ERR-NUM                      VALUE 3.
000000*/-------------------------------------------------------------/*
000000 PROCEDURE                       DIVISION.
000000*/-------------------------------------------------------------/*
000000* MODULE   : CBV0001-CTL-MAIN
000000* OVERVIEW : メイン処理
000000*            入力受付
000000*            入力チェック処理呼出
000000*/-------------------------------------------------------------/*
000000 CBV0001-CTL-MAIN.
000000     PERFORM UNTIL CST-LOOP-FLG = 'Y'
000000         MOVE 'Y'                TO CST-LOOP-FLG
000000         MOVE 0                  TO CST-RESULT-CODE
000000         DISPLAY ' Input value (1 to 5 digits) : '
000000         WITH NO ADVANCING
000000         ACCEPT WS-RAW-DATA
000000         PERFORM CBV0001-CAS-NUL
000000         IF CST-LOOP-FLG = 'Y'
000000             PERFORM CBV0001-CAS-LEN
000000         END-IF
000000         IF CST-LOOP-FLG = 'Y'
000000             PERFORM CBV0001-CAS-NUM
000000         END-IF
000000     END-PERFORM.
000000     PERFORM CBV0001-DISP-RESULT.
000000     STOP RUN.
000000*/-------------------------------------------------------------/*
000000* MODULE   : CBV0001-INIT-WS
000000* OVERVIEW : ワーク変数初期化処理
000000*            作業領域初期化
000000*/-------------------------------------------------------------/*
000000 CBV0001-INIT-WS.
000000     INITIALIZE WS-VARIABLES.         
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000* MODULE   : CBV0001-CAS-NUL
000000* CASE-ID  : CBV126836217
000000* OVERVIEW : 未入力チェック処理
000000*            入力値空白判定
000000*/-------------------------------------------------------------/*
000000 CBV0001-CAS-NUL.
000000     IF FUNCTION TRIM(WS-RAW-DATA) = SPACES
000000         MOVE 'N'                TO CST-LOOP-FLG
000000         SET CST-ERR-NUL         TO TRUE
000000         PERFORM CBV0001-DISP-RESULT
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000* MODULE   : CBV0001-CAS-LEN
000000* CASE-ID  : CBV384920561
000000* OVERVIEW : 桁数チェック処理
000000*            入力値桁数判定（1～5桁）
000000*/-------------------------------------------------------------/*
000000 CBV0001-CAS-LEN.
000000     COMPUTE WS-LEN = FUNCTION LENGTH(
000000             FUNCTION TRIM(WS-RAW-DATA TRAILING)
000000                                     )
000000     IF WS-LEN < CST-MIN-LENGTH 
000000     OR WS-LEN > CST-MAX-LENGTH
000000         MOVE 'N'                TO CST-LOOP-FLG
000000         SET CST-ERR-LEN         TO TRUE
000000         PERFORM CBV0001-DISP-RESULT
000000     ELSE
000000         MOVE WS-RAW-DATA(1:CST-MAX-LENGTH)
000000                                 TO 
000000                            WS-DATA
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000* MODULE   : CBV0001-CAS-NUM
000000* CASE-ID  : CBV902174683
000000* OVERVIEW : 数値チェック処理
000000*            数値のみ入力判定
000000*/-------------------------------------------------------------/*
000000 CBV0001-CAS-NUM.
000000     MOVE WS-DATA                TO WS-TEMP
000000     INSPECT WS-TEMP
000000         REPLACING ALL '0' BY SPACE
000000                   ALL '1' BY SPACE
000000                   ALL '2' BY SPACE
000000                   ALL '3' BY SPACE
000000                   ALL '4' BY SPACE
000000                   ALL '5' BY SPACE
000000                   ALL '6' BY SPACE
000000                   ALL '7' BY SPACE
000000                   ALL '8' BY SPACE
000000                   ALL '9' BY SPACE
000000     IF FUNCTION TRIM(WS-TEMP) = SPACES
000000         MOVE WS-DATA            TO WS-NUM-VALUE
000000     ELSE
000000         MOVE 'N'                TO CST-LOOP-FLG
000000         SET CST-ERR-NUM         TO TRUE
000000         PERFORM CBV0001-DISP-RESULT
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000* MODULE   : CBV0001-DISP-RESULT
000000* OVERVIEW : エラーメッセージ表示処理
000000*            エラーコードに応じてメッセージを表示
000000*/-------------------------------------------------------------/*
000000 CBV0001-DISP-RESULT.
000000     EVALUATE TRUE
000000         WHEN CST-SUCCESS
000000             DISPLAY ' PASS VALIDATION '
000000         WHEN CST-ERR-NUL
000000             DISPLAY ' INPUT CANNOT BE BLANK '
000000         WHEN CST-ERR-LEN
000000             DISPLAY ' LENGTH MUST BE 1 TO 5 DIGITS '
000000         WHEN CST-ERR-NUM
000000             DISPLAY ' INPUT IS NOT NUMERIC '
000000         WHEN OTHER
000000             CONTINUE
000000     END-EVALUATE.
000000     EXIT.
000000***************************************************************** 
